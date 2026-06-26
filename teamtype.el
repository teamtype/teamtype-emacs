;;; teamtype.el --- Emacs module for TeamType collaborative editing  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cash, blinry

;; Author: Jamie Cash <jamie@occasionallycogent.com>, blinry <mail@blinry.org>
;; Package-Requires: ((emacs "30.1"))
;; Version: 1.0
;; Keywords: tools, data, comm

;; This file is not part of GNU Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs integration for Teamtype (https://teamtype.github.io/teamtype/introduction.html)

;;; Code:

(require 'jsonrpc)
(require 'browse-url)
(require 'eglot)
(require 'color)

(defgroup teamtype ()
  "Teamtype configuration options.")

(defcustom teamtype-client-command (list "teamtype" "client")
  "Command used to connect to teamtype daemon."
  :type '(repeat string)
  :group 'teamtype)

(defcustom teamtype-auto-connect 'ask
  "Automatically start the teamtype client when opening a file in a
  directory with a `.teamtype' directory.

If `ask', prompt the user before starting the client.
If `never', do not automatically start the client.
If `always', start the client automatically."
  :type '(choice (const :tag "Ask when opening a file" ask)
                 (const :tag "Never automatically connect" never)
                 (const :tag "Always automatically connect" always))
  :group 'teamtype)

(defgroup teamtype-faces ()
  "Faces used in teamtype"
  :group 'faces
  :group 'teamtype
  :prefix "teamtype-")

(defface teamtype-other-cursor-face
  `((((class color) (background dark))
     :inherit cursor
     :background ,(color-darken-name
                   (face-background 'cursor nil t)
                   50)
     :foreground "black")
    (((class color) (background light))
     :inherit cursor
     :background ,(color-lighten-name
                   (face-background 'cursor nil t)
                   50)
     :foreground "white")
    (t :inherit cursor))
  "Face for shared cursors."
  :group 'teamtype-faces)

(defface teamtype-other-user-name-face
  `((((class color) (background light))
     :inherit shadow
     :background ,(color-lighten-name
                   (face-background 'highlight nil t)
                   50))
    (((class color) (background dark))
     :inherit shadow
     :background ,(color-darken-name
                   (face-background 'highlight nil t)
                   50))
    (t :inherit shadow))
  "Face for usernames"
  :group 'teamtype-faces)

(defvar teamtype--daemon-connections nil
  "Reference to the current daemon connections, as an alist of
  `(directory refcount . connection)'.")
(defvar-local teamtype--daemon-connection nil
  "Buffer-local current daemon connection.")
(defvar-local teamtype--editor-revision 0
  "Editor revision in the current buffer.")
(defvar-local teamtype--daemon-revision 0
  "Daemon revision in the current buffer.")
(defvar teamtype--cursors nil
  "Associates user IDs with the cursor overlays.")

(defun teamtype--uri-to-path (uri)
  "Convert file:// uri from TeamType to file path."
  (let ((url (url-generic-parse-url uri)))
    (when (string= "file" (url-type url))
      (url-unhex-string (url-filename url)))))

(defvar-local teamtype--applying-server-edits nil)

(defun teamtype--clear-user-cursors (userid)
  (when-let* ((user-overlays (assoc-string userid teamtype--cursors)))
    (cl-map nil #'delete-overlay (cdr user-overlays))
    (setq teamtype--cursors (assoc-delete-all userid teamtype--cursors #'string=))))

(defun teamtype--range-region (range)
  (let ((eglot-move-to-linepos-function #'eglot-move-to-utf-32-linepos))
    (eglot-range-region range)))

(defun teamtype--notification-dispatcher (_conn method params)
  (when-let* ((edited-buffer (thread-first (plist-get params :uri)
                                           (teamtype--uri-to-path)
                                           (get-file-buffer))))
    (with-current-buffer edited-buffer
      (cl-case method
        (cursor
         (let ((user-id (plist-get params :userid))
               (user-name (concat
                           " "
                           (thread-first
                             (or (plist-get params :name) "👻")
                             (propertize 'face 'teamtype-other-user-name-face)))))
           (teamtype--clear-user-cursors user-id)
           (thread-last
             (plist-get params :ranges)
             (cl-mapcan
              (lambda (range)
                (pcase-let ((`(,beg . ,end) (teamtype--range-region range)))
                  (list
                   ;; create overlay for cursor
                   (let* ((end (if (= beg end) (+ end 1) end))
                          (overlay (make-overlay beg end)))
                     (overlay-put overlay
                                  'face 'teamtype-other-cursor-face)
                     overlay)
                   ;; create overlay for name at end-of-line
                   ;; XXX: this shows the name for each cursor for the user;
                   ;; kind of weird for, e.g. vim's block-selection,
                   ;; but we want this for true-multi-cursor. Perhaps
                   ;; sort by line and only show the name when there's
                   ;; a discontinuity?
                   (let* ((eol (save-excursion
                                 (goto-char beg)
                                 (end-of-line)
                                 (point)))
                          (overlay (make-overlay eol eol)))
                     (overlay-put overlay 'after-string user-name)
                     overlay)))))
             (cons user-id)
             ((lambda (overlays) (push overlays teamtype--cursors))))))
        (edit
         (if (= (plist-get params :revision) teamtype--editor-revision)
             (progn
               (setf teamtype--applying-server-edits t)
               (atomic-change-group
                 (let ((change-group (prepare-change-group))
                       (replacement (plist-get params :replacement)))
                   (cl-incf teamtype--daemon-revision)
                   (thread-last
                     (plist-get params :delta)
                     (reverse)
                     (mapcar
                      (lambda (edit)
                        (pcase-let ((`(,beg . ,end) (teamtype--range-region (plist-get edit :range)))
                                    (replacement (plist-get edit :replacement)))
                          `(,beg ,end . ,replacement))))
                     (mapc
                      (pcase-lambda (`(,beg ,end . ,replacement))
                        ;; TODO: could use Emacs <30 if we replace `replace-region-contents'
                        ;; with a fallback (see `eglot--apply-text-edits' for example)
                        (if (> emacs-major-version 30)
                            (replace-region-contents beg end replacement)
                          (replace-region-contents beg end (lambda () replacement))))))
                   (undo-amalgamate-change-group change-group)))
               (setf teamtype--applying-server-edits nil))
           (display-warning
            'teamtype
            (format-message
             "Got out-of-sync TeamType revision! Got %s, expected %s"
             (plist-get params :revision) teamtype--editor-revision)
            :debug)))))))

(defun teamtype--connect-to-daemon (directory)
  "Create a connection to the daemon in the current directory"
  (let ((conn (make-instance 'jsonrpc-process-connection
                             :name (concat "teamtype client" directory)
                             :process
                             (make-process
                              :name (concat "teamtype client" directory)
                              :command teamtype-client-command
                              :connection-type 'pipe
                              :coding 'utf-8-emacs-unix
                              :noquery t
                              :stderr (get-buffer-create
                                       (format "*teamtype %s stderr" directory))
                              :file-handler t)
                             :notification-dispatcher #'teamtype--notification-dispatcher
                             :on-shutdown (lambda (_conn)
                                            (setq teamtype--daemon-connections
                                                  (assoc-delete-all directory
                                                                    teamtype--daemon-connections
                                                                    #'string=))))))
    (progn
      (setq teamtype--daemon-connections
            ;; alist of project-dir, reference-count, connection object
            (cons `(,directory 1 . ,conn) teamtype--daemon-connections))
      conn)))

(defun teamtype--project-root-directory ()
  (thread-first
    (current-buffer)
    (buffer-file-name)
    (locate-dominating-file ".teamtype")))

(defun teamtype--current-buffer-uri ()
  (browse-url-file-url (buffer-file-name (current-buffer))))

(defun teamtype--get-daemon-connection ()
  "Get connection to the Teamtype daemon for the project directory
containing the current buffer. If one already exists in
`teamtype--daemon-connections', increment the reference count; other
create one. In either case, set the connection to the (buffer-local)
variable `teamtype--daemon-connection'."
  (thread-last
    (let ((dir (teamtype--project-root-directory)))
      (if-let* ((dir-count-conn (assoc-string dir teamtype--daemon-connections)))
          (progn
            (cl-incf (cadr dir-count-conn) 1)
            (cddr dir-count-conn))
        (teamtype--connect-to-daemon dir)))
    (setq teamtype--daemon-connection)))

(defun teamtype--disconnect-from-daemon ()
  (when teamtype--daemon-connection
    (jsonrpc-async-request
     teamtype--daemon-connection
     :close
     (list :uri (teamtype--current-buffer-uri)))
    (if-let* ((dir-count-conn (assoc-string
                               (teamtype--project-root-directory)
                               teamtype--daemon-connections)))
        (progn
          (cl-decf (cadr dir-count-conn) 1)
          (when (zerop (cadr dir-count-conn))
            (jsonrpc-shutdown teamtype--daemon-connection)))
      (warn "Couldn't find Teamtype connection!"))))

(defun teamtype--open-file ()
  (let ((file-uri (teamtype--current-buffer-uri))
        (content (buffer-substring-no-properties (point-min) (point-max))))
    (jsonrpc-async-request
     teamtype--daemon-connection
     :open
     (list :uri file-uri
           :content content))))

(defun teamtype--pos-to-teamtype-position (pos)
  (eglot--widening
   (list :line (1- (line-number-at-pos pos t))
         :character (progn (goto-char pos)
                           (eglot-utf-32-linepos)))))

(defvar-local teamtype--edit-start nil)
(defvar-local teamtype--edit-end nil)

(defun teamtype--before-change (start end)
  (unless teamtype--applying-server-edits
    (setq teamtype--edit-start (teamtype--pos-to-teamtype-position start)
          teamtype--edit-end (teamtype--pos-to-teamtype-position end))))

(defun teamtype--after-change (start end length)
  (unless teamtype--applying-server-edits
    (cl-incf teamtype--editor-revision)
    ;; TODO: debounce this?
    (let ((delta (list :range (list :start teamtype--edit-start
                                    :end teamtype--edit-end)
                       :replacement (buffer-substring-no-properties start end))))
      (jsonrpc-async-request
       teamtype--daemon-connection
       :edit
       (list :uri (teamtype--current-buffer-uri)
             :revision teamtype--daemon-revision
             :delta (vector delta))))))

(defvar-local teamtype--my-cursor-position '((0 . 0)))

(defun teamtype--current-cursor-positions ()
  ;; TODO: if evil + visual block mode do something else?
  ;; TODO: whatever multi-cursor packages?
  (if (use-region-p)
      (pcase-let ((`((,start . , end)) (region-bounds)))
        (list (cons start (1+ end))))
    (list (cons (point) (point)))))

(defun teamtype--post-command ()
  (let ((here (teamtype--current-cursor-positions)))
    (when (not (equal here teamtype--my-cursor-position))
      (setq teamtype--my-cursor-position here)
      ;; TODO: debounce/throttle this?
      (jsonrpc-async-request
       teamtype--daemon-connection
       :cursor
       (list :uri (teamtype--current-buffer-uri)
             :ranges (thread-last
                       (teamtype--current-cursor-positions)
                       (cl-map
                        'vector
                        (pcase-lambda (`(,start . ,end))
                          (list :start (teamtype--pos-to-teamtype-position start)
                                :end (teamtype--pos-to-teamtype-position end))))))))))

(defvar teamtype-client-mode) ; forward decl

(defun teamtype--supersession-threat-wrapper (f filename)
  "Wrapper used for `:around' advice to make `ask-user-about-supersession-threat' not worry about the file changing out from under us when being managed by TeamType."
  (if teamtype-client-mode
      t
    (funcall f filename)))

(defun teamtype--shutdown ()
  (teamtype-client-mode -1))

(defun teamtype-jump-to-cursor (user-id)
  "Jump to a peer's cursor."
  (interactive (list (completing-read
                      "User: "
                      (lambda (input predicate action)
                        (if (eq action 'metadata)
                            '(metadata
                              (category . teamtype-user)
                              (display-sort-function . identity)
                              (cycle-sort-function . identity))

                          (complete-with-action action
                                                (mapcar #'car teamtype--cursors)
                                                input
                                                predicate)))))))
                                                
(define-minor-mode teamtype-client-mode
  "Minor mode for editing a document that is being collaborated with via Teamtype.
Run when editing a file in a directory managed by the Teamtype daemon (i.e. the direction in which either `teamtype share` or' `teamtype join ...' has been run."
  :global nil
  (cond
   (teamtype-client-mode
    ;; TODO: change default-directory to be parent directory containing .teamtype directory
    ;; Disable buffer-local auto-revert
    (auto-revert-mode -1)
    ;; Make global-auto-revert-mode ignore this buffer
    (when (boundp 'inhibit-auto-revert-buffers)
      (add-to-list 'inhibit-auto-revert-buffers (current-buffer)))
    ;; Don't warn about buffer edits when file is changing out from under us
    (advice-add 'ask-user-about-supersession-threat :around #'teamtype--supersession-threat-wrapper)
    (setq teamtype--editor-revision 0)
    (setq teamtype--daemon-revision 0)
    (teamtype--get-daemon-connection)
    (teamtype--open-file)
    (add-hook 'before-change-functions #'teamtype--before-change nil t)
    (add-hook 'after-change-functions #'teamtype--after-change nil t)
    (add-hook 'post-command-hook #'teamtype--post-command nil t)
    (add-hook 'kill-buffer-hook #'teamtype--shutdown nil t))
   (t
    (advice-remove 'ask-user-about-supersession-threat
                   #'teamtype--supersession-threat-wrapper)
    (when (boundp 'inhibit-auto-revert-buffers)
      (setq inhibit-auto-revert-buffers
            (delq (current-buffer) inhibit-auto-revert-buffers)))
    (teamtype--disconnect-from-daemon)
    (remove-hook 'post-command-hook #'teamtype--post-command t)
    (remove-hook 'before-change-functions #'teamtype--before-change t)
    (remove-hook 'after-change-functions #'teamtype--after-change t)
    (remove-hook 'kill-buffer-hook #'teamtype--shutdown t))))

(defun teamtype--maybe-auto-start-connection ()
  "Check if the current file is in a teamtype-monitored directory and
  possibly automatically start the client connection, based on the
  value of `teamtype-auto-connect'."
  (unless (eq teamtype-auto-connect 'never)
    (when-let* ((tt-dir (locate-dominating-file (buffer-file-name (current-buffer))
                                                ;; TODO: check if .teamtype is a directory?
                                                ;; TODO: check if socket exists/daemon is running?
                                                ".teamtype")))
      (when (or (eq teamtype-auto-connect 'always)
                (y-or-n-p (concat "Connect to Teamtype daemon at " tt-dir ": ")))
        (teamtype-client-mode +1)))))

(add-hook 'find-file-hook #'teamtype--maybe-auto-start-connection)

(provide 'teamtype)
;;; teamtype.el ends here
