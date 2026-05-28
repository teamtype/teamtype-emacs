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

(defcustom teamtype-client-command (list "teamtype" "client")
  "Command used to connect to teamtype daemon."
  :type '(repeat string))

(defvar-local teamtype--daemon-connection nil
  "Reference to the current daemon connection.")
(defvar-local teamtype--editor-revision 0
  "Editor revision in the current buffer.")
(defvar-local teamtype--daemon-revision 0
  "Daemon revision in the current buffer.")

(defun teamtype--uri-to-path (uri)
  "Convert file:// uri from TeamType to file path."
  (let ((url (url-generic-parse-url uri)))
    (when (string= "file" (url-type url))
      (url-unhex-string (url-filename url)))))

(defvar-local teamtype--applying-server-edits nil)

(defun teamtype--notification-dispatcher (_conn method params)
  (cl-case method
    (cursor nil)
    (edit
     (let ((edited-buffer (thread-first (plist-get params :uri)
                                        (teamtype--uri-to-path)
                                        (get-file-buffer))))
       (with-current-buffer edited-buffer
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
                        (pcase-let ((`(,beg . ,end) (eglot-range-region (plist-get edit :range)))
                                    (replacement (plist-get edit :replacement)))
                          `(,beg ,end . ,replacement))))
                     (mapc
                      (pcase-lambda (`(,beg ,end . ,replacement))
                        ;; TODO: could use Emacs <30 if we replace `replace-region-contents'
                        ;; with a fallback (see `eglot--apply-text-edits' for example)
                        (if (> emacs-major-version 30)
                            (replace-region-contents beg end replacement)
                          (replace-region-contents beg end (lambda () replacement))))))
                   (undo-amalgamate-change-group change-group))))
           (warn "Got out-of-sync TeamType revision! Got %s, expected %s"
                 (plist-get params :revision) teamtype--editor-revision)))))))

(defun teamtype--connect-to-daemon (directory)
  "Create a connection to the daemon in the current directory"
  (setq teamtype--daemon-connection
        (make-instance 'jsonrpc-process-connection
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
                       ;; Removing for demo
                       :notification-dispatcher #'teamtype--notification-dispatcher
                       :on-shutdown (lambda (_conn) (setq teamtype--daemon-connection nil)))))

(defun teamtype--disconnect-from-daemon ()
  (when teamtype--daemon-connection
    ;; TODO: send something to say we're going away?
    (jsonrpc-shutdown teamtype--daemon-connection)))

(defun teamtype--current-buffer-uri ()
  (browse-url-file-url (buffer-file-name (current-buffer))))

(defun teamtype--open-file (buffer)
  (let ((file-uri (browse-url-file-url (buffer-file-name buffer)))
        (content (with-current-buffer buffer
                   (buffer-substring-no-properties (point-min) (point-max)))))
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

(defun teamtype--after-change (start end length)
  (unless teamtype--applying-server-edits
    (cl-incf teamtype--editor-revision)
    ;; TODO: debounce this?
    (let ((delta (list :range (list :start (teamtype--pos-to-teamtype-position start)
                                    :end (teamtype--pos-to-teamtype-position (+ start length)))
                       :replacement (buffer-substring-no-properties start end))))
      (jsonrpc-async-request
       teamtype--daemon-connection
       :edit
       (list :uri (teamtype--current-buffer-uri)
             :revision teamtype--daemon-revision
             :delta (vector delta))))))

(defvar teamtype-client-mode) ; forward decl
(define-minor-mode teamtype-client-mode
  "Minor mode for editing a document that is being collaborated with via Teamtype.
Run when editing a file in a directory managed by the Teamtype daemon (i.e. the direction in which either `teamtype share` or' `teamtype join ...' has been run."
  :global nil
  (cond
   (teamtype-client-mode
    ;; TODO: change default-directory to be parent directory containing .teamtype directory
    ;; TODO: turn off auto-revert-mode if enabled for this buffer?
    (setq teamtype--editor-revision 0)
    (setq teamtype--daemon-revision 0)
    (teamtype--connect-to-daemon default-directory)
    (teamtype--open-file (current-buffer))
    ;; TODO: send :close message after buffer discarded
    (add-hook 'after-change-functions #'teamtype--after-change nil t))
   (t
    (teamtype--disconnect-from-daemon)
    (remove-hook 'after-change-functions #'teamtype--after-change))))

(provide 'teamtype)
;;; teamtype.el ends here
