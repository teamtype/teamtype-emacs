;;; teamtype.el --- Emacs module for TeamType collaborative editing  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jamie Cash, blinry

;; Author: Jamie Cash <jamie@occasionallycogent.com>, blinry <mail@blinry.org>
;; Package-Requires: ((emacs "25.2"))
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

(defcustom teamtype-client-command (list "teamtype" "client")
  "Command used to connect to teamtype daemon."
  :type '(repeat string))

(defvar-local teamtype--daemon-connection nil
  "Reference to the current daemon connection.")

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
                       :on-shutdown (lambda (_conn) (setq teamtype--daemon-connection nil)))))

(defun teamtype--disconnect-from-daemon ()
  (when teamtype--daemon-connection
    ;; TODO: send something to say we're going away?
    (jsonrpc-shutdown teamtype--daemon-connection)))

(defun teamtype--open-file (buffer)
  (let ((file-uri (browse-url-file-url (buffer-file-name buffer)))
        (content (with-current-buffer buffer
                   (buffer-substring-no-properties (point-min) (point-max)))))
    (jsonrpc-async-request
     teamtype--daemon-connection
     :open
     (list :uri file-uri
           :content content))))

(defvar teamtype-client-mode) ; forward decl
(define-minor-mode teamtype-client-mode
  "Minor mode for editing a document that is being collaborated with via Teamtype.
Run when editing a file in a directory managed by the Teamtype daemon (i.e. the direction in which either `teamtype share` or' `teamtype join ...' has been run."
  :global nil
  (cond
   (teamtype-client-mode
    ;; TODO: change default-directory to be parent directory containing .teamtype directory
    (teamtype--connect-to-daemon default-directory)
    (teamtype--open-file (current-buffer)))
   (t
    (teamtype--disconnect-from-daemon teamtype--daemon-connection))))

(provide 'teamtype)
;;; teamtype.el ends here
