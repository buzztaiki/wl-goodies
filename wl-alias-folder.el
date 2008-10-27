
;;; wl-alias-folder.el --- Alias Folder Support for Wanderlust.

;; Copyright (C) 2008  Taiki SUGAWARA <buzz.taiki@gmail.com>

;; Author: Taiki SUGAWARA <buzz.taiki@gmail.com>
;; Keywords: mail, net news

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; (@* "Configuration Examples")

;; If you want to use alias folder for gmail acconut, you run a following
;; steps.

;; 1. Add followings to your `.wl' file.
;;
;; (require 'wl-alias-folder)
;; (elmo-define-folder ?: 'alias)
;; (setq elmo-alias-folder-alist
;;       '(("gmail" imap4
;;          :user "YOUR GMAIL ACCONT"
;;          :auth clear
;;          :server "imap.gmail.com"
;;          :port 993
;;          :stream-type ssl)))
;;
;; 2. Add followings to you `.folders' file.
;;
;; :gmail:/
;;
;; 3. Restart Wanderlust

;;; (@* "TODO")

;;; Code:

;; ================================================================
;; (@* "ELMO Layer")
;; ================================================================

(require 'elmo)
(require 'elmo-msgdb)

(defvar elmo-alias-folder-alist nil)

(eval-when-compile
  (require 'find-func)
  (defmacro elmo-alias-define-delegate-method (name args)
    (let ((call (if (memq '&rest args) 'apply 'funcall)))
      `(luna-define-method ,name
	 ((folder elmo-alias-folder)
	  ,@args)
	 (,call ',name (elmo-alias-folder-target-internal folder)
		,@(elmo-delete-if (lambda (x) (memq x '(&optional &rest)))
				  (copy-list args))))))

  (defmacro elmo-alias-define-delegate-methods ()
    (let ((elmo-el (find-library-name "elmo")))
      (save-excursion
	(set-buffer (find-file-noselect elmo-el t))
	(goto-char (point-min))
	(let (all-forms)
	  (condition-case e
	      (while (not (eobp))
		(let ((form (read (current-buffer ))))
		  (when (eq (car form) 'luna-define-generic)
		    (let ((name (nth 1 form))
			  (args (cdr (nth 2 form))))
		      (push `(elmo-alias-define-delegate-method ,name ,args)
			    all-forms)))))
	    (end-of-file))
	  (cons 'progn all-forms))))))

(eval-and-compile
  (luna-define-class elmo-alias-folder (elmo-folder)
		     (alias-name target converter))
  (luna-define-internal-accessors 'elmo-alias-folder))

(elmo-alias-define-delegate-methods)

(luna-define-method elmo-folder-initialize
  ((folder elmo-alias-folder)
   name)
  (unless (string-match "^\\([^:]+\\)\\(:\\(.+\\)\\)*" name)
    (error "Folder syntax error `%s'" (elmo-folder-name-internal folder)))
  (let* ((alias-name (match-string 1 name))
	 (mailbox (match-string 3 name))
	 (spec (cdr (assoc alias-name elmo-alias-folder-alist)))
	 converter target)
    (unless spec
      (error "Cannot fond alias `%s' in `elmo-alias-folder-alist'" alias-name))
    (setq converter (luna-make-entity
		     (intern (format "elmo-alias-%s-converter" (car spec)))
		     :config (cdr spec)))
    (setq target (elmo-get-folder
		  (elmo-alias-convert-to-target converter mailbox)))
    (elmo-alias-folder-set-alias-name-internal folder alias-name)
    (elmo-alias-folder-set-converter-internal folder converter)
    (elmo-alias-folder-set-target-internal folder target)
    (elmo-alias-connect-signals
     folder (elmo-alias-folder-target-internal folder))
    folder))

(defun elmo-alias-connect-signals (folder target)
  (elmo-connect-signal
   target 'flag-changing folder
   (elmo-define-signal-handler (folder target number old-flags new-flags)
     (elmo-emit-signal 'flag-changing folder number old-flags new-flags)))
  (elmo-connect-signal
   target 'flag-changed folder
   (elmo-define-signal-handler (folder target numbers)
     (elmo-emit-signal 'flag-changed folder numbers)))
  (elmo-connect-signal
   target 'status-changed folder
   (elmo-define-signal-handler (folder target numbers)
     (elmo-emit-signal 'status-changed folder numbers)))
  (elmo-connect-signal
   target 'update-overview folder
   (elmo-define-signal-handler (folder target number)
     (elmo-emit-signal 'update-overview folder number))))

(luna-define-method elmo-folder-list-subfolders
  ((folder elmo-alias-folder)
   &optional one-level)
  (let* ((target (elmo-alias-folder-target-internal folder))
	 (alias-name (elmo-alias-folder-alias-name-internal folder))
	 (converter (elmo-alias-folder-converter-internal folder)))
    (elmo-mapcar-list-of-list
     (lambda (name)
       (concat (elmo-folder-prefix-internal folder)
	       alias-name ":"
	       (elmo-alias-convert-from-target converter name)))
     (elmo-folder-list-subfolders target one-level))))

(luna-define-method elmo-folder-have-subfolder-p
  ((folder elmo-alias-folder))
  (let ((target (elmo-alias-folder-target-internal folder)))
    (and
     (elmo-alias-mailbox-parser (elmo-folder-name-internal target))
     (elmo-folder-have-subfolder-p target))))

;; ----------------------------------------------------------------
;; (@* "Icon Support")
;; ----------------------------------------------------------------

(luna-define-generic elmo-folder-icon-type (folder)
  "Return an icon type of this FOLDER.")
(luna-define-method elmo-folder-icon-type ((folder elmo-alias-folder))
  (elmo-folder-type-internal (elmo-alias-folder-target-internal folder)))


;; ----------------------------------------------------------------
;; (@* "Converters")
;; ----------------------------------------------------------------

(luna-define-generic elmo-alias-convert-from-target (converter name)
  "convert from target folder NAME to mailbox.")
(luna-define-generic elmo-alias-convert-to-target (converter mailbox)
  "convert from MAILBOX to target folder name.")

(defun elmo-alias-folder-prefix (type)
  (let ((prefix (car (rassq type elmo-folder-type-alist))))
    (and prefix (char-to-string prefix))))

(defun elmo-alias-stream-type-spec (stream-type &optional sub-alist)
  (cdr (assq stream-type
	     (mapcar (lambda (x) (cons (cadr x) (car x)))
		     (append
		      sub-alist
		      elmo-network-stream-type-alist)))))


;; (@* "IMAP4 Converter")
(require 'elmo-imap4)
(eval-and-compile
  ;; CONFIG allows followings:
  ;;    - :user
  ;;    - :auth
  ;;    - :server
  ;;    - :port
  ;;    - :stream-type
  (luna-define-class elmo-alias-imap4-converter () (config))
  (luna-define-internal-accessors 'elmo-alias-imap4-converter))

(luna-define-method elmo-alias-convert-from-target
  ((converter elmo-alias-imap4-converter)
   name)
  (let ((tokens (car (elmo-parse-separated-tokens
		      name elmo-imap4-folder-name-syntax))))
    (substring (cdr (assq 'mailbox tokens)) 1)))

(luna-define-method elmo-alias-convert-to-target
  ((converter elmo-alias-imap4-converter)
   mailbox)
  (let* ((config (elmo-alias-imap4-converter-config-internal converter))
	 (prefix (elmo-alias-folder-prefix 'imap4))
	 (user (plist-get config :user))
	 (auth (plist-get config :auth))
	 (server (plist-get config :server))
	 (port (plist-get config :port))
	 (stream-type (plist-get config :stream-type))
	 stream-type-spec)
    (when (and auth (symbolp auth) )
      (setq auth (symbol-name auth)))
    (when (numberp port)
      (setq port (number-to-string port)))
    (when (stringp stream-type)
      (setq stream-type (intern stream-type)))
    (setq stream-type-spec (elmo-alias-stream-type-spec
			    stream-type elmo-imap4-stream-type-alist))
    (apply 'concat
	   prefix
	   mailbox
	   (append
	    (and user (append
		       (list ":" user)
		       (and auth (list "/" auth))))
	    (and server (list "@" server))
	    (and port (list ":" port))
	    (and stream-type-spec (list stream-type-spec))))))

;; (@* "Localdir Converter")
(require 'elmo-localdir)
(eval-and-compile
  ;; CONFIG allows followings:
  ;;    - :path
  (luna-define-class elmo-alias-localdir-converter () (config))
  (luna-define-internal-accessors 'elmo-alias-localdir-converter))

(luna-define-method elmo-alias-convert-from-target
  ((converter elmo-alias-localdir-converter)
   name)
  (let* ((config (elmo-alias-localdir-converter-config-internal converter))
	 (target-path (substring name 1))
	 (path (plist-get config :path)))
    (if path
	(let ((relative
	       (file-relative-name
		(expand-file-name target-path)
		(and path (expand-file-name path)))))
	  (if (string= relative ".") "" relative))
      target-path)))

(luna-define-method elmo-alias-convert-to-target
  ((converter elmo-alias-localdir-converter)
   mailbox)
  (let* ((config (elmo-alias-localdir-converter-config-internal converter))
	 (prefix (elmo-alias-folder-prefix 'localdir))
	 (path (plist-get config :path)))
    (concat
     prefix
     (and path (expand-file-name (or mailbox "") path)))))

;; (@* "NNTP Converter")
(require 'elmo-nntp)
(eval-and-compile
  ;; CONFIG allows followings:
  ;;    - :user
  ;;    - :server
  ;;    - :port
  ;;    - :stream-type
  (luna-define-class elmo-alias-nntp-converter () (config))
  (luna-define-internal-accessors 'elmo-alias-nntp-converter))

(luna-define-method elmo-alias-convert-from-target
  ((converter elmo-alias-nntp-converter)
   name)
  (let ((tokens (car (elmo-parse-separated-tokens
		      name elmo-nntp-folder-name-syntax))))
    (substring (cdr (assq 'group tokens)) 1)))

(luna-define-method elmo-alias-convert-to-target
  ((converter elmo-alias-nntp-converter)
   mailbox)
  (let* ((config (elmo-alias-nntp-converter-config-internal converter))
	 (prefix (elmo-alias-folder-prefix 'nntp))
	 (user (plist-get config :user))
	 (server (plist-get config :server))
	 (port (plist-get config :port))
	 (stream-type (plist-get config :stream-type))
	 stream-type-spec)
    (when (numberp port)
      (setq port (number-to-string port)))
    (when (stringp stream-type)
      (setq stream-type (intern stream-type)))
    (setq stream-type-spec (elmo-alias-stream-type-spec
			    stream-type elmo-nntp-stream-type-alist))
    (apply 'concat
	   prefix
	   mailbox
	   (append
	    (and user (list ":" user))
	    (and server (list "@" server))
	    (and port (list ":" port))
	    (and stream-type-spec (list stream-type-spec))))))

(provide 'elmo-alias)

;; ================================================================
;; (@* "WL Layer")
;; ================================================================

(defadvice wl-highlight-folder-current-line (after add-alias-icon activate)
  (unless (wl-folder-buffer-group-p)
    (let ((overlay (find-if (lambda (x) (overlay-get x 'wl-e21-icon))
			    (overlays-in (line-beginning-position)
					 (line-end-position))))
	  (entity (wl-folder-get-entity-from-buffer)))
      (when (and overlay entity)
	(let* ((elmo-folder (elmo-make-folder entity))
	       (icon-type (elmo-folder-icon-type elmo-folder))
	       image)
	  (when icon-type
	    (setq image (get (intern (format "wl-folder-%s-image" icon-type))
			     'image))
	    (overlay-put overlay 'before-string
			 (propertize " " 'display image 'invisible t))))))))

(provide 'wl-alias-folder)
;;; wl-alias-folder.el ends here
