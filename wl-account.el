;;; wl-account.el --- 

;; Copyright (C) 2009  Taiki SUGAWARA <buzz.taiki@gmail.com>

;; Author: Taiki SUGAWARA <buzz.taiki@gmail.com>
;; Keywords: mail

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Todo
;; - support refile

;;; Code:

(require 'wl-vars)
(require 'wl-draft)
(require 'wl-template)

(defvar wl-account-config-alist nil
  "\((ADDRESS . ACCOUNT-CONFIG) ...))

ACCOUNT-CONFIG:
\((default . DEFAULT-VALUE)
  (config . CONFIG-SPEC)
  (template . CONFIG-SPEC)
  (folder-format . FOLDER-FORMAT-SPEC))

DEFAULT-VALUE:
t or nil

CONFIG-SPEC:
same as value part of `wl-draft-config-alist', `wl-template-alist'.

FOLDER-FORMAT-SPEC:
list of string or `mailbox' symbol.
if `mailbox' is appeared, this symbol is replace to mailbox string such as \"INBOX\".
"
)

;;; utilities
(defsubst wl-account-find-if (predicate seq)
  (elmo-map-until-success
   (lambda (x) (and (funcall predicate x) x))
   seq))

;;; accessors
(defun wl-account-address (account)
  (car account))
(defun wl-account-value (account name)
  (cdr (assq name (cdr account))))

(defmacro wl-account-define-accessor (name)
  `(defun ,(intern (concat "wl-account-" (symbol-name name)))
     (account)
     (wl-account-value account ',name)))

(wl-account-define-accessor config)
(wl-account-define-accessor template)
(wl-account-define-accessor folder-format)
(wl-account-define-accessor default)
(wl-account-define-accessor user-name)

(defalias 'wl-account-default-p 'wl-account-default)

(defun wl-account-folder-regexp-1 (folder-format)
  (format "^%s$"
	  (mapconcat
	   (lambda (x)
	     (cond ((stringp x)
		    (regexp-quote x))
		   ((eq x 'mailbox)
		    ".+")
		   (t nil)))
	   folder-format "")))

(defun wl-account-folder-regexp (account)
  (wl-account-folder-regexp-1
   (wl-account-folder-format account)))

(defun wl-account-from (account)
  (format "%s <%s>"
	  (or (wl-account-user-name account)
	      (user-full-name))
	  (wl-account-address account)))

;;; account finders
(defun wl-account-default-account ()
  (wl-account-find-if
   'wl-account-default-p
   wl-account-config-alist))

(defun wl-account-folder-account (folder)
  (when folder
    (wl-account-find-if
     (lambda (account)
       (let ((regexp (wl-account-folder-regexp account)))
	 (and regexp
	      (string-match regexp folder))))
     wl-account-config-alist)))

(defun wl-account-address-account (address)
  (assoc address wl-account-config-alist))

;;; draft config
(defun wl-account-from-field-address ()
  (cadr
   (std11-extract-address-components
    (car (wl-parse-addresses (std11-field-body "From"))))))

(defun wl-account-config-exec ()
  (let* ((account
	  (or (wl-account-address-account (wl-account-from-field-address))
	      (wl-account-default-account)))
	 (exec-flag wl-draft-config-exec-flag)
	 (wl-draft-config-exec-flag t))
    (unwind-protect
	(when account
	  (wl-draft-config-exec
	   (list
	    (cons t
		  (append
		   (list (cons 'wl-from (wl-account-from account))
			 (cons 'wl-envelope-from (wl-account-address account)))
		   (wl-account-config account))))))
      (setq wl-draft-config-exec-flag exec-flag))))

;;; initializers
(defun wl-account-mail-setup ()
  (let ((account
	 (or (wl-account-folder-account wl-draft-parent-folder)
	     (wl-account-default-account))))
    (wl-template-insert (wl-account-address account))))

(defun wl-account-template-init ()
  (dolist (account (reverse
		    (cons (wl-account-default-account)
			  wl-account-config-alist)))
    (set-alist 'wl-template-alist
	       (wl-account-address account)
	       (append
		(list
		 (cons "From" (wl-account-from account)))
		(wl-account-template account)))))

(defun wl-account-user-mail-address-list ()
  (mapcar 'wl-account-address wl-account-config-alist))

(defun wl-account-init (&optional alist)
  (when alist
    (setq wl-account-config-alist alist))
  (add-hook 'wl-draft-send-hook 'wl-account-config-exec)
  (add-hook 'wl-mail-setup-hook 'wl-account-mail-setup)
  (setq wl-user-mail-address-list
	(wl-account-user-mail-address-list))
  (wl-account-template-init))

(provide 'wl-account)
;;; wl-account.el ends here
