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
;; - support template
;; - support refile
;; - use following format for config-alist?
;;   '(("hoge@hoge.com"
;;       (default . t)
;;       (folder-format "%" mailbox "@hogehoge")
;;       (config
;;        (wl-smtp-posting-server . "smtp.fuga.com"))
;;       (template
;;        nil)))

;;; Code:

(defvar wl-account-config-alist nil)

(defsubst wl-account-find-if (predicate seq)
  (elmo-map-until-success
   (lambda (x) (and (funcall predicate x) x))
   seq))

(defun wl-account-address (account)
  (nth 0 account))
(defun wl-account-properties (account)
  (nth 1 account))
(defun wl-account-config (account)
  (nth 2 account))

(defun wl-account-property (account key)
  (plist-get (nth 1 account) key))

(defun wl-account-from (account)
  (format "%s <%s>"
	  (or (wl-account-property account :name)
	      (user-full-name))
	  (wl-account-address account)))

(defun wl-account-from-field-value ()
  (cadr
   (std11-extract-address-components
    (car (wl-parse-addresses (std11-field-body "From"))))))

(defun wl-account-config-exec ()
  (let* ((account
	  (or (assoc (wl-account-from-field-value)
		     wl-account-config-alist)
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

(defun wl-account-default-account ()
  (wl-account-find-if
   (lambda (account)
     (wl-account-property account :default))
   wl-account-config-alist))

(defun wl-account-mail-setup ()
  (let ((account
	 (or
	  (wl-account-find-if
	   (lambda (account)
	     (let ((regexp (wl-account-property account :folder-regexp)))
	       (and regexp
		    (string-match regexp wl-draft-parent-folder))))
	   wl-account-config-alist)
	  (wl-account-default-account))))
    (when account
      (wl-draft-replace-field
       "From" (wl-account-from account)))))

(defun wl-account-user-mail-address-list ()
  (mapcar 'car wl-account-config-alist))

(defun wl-account-init ()
  (add-hook 'wl-draft-send-hook 'wl-account-exec-config)
  (add-hook 'wl-mail-setup-hook 'wl-account-mail-setup)
  (setq wl-user-mail-address-list
	(wl-account-user-mail-address-list)))

(provide 'wl-account)
;;; wl-account.el ends here
