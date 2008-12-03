;;; wl-inline-forward.el --- Inline Forward Support for Warnderlust.

;; Copyright (C) 2008  Taiki SUGAWARA <buzz.taiki@gmail.com>

;; Author: Taiki SUGAWARA <buzz.taiki@gmail.com>
;; Keywords: mail

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
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
;; This extension provides inline style message forwarding (like a windows
;; MUA) to Wanderlust.

;;; (@* "Configuration")
;; First, add followings to your `.wl' file.
;;
;;   (require 'wl-inline-forward)
;;
;; If you want to use inline forward always, add followings.
;;
;;   (define-key wl-summary-mode-map "f" 'wl-inline-forward)
;;
;; Otherwise, if you want to select forwarding method every time, add
;; followings.
;;
;;   (define-key wl-summary-mode-map "f" 'wl-inline-forward-select-method)

;;; Code:
(require 'std11)
(require 'eword-decode)
(require 'wl-draft)
(require 'wl-summary)

(defvar wl-inline-forward-format-date-function 'identity
  "*Format date function for `wl-inline-forward'.
This variable calls following arguments:
  \(DATE-STRING)
DATE-STRING comes from Date header of forwarded mail.
You can use following function and varialbes for format date.
  `wl-inline-forward-format-date'
  `wl-inline-forward-date-format'
")

(defvar wl-inline-forward-date-format "%Y-%m-%d %T %z"
  "*Date format for `wl-inline-forward-format-date'")

(defvar wl-inline-forward-format-addresses-function
  'wl-inline-forward-format-addresses
  "*Format addresses function for `wl-inline-forward'.
This variable calls following arguments:
  \(ADDRESSES-STRING FIELD)
ADDRESSES-STRING is a value of FIELD
FIELD is a header field of forwarded mail.
You can use following functions for format addresses.
  `wl-inline-forward-format-addresses'
  `wl-inline-forward-format-addresses-as-name'
  `wl-inline-forward-format-addresses-as-address'
")

(defun wl-inline-forward-format-date (date-string)
  "Format DATE-STRING with `wl-inline-forward-date-format'."
  (format-time-string
   wl-inline-forward-date-format
   (elmo-time-parse-date-string date-string)))

(defun wl-inline-forward-extract-addresses (address-string)
  (mapcar
   (lambda (x)
     (list (std11-full-name-string x)
	   (std11-address-string x)))
   (std11-parse-addresses-string
    (std11-unfold-string address-string))))

(defun wl-inline-forward-format-addresses-as-name (addresses-string field)
  (wl-inline-forward-format-addresses
   (mapconcat (lambda (x) (or (car x) (cadr x)))
	      (wl-inline-forward-extract-addresses addresses-string)
	      ", ")
   field))

(defun wl-inline-forward-format-addresses-as-address (addresses-string field)
  (wl-inline-forward-format-addresses
   (mapconcat 'cadr
	      (wl-inline-forward-extract-addresses addresses-string)
	      ", ")
   field))

(defun wl-inline-forward-format-addresses (addresses-string field)
  (mime-decode-field-body addresses-string field 'wide))

(defun wl-inline-forward ()
  "forward message as inline."
  (interactive)
  (let ((summary-buf (current-buffer))
	(message (wl-summary-message-string 'maybe))
	(folder (wl-summary-buffer-folder-name))
	(number (wl-summary-message-number))
	field-alist forward-subject references
	content-type content-transfer-encoding
	body)
    (with-temp-buffer
      (set-buffer-multibyte t)
      (insert (string-to-multibyte message))
      (setq field-alist
	    (mapcar
	     (lambda (pair)
	       (let* ((field (car pair))
		      (func (cdr pair))
		      (value (std11-field-body field)))
		 (cons field (and value (funcall func value field)))))
	     `(("Subject" . (lambda (value field)
			      (mime-decode-field-body value field 'plain)))
	       ("From" . ,wl-inline-forward-format-addresses-function)
	       ("Date" . (lambda (value field)
			   (funcall wl-inline-forward-format-date-function value)))
	       ("To" . ,wl-inline-forward-format-addresses-function)
	       ("Cc" . ,wl-inline-forward-format-addresses-function))))
      (setq forward-subject (wl-draft-forward-make-subject (cdr (assoc "Subject" field-alist))))
      (setq references (nconc
			(std11-field-bodies '("References" "In-Reply-To"))
			(list (std11-field-body "Message-Id"))))
      (setq references (delq nil references)
	    references (mapconcat 'identity references " ")
	    references (wl-draft-parse-msg-id-list-string references)
	    references (wl-delete-duplicates references)
	    references (when references
			 (mapconcat 'identity references "\n\t")))
      (setq content-type (std11-field-body "Content-Type"))
      (setq content-transfer-encoding (std11-field-body "Content-Transfer-Encoding"))
      (goto-char (point-min))
      (or (re-search-forward "\n\n" nil t)
	  (search-forward (concat mail-header-separator "\n") nil t))
      (setq body (buffer-substring (point) (point-max))))
    ;; prepare draft buffer.
    (wl-draft (list (cons 'To "")
		    (cons 'Subject forward-subject)
		    (cons 'References references))
	      content-type content-transfer-encoding body t folder number)
    ;; remove unused tags.
    (wl-draft-body-goto-top)
    (save-excursion
      (let* ((ret (mime-edit-find-inmost)))
	(let ((bb (nth 1 ret))
	      (be (nth 2 ret))
	      (eb (nth 3 ret)))
	  (when eb
	    (goto-char eb)
	    (when (looking-at mime-edit-end-tag-regexp)
	      (delete-region (match-beginning 0) (match-end 0))))
	  (when (and bb be)
	    (delete-region bb be)))))
    (when (looking-at mime-edit-single-part-tag-regexp)
      (delete-region (match-beginning 0) (match-end 0)))
    ;; insert citation headers
    (insert "\n---------- Forwarded Message ----------\n")
    (mapc (lambda (pair)
	    (when (cdr pair)
	      (insert (car pair) ": " (cdr pair) "\n")))
	  field-alist)
    ;; finish!
    (mail-position-on-field "To")
    (setq wl-draft-config-variables
	  (append wl-draft-parent-variables
		  wl-draft-config-variables))
    (wl-draft-config-info-operation wl-draft-buffer-message-number 'save)
    (run-hooks 'wl-draft-forward-hook)
    (with-current-buffer summary-buf
      (run-hooks 'wl-summary-forward-hook))
    (run-hooks 'wl-mail-setup-hook)))

(defun wl-inline-forward-select-method ()
  "Select forward method."
  (interactive)
  (let ((c (read-char "Select Forward Method: R)fc822 I)nline")))
    (cond ((eq c ?r) (wl-summary-forward))
	  ((eq c ?w) (wl-inline-forward))
	  (t (message "Cancel")))))

(provide 'wl-inline-forward)
;;; wl-inline-forward.el ends here
