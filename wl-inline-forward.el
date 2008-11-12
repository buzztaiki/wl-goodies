;;; wl-inline-forward.el --- Inline Forward Add-On for Warnderlust.

;; Copyright (C) 2008  Taiki SUGAWARA <sugawara_t@ariel-networks.com>

;; Author: Taiki SUGAWARA <sugawara_t@ariel-networks.com>
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

;;; (@* "Configuration Examples")

;; Add followings to your `.wl' file.
;;
;;   (require 'wl-inline-forward)

;; If you want to use inline forward always, add followings.
;;
;;   (define-key wl-summary-mode-map "f" 'wl-inline-forward)

;; Otherwise, if you want to select forwarding method every time, add
;; followings.
;;
;;   (define-key wl-summary-mode-map "f" 'wl-inline-forward-select-method)

;;; Code:

(defun wl-inline-forward ()
  "forward message as inline."
  (interactive)
  (let ((summary-buf (current-buffer))
	(message (wl-summary-message-string 'maybe))
	(folder (wl-summary-buffer-folder-name))
	(number (wl-summary-message-number))
	(get-field-func
	 (lambda (name mode)
	   (let ((value (std11-field-body name)))
	     (or (and value (mime-decode-field-body value name mode))
		 ""))))
	subject forward-subject to cc from date references
	content-type content-transfer-encoding
	body)
    (with-temp-buffer
      (set-buffer-multibyte t)
      (insert (string-to-multibyte message))
      (setq subject (funcall get-field-func "Subject" 'plain))
      (setq forward-subject (wl-draft-forward-make-subject subject))
      (setq to (funcall get-field-func "To" 'wide))
      (setq cc (funcall get-field-func "Cc" 'wide))
      (setq from (funcall get-field-func "From" 'wide))
      (setq date (std11-field-body "Date"))
      (setq date (or (and date
			  (let ((datevec (timezone-parse-date date)))
			    (format "%s/%s/%s %s"
				    (aref datevec 0)
				    (aref datevec 1)
				    (aref datevec 2)
				    (aref datevec 3))))
		     ""))
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
    (insert "From: " from "\n")
    (insert "Subject: " subject "\n")
    (insert "Date: " date "\n")
    (insert "To: " to "\n")
    (insert "To: " cc "\n")
    ;; finish!
    (mail-position-on-field "To")
    (setq wl-draft-config-variables
	  (append wl-draft-parent-variables
		  wl-draft-config-variables))
    (wl-draft-config-info-operation wl-draft-buffer-message-number 'save)
    (run-hooks 'wl-draft-forward-hook)
    (with-current-buffer summary-buf (run-hooks 'wl-summary-forward-hook))
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
