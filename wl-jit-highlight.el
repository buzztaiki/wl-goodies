;;; wl-jit-highlight.el --- jit-lock highlighting for Wanderlust

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
;; This extension provides jit-lock highlighting for draft-buffer. When you
;; use this extension, you don't need type C-l at writing draft.

;;; (@* "Configuration")
;; Add followings to your `.wl' file.
;;
;;   (require 'wl-jit-highlight)
;;   (add-hook 'wl-draft-mode-hook 'wl-jit-highlight-draft-register)
;;   (define-key wl-draft-mode-map [remap font-lock-fontify-block] 'wl-jit-highlight-draft-buffer)
;;   (define-key wl-draft-mode-map [remap font-lock-fontify-buffer] 'wl-jit-highlight-draft-buffer)
;;   (define-key wl-draft-mode-map [remap wl-draft-highlight-and-recenter] 'recenter)

;;; (@* "TODO")
;; - replace wl-highlight-message to this packages function?
;; - rename wl-jit-highlight-draft-* to wl-jit-highlight-message-*?

;;; Code:

(defface wl-jit-highlight-mime-tag
  '((t :foreground "firebrick" :background "gray95" :box (:line-width 1 :color "dim gray")))
  "MIME-Tag face of draft buffer.")

(defun wl-jit-highlight-goto-next-invisible-change (bound)
  (goto-char (or (next-single-property-change
                  (point) 'invisible nil bound)
                 bound)))

(defun wl-jit-highlight-draft-region (start end)
  "Highlight between START and END.
But don't highlight invisible regions."
  (interactive "r")
  (let ((modified (buffer-modified-p)))
    (unwind-protect
	(save-excursion
	  (setq start
		(progn (goto-char start) (line-beginning-position)))
	  (setq end
		(progn (goto-char end) (forward-line) (point)))
	  (goto-char start)
	  (when (get-text-property (point) 'invisible)
	    (wl-jit-highlight-goto-next-invisible-change end))
	  (while (< (point) end)
	    (let ((sub-start (point))
		  (sub-end (wl-jit-highlight-goto-next-invisible-change end)))
	      (put-text-property sub-start sub-end 'face nil)
	      (wl-highlight-message
               sub-start sub-end t
               (not
                (save-excursion
                  (goto-char sub-start)
                  (re-search-forward 
                   (concat "^" (regexp-quote mail-header-separator) "$")
                   nil t))))
	      (goto-char sub-start)
	      (while (re-search-forward mime-edit-tag-regexp
					sub-end t)
		(put-text-property (match-beginning 0) (match-end 0)
				   'face 'wl-jit-highlight-mime-tag))
	      (wl-jit-highlight-goto-next-invisible-change end))))
      (set-buffer-modified-p modified))))

(defun wl-jit-highlight-draft-buffer ()
  (interactive)
  (wl-jit-highlight-draft-region (point-min) (point-max)))

(defun wl-jit-highlight-draft-register ()
  (jit-lock-register 'wl-jit-highlight-draft-region))

(provide 'wl-jit-highlight)
;;; wl-jit-highlight.el ends here
