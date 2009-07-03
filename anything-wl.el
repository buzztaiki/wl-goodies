;;; anything-wl.el --- Anything Interface for Wanderlust

;; Copyright (C) 2009  Taiki SUGAWARA <buzz.taiki@gmail.com>

;; Author: Taiki SUGAWARA <buzz.taiki@gmail.com>
;; Keywords: wl, mail, anything

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

;;; Code:
(require 'anything)
(require 'wl-folder)
(require 'wl-summary)


(defvar anything-wl-ignore-folder-regexp-list nil)

(defvar anything-c-wl-folder-source
  '((name . "Folders")
    (candidates . anything-wl-folder-candidates)
    (action ("GoTo Folder" . anything-wl-goto-folder))))

(defun anything-wl-folder-candidates ()
  (let (folders)
    (mapatoms
     (lambda (x)
       (let ((name (symbol-name x)))
	 (unless (wl-string-match-member
		  name anything-wl-ignore-folder-regexp-list)
	   (push name folders))))
     wl-folder-entity-hashtb)
    (sort folders 'string<)))

(defun anything-wl-goto-folder (folder)
  (cond
   ((eq major-mode 'wl-folder-mode)
    (wl-folder-goto-folder-subr folder))
   ((eq major-mode 'wl-summary-mode)
    (wl-summary-goto-folder-subr
     folder
     (wl-summary-get-sync-range (wl-folder-get-elmo-folder folder))
     nil nil t))
   (t (error "current buffer is not wl"))))
    
(defun anything-wl-select-folder ()
  (interactive)
  (anything '(anything-c-wl-folder-source)))


(provide 'anything-wl)
;;; anything-wl.el ends here
