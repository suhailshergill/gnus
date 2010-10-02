;;; shr.el --- Simple HTML Renderer

;; Copyright (C) 2010 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: html

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package takes a HTML parse tree (as provided by
;; libxml-parse-html-region) and renders it in the current buffer.  It
;; does not do CSS, JavaScript or anything advanced: It's geared
;; towards rendering typical short snippets of HTML, like what you'd
;; find in HTML email and the like.

;;; Code:

(defvar shr-folding-mode nil)

(defvar shr-width 70)

(defun shr-transform-dom (dom)
  (let ((result (list (pop dom))))
    (dolist (arg (pop dom))
      (push (cons (intern (concat ":" (symbol-name (car arg))) obarray)
		  (cdr arg))
	    result))
    (dolist (sub dom)
      (if (stringp sub)
	  (push (cons :text sub) result)
	(push (shr-transform-dom sub) result)))
    (nreverse result)))

(defun shr-insert-document (dom)
  (setq dom (shr-transform-dom dom))
  (shr-descend dom))

(defun shr-descend (dom)
  (let ((function (intern (concat "shr-" (symbol-name (car dom))) obarray)))
    (if (fboundp function)
	(funcall function (cdr dom))
      (shr-generic (cdr dom)))))

(defun shr-generic (cont)
  (dolist (sub cont)
    (cond
     ((eq (car sub) :text)
      (shr-insert (cdr sub)))
     ((consp (cdr sub))
      (shr-descend sub)))))

(defun shr-p (cont)
  (shr-ensure-newline)
  (insert "\n")
  (shr-generic cont)
  (insert "\n"))

(defun shr-b (cont)
  (shr-fontize-cont cont 'bold))

(defun shr-i (cont)
  (shr-fontize-cont cont 'italic))

(defun shr-u (cont)
  (shr-fontize-cont cont 'underline))

(defun shr-s (cont)
  (shr-fontize-cont cont 'strikethru))

(defun shr-fontize-cont (cont type)
  (let ((start (point)))
    (shr-generic cont)
    (shr-add-font start (point) type)))

(defun shr-add-font (start end type)
  (put-text-property start end 'face type))

(defun shr-img (cont)
  (shr-ensure-newline)
  (let ((start (point-marker)))
    (let ((alt (or (cdr (assq :alt cont)) "[img]"))
	  (url (cdr (assq :src cont))))
      (if (url-is-cached url)
	  (shr-put-image (shr-get-image-data url) (point) alt)
	(insert alt)
	(url-retrieve url 'shr-image-fetched
		      (list (current-buffer) start (point-marker)))))
    (shr-ensure-newline)))

(defun shr-image-fetched (status buffer start end)
  (when (and (buffer-name buffer)
	     (not (plist-get status :error)))
    (url-store-in-cache (current-buffer))
    (when (or (search-forward "\n\n" nil t)
	      (search-forward "\r\n\r\n" nil t))
      (let ((data (buffer-substring (point) (point-max))))
        (with-current-buffer buffer
          (let ((alt (buffer-substring start end))
		(inhibit-read-only t))
	    (delete-region start end)
	    (shr-put-image data start alt))))))
  (kill-buffer (current-buffer)))

(defun shr-put-image (data point alt)
  (if (not (display-graphic-p))
      (insert alt)
    (let ((image (create-image data nil t)))
      (put-image image point alt))))

(defun shr-pre (cont)
  (let ((shr-folding-mode nil))
    (shr-ensure-newline)
    (shr-generic cont)
    (shr-ensure-newline)))

(defun shr-blockquote (cont)
  (shr-pre cont))

(defun shr-ensure-newline ()
  (unless (zerop (current-column))
    (insert "\n")))

(defun shr-insert (text)
  (cond
   ((eq shr-folding-mode 'none)
    (insert t))
   (t
    (let (column)
      (dolist (elem (split-string text))
	(setq column (current-column))
	(if (zerop column)
	    (insert elem)
	  (if (> (+ column (length elem) 1) shr-width)
	      (insert "\n" elem)
	    (insert " " elem))))))))

(defun shr-get-image-data (url)
  "Get image data for URL.
Return a string with image data."
  (with-temp-buffer
    (mm-disable-multibyte)
    (url-cache-extract (url-cache-create-filename url))
    (when (or (search-forward "\n\n" nil t)
              (search-forward "\r\n\r\n" nil t))
      (buffer-substring (point) (point-max)))))

(provide 'shr)

;;; shr.el ends here
