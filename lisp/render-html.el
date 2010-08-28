;;; render-html.el --- Quoted-Printable functions

;; Copyright (C) 2010  Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: html, web

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

;; The idea is to provide a simple, fast and pretty minimal way to
;; render HTML (including links and images) in a buffer, based on an
;; external HTML renderer (i.e., w3m).

;;; Code:

(defun render-html-file (file)
  (erase-buffer)
  (let ((process (start-process "w3m" (current-buffer)
				"w3m" "-halfdump" file)))
    (set-process-sentinel process 'render-html-finished-rendering)))

(defun render-html-finished-rendering (process event)
  (when (string-match "finished" event)
    (save-excursion
      (set-buffer (process-buffer process))
      (render-html-wash-tags))))

(defun render-html-wash-tags ()
  (let (tag parameters string)
    ;;(subst-char-in-region (point-min) (point-max) ?_ ? )
    (goto-char (point-min))
    (while (re-search-forward "<\\([^ ]+\\)\\([^>]*\\)>\\([^<]*\\)<[^>]*>" nil t)
      (setq tag (match-string 1)
	    parameters (match-string 2)
	    string (match-string 3))
      (replace-match string))))

;;; render-html.el ends here
