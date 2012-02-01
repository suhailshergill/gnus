;;; gnus-compat.el --- Compatability functions for Gnus

;; Copyright (C) 2012 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: compat

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

;; This package defines and redefines a bunch of functions for Gnus
;; usage.  The basic (and somewhat unsound) idea is to make all
;; Emacsen look like the current trunk of Emacs.  So it will define
;; functions "missing" in other Emacs instances, and redefine other
;; functions to work like the Emacs trunk versions.

(provide 'gnus-compat)

(when (and (not (fboundp 'help-function-arglist))
	   (fboundp 'function-arglist))
  (defun help-function-arglist (def &optional preserve-names)
    (cdr (car (read-from-string (downcase (function-arglist def)))))))

;; gnus-compat.el ends here
