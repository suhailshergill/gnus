;;; shr-color.el --- Simple HTML Renderer color management

;; Copyright (C) 2010 Free Software Foundation, Inc.

;; Author: Julien Danjou <julien@danjou.info>
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

;; This package handles colors display for shr.

;;; Code:

(require 'color-lab)

(defgroup shr-color nil
  "Simple HTML Renderer colors"
  :group 'shr)

(defcustom shr-color-visible-luminance-min 40
  "Minimum luminance distance between two colors to be considered visible.
Must be between 0 and 100."
  :group 'shr
  :type 'float)

(defcustom shr-color-visible-distance-min 5
  "Minimum color distance between two colors to be considered visible.
This value is used to compare result for `ciede2000'. Its an
absolute value without any unit."
  :group 'shr
  :type 'integer)

(defun set-minimum-interval (val1 val2 min max interval &optional fixed)
  "Set minimum interval between VAL1 and VAL2 to INTERVAL.
The values are bound by MIN and MAX.
If FIXED is t, then val1 will not be touched."
  (let ((diff (abs (- val1 val2))))
    (unless (>= diff interval)
      (if fixed
          (let* ((missing (- interval diff))
                 ;; If val2 > val1, try to increase val2
                 ;; That's the "good direction"
                 (val2-good-direction
                  (if (> val2 val1)
                      (min max (+ val2 missing))
                    (max min (- val2 missing))))
                 (diff-val2-good-direction-val1 (abs (- val2-good-direction val1))))
            (if (>= diff-val2-good-direction-val1 interval)
                (setq val2 val2-good-direction)
              ;; Good-direction is not so good, compute bad-direction
              (let* ((val2-bad-direction
                      (if (> val2 val1)
                          (max min (- val1 interval))
                        (min max (+ val1 interval))))
                     (diff-val2-bad-direction-val1 (abs (- val2-bad-direction val1))))
                (if (>= diff-val2-bad-direction-val1 interval)
                    (setq val2 val2-bad-direction)
                  ;; Still not good, pick the best and prefer good direction
                  (setq val2
                        (if (>= diff-val2-good-direction-val1 diff-val2-bad-direction-val1)
                            val2-good-direction
                          val2-bad-direction))))))
        ;; No fixed, move val1 and val2
        (let ((missing (/ (- interval diff) 2.0)))
          (if (< val1 val2)
              (setq val1 (max min (- val1 missing))
                    val2 (min max (+ val2 missing)))
            (setq val2 (max min (- val2 missing))
                  val1 (min max (+ val1 missing))))
          (setq diff (abs (- val1 val2)))   ; Recompute diff
          (unless (>= diff interval)
            ;; Not ok, we hit a boundary
            (let ((missing (- interval diff)))
              (cond ((= val1 min)
                     (setq val2 (+ val2 missing)))
                    ((= val2 min)
                     (setq val1 (+ val1 missing)))
                    ((= val1 max)
                     (setq val2 (- val2 missing)))
                    ((= val2 max)
                     (setq val1 (- val1 missing)))))))))
    (list val1 val2)))

(defun shr-color-visible (bg fg &optional fixed-background)
  "Check that BG and FG colors are visible if they are drawn on each other.
Return t if they are. If they are too similar, two new colors are
returned instead.
If FIXED-BACKGROUND is set, and if the color are not visible, a
new background color will not be computed. Only the foreground
color will be adapted to be visible on BG."
  ;; Convert fg and bg to CIE Lab
  (let* ((fg-lab (apply 'rgb->lab (rgb->normalize fg)))
         (bg-lab (apply 'rgb->lab (rgb->normalize bg)))
         ;; Compute color distance using CIE DE 2000
         (fg-bg-distance (color-lab-ciede2000 fg-lab bg-lab))
         ;; Compute luminance distance (substract L component)
         (luminance-distance (abs (- (car fg-lab) (car bg-lab)))))
    (if (and (>= fg-bg-distance shr-color-visible-distance-min)
             (>= luminance-distance shr-color-visible-luminance-min))
        (list bg fg)
      ;; Not visible, try to change luminance to make them visible
      (let ((Ls (set-minimum-interval (car bg-lab) (car fg-lab) 0 100
                                      shr-color-visible-luminance-min
                                      fixed-background)))
        (setcar bg-lab (car Ls))
        (setcar fg-lab (cadr Ls))
        (list
         (apply 'format "#%02x%02x%02x"
                (mapcar (lambda (x) (* (max (min 1 x) 0) 255)) (apply 'lab->rgb bg-lab)))
         (apply 'format "#%02x%02x%02x"
                (mapcar (lambda (x) (* (max (min 1 x) 0) 255)) (apply 'lab->rgb fg-lab))))))))

(provide 'shr-color)

;;; shr-color.el ends here
