;;; rtree.el --- functions for manipulating range trees
;; Copyright (C) 2010 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; A "range tree" is a binary tree that stores ranges.  They are
;; similar to interval trees, but do not allow overlapping intervals.

;; A range is an ordered list of number intervals, like this:

;; ((10 . 25) 56 78 (98 . 201))

;; Common operations, like lookup, deletion and insertion are O(n) in
;; a range, but an rtree is O(log n) in all these operations.
;; Transformation between a range and an rtree is O(n).

;; The rtrees are quite simple.  The structure of each node is

;; (cons (cons low high) (cons left right))

;; That is, they are three cons cells, where the car of the top cell
;; is the actual range, and the cdr has the left and right child.  The
;; rtrees aren't automatically balanced, but are balanced when
;; created, and can be rebalanced when deemed necessary.

;;; Code:

(eval-when-compile
  (require 'cl))

(defmacro rtree-make-node ()
  `(list (list nil) nil))

(defmacro rtree-set-left (node left)
  `(setcar (cdr ,node) ,left))

(defmacro rtree-set-right (node right)
  `(setcdr (cdr ,node) ,right))

(defmacro rtree-set-range (node range)
  `(setcar ,node ,range))

(defmacro rtree-low (node)
  `(caar ,node))

(defmacro rtree-high (node)
  `(cdar ,node))

(defmacro rtree-left (node)
  `(cadr ,node))

(defmacro rtree-right (node)
  `(cddr ,node))

(defmacro rtree-range (node)
  `(car ,node))

(defsubst rtree-normalise-range (range)
  (when (numberp range)
    (setq range (cons range range)))
  range)

(defun rtree-make (range)
  "Make an rtree from RANGE."
  ;; Normalize the range.
  (unless (listp (cdr-safe range))
    (setq range (list range)))
  (rtree-make-1 (cons nil range) (length range)))

(defun rtree-make-1 (range length)
  (let ((mid (/ length 2))
	(node (rtree-make-node)))
    (when (> mid 0)
      (rtree-set-left node (rtree-make-1 range mid)))
    (rtree-set-range node (rtree-normalise-range (cadr range)))
    (setcdr range (cddr range))
    (when (> (- length mid 1) 0)
      (rtree-set-right node (rtree-make-1 range (- length mid 1))))
    node))

(defun rtree-memq (tree number)
  "Return non-nil if NUMBER is present in TREE."
  (while (and tree
	      (not (and (>= number (rtree-low tree))
			(<= number (rtree-high tree)))))
    (setq tree
	  (if (< number (rtree-low tree))
	      (rtree-left tree)
	    (rtree-right tree))))
  tree)

(defun rtree-extract (tree)
  "Convert TREE to range form."
  (let (stack result)
    (while (or stack
	       tree)
      (if tree
	  (progn
	    (push tree stack)
	    (setq tree (rtree-right tree)))
	(setq tree (pop stack))
	(push (if (= (rtree-low tree)
		     (rtree-high tree))
		  (rtree-low tree)
		(rtree-range tree))
	      result)
	(setq tree (rtree-left tree))))
    result))

(provide 'rtree)

;;; rtree.el ends here
