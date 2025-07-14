;;; 032-after-load-functions.el --- Define functions for deferred loading -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Free Software Foundation, Inc.
;;
;; Author: Luis Henriquez-Perez <luis@luishp.xyz>
;; Homepage: https://github.com/Luis-Henriquez-Perez/dotfiles/
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Define functions for deferred loading
;;
;;; Code:
(require 'subr-x)
(require '001-init-log)
(require '030-base-functions)
(eval-when-compile (require '031-anaphoric-macros))
(eval-when-compile (require '031-autolet-macros))
(eval-when-compile (require '031-looping-macros))

(defvar oo-after-bound-forms (make-hash-table :size 100)
  "A hash table whose elements are (ITEM . FORMS).
ITEM is a variable symbol.  FORMS are alist of lisp forms.")

(defun oo-eval-after-bound-forms (&rest _)
  "Evaluate list of forms that need to be."
  (dolist (key (hash-table-keys oo-after-bound-forms))
    (when (and (symbolp key) (boundp key))
      (funcall `(lambda () ,@(nreverse (gethash key oo-after-bound-forms))))
      (remhash key oo-after-bound-forms))))

(defun oo-call-after-bound (symbol fn)
  "Call FN after SYMBOL is bound.
If SYMBOL is already bound FN is called immediately."
  (if (boundp symbol)
      (funcall fn)
    (push `(ignore-errors (funcall ',fn)) (gethash symbol oo-after-bound-forms))))

;; Do not want to just.
(defmacro afterbound! (symbol &rest body)
  "Eval BODY after SYMBOL is bound."
  (declare (indent 1))
  `(if (boundp ',symbol)
       (progn ,@body)
     (push '(ignore-errors ,@body) (gethash symbol oo-after-bound-forms))))

(defmacro afterfeature! (feature &rest body)
  "Eval BODY after FEATURE is loaded."
  (declare (indent 1))
  `(if (featurep ',feature)
       (progn ,@body)
     (push '(ignore-errors ,@body) (gethash symbol oo-after-load-forms))))

(defvar oo-after-load-forms (make-hash-table :size 100)
  "A hash table whose elements are (FEATURE . FORMS).
FEATURE is a feature symbol.  FORMS are alist of lisp forms to be evaluated
after FEATURE is loaded.")

(defun oo-call-after-load (feature fn)
  "Call FN after FEATURE is loaded."
  (if (featurep feature)
      (funcall fn)
    (push `(ignore-errors (funcall ',fn)) (gethash feature oo-after-load-forms))
    (eval-after-load feature
      `(awhen! (gethash ',feature oo-after-load-forms)
         (eval (cons 'progn (nreverse it)))
         (remhash ',feature oo-after-load-forms)))))
;;; provide
(provide '032-after-load-functions)
;;; 032-after-load-functions.el ends here
