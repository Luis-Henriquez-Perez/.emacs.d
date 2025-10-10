;;; 000-anaphoric-macros.el --- Anaphoric macros -*- lexical-binding: t; -*-
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
;; These are basic anaphoric macros loosely based on the `anaphora' package.
;;
;;; Code:
(defmacro alet! (form &rest body)
  "Bind the result FORM to `it' for the duration of BODY."
  (declare (debug let) (indent 1))
  `(let ((it ,form))
     ,@body))

(defmacro aand! (&rest conditions)
  "Like `and' but bind the result of first condition to `it'."
  `(alet! ,(car conditions)
     (and it ,@(cdr conditions))))

(defmacro and! (&rest conditions)
  "Like `aand!' but bind the result of each condition to `it'."
  `(let (it) (and ,@(mapcar (lambda (c) `(setq it ,c)) conditions))))

(defmacro aif! (cond then &rest else)
  "Like `if' but bind the result of COND to `it' for duration of THEN and ELSE."
  (declare (debug t) (indent 2))
  `(alet! ,cond (if it ,then ,@else)))

(defmacro awhen! (cond &rest body)
  "Like `when' but the result of COND is bound to `it'."
  (declare (debug when) (indent 1))
  `(aif! ,cond (progn ,@body) nil))

(defmacro aprog1! (form &rest body)
  "Like `prog1' but bind first form to `it'."
  (declare (debug when) (indent 1))
  `(alet! ,form (prog1 it ,@body)))

(defmacro each! (list &rest body)
  "Evaluate BODY for each element of LIST and return nil.
Each element of LIST is bound to `it'."
  (declare (debug (form body)) (indent 1))
  `(dolist (it ,list) ,@body))

(defmacro alet2! (form1 form2 &rest body)
  "Bind FORM1 and FORM2 to `it' and `other' and evaluate BODY."
  (declare (debug let) (indent 2))
  `(let ((it ,form1)
         (other ,form2))
     ,@body))

;; The standard way to write this macro is to have it return the element
;; removed.  A more controversial but potentialy useful way is to write one that
;; returns the predicate expression.  That way I could additionally perform some
;; operation on the element.  And worst case I could just return the original
;; element via (and SUBPRED it).
(defmacro aremf! (list pred)
  "Remove the first element that satisfies PRED and return PRED.
PRED should be a form that evaluates with `it` bound to each element."
  (declare (indent 1))
  (let ((glist (gensym "list"))
        (gpred (gensym "pred"))
        (grest (gensym "rest")))
    `(let* ((,glist ,list)
            (,grest nil)
            (,gpred nil)
            (it nil))
       (while ,glist
         (setq it (car ,glist))
         (setq ,gpred ,pred)
         (if ,gpred
             (progn
               (setq ,list (nconc (nreverse ,grest) (cdr ,glist)))
               (setq ,glist nil)) ; exit loop
           (push it ,grest)
           (setq ,glist (cdr ,glist))))
       ,gpred)))
;;; provide
(provide '000-anaphoric-macros)
;;; 000-anaphoric-macros.el ends here
