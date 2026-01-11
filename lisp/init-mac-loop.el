;;; init-mac-loop.el --- Looping macros -*- lexical-binding: t; -*-
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
;; This is.
;;
;;; Code:
(require 'pcase)
(require 'seq)
(require 'cl-lib)
(require 'functions-base)

(defmacro o-for (loop-struct &rest body)
  "A generic looping macro and drop-in replacement for `dolist'.
BODY is the body of the loop.  LOOP-STRUCT determines how `o-for' loops and can
take the following forms:

(VAR NUMBER) Same as `dotimes'.

(MATCH-FORM SEQUENCE) Evaluate BODY for every element in sequence.

(reverse MATCH-FORM LIST) Evaluate body for each element of LIST in reversed order.

(repeat n) Evaluate BODY N times where (> n 0)."
  (declare (indent 1))
  (pcase loop-struct
    ((or (and (pred integerp) n) `(:repeat ,n))
     `(dotimes (_ ,n) ,@body))
    (`(:reverse ,match-form ,list)
     (let ((v (make-symbol "vector"))
           (i (make-symbol "i")))
       `(let* ((,v (vconcat ,list))
               (,i (length ,v)))
          (while (> ,i 0)
            (setq ,i (1- ,i))
            (pcase-let* ,(o-pcase-bindings match-form `(aref ,v ,i))
              ,@body)))))
    (`(,(and match-form (or (pred listp) (pred vectorp))) ,list . ,(and rest (guard t)))
     (cl-with-gensyms (elt)
       `(o-for (,elt ,list ,@rest)
          (pcase-let* ,(o-pcase-bindings match-form elt)
            ,@body))))
    (`(,elt ,list :by ,fn)
     `(cl-loop for ,elt on ,list by ,fn
               do ,(macroexp-progn body)))
    (`(,(and elt (pred symbolp)) ,list)
     (cl-once-only (list)
       `(cond ((listp ,list)
               (dolist (,elt ,list) ,@body))
              ((sequencep ,list)
               (seq-doseq (,elt ,list) ,@body))
              ((integerp ,list)
               (dotimes (,elt ,list) ,@body))
              (t
               (error "Unknown list predicate: %S" ',loop-struct)))))))

(defmacro o-collect (loop-struct &rest body)
  (declare (indent 1))
  (let ((collection (make-symbol "--collection--")))
    `(let (,collection)
       (o-for ,loop-struct
         (push (progn ,@body) ,collection))
       (nreverse ,collection))))
;;; provide
(provide 'init-mac-loop)
;;; init-mac-loop.el ends here
