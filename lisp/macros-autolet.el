;;; macros-autolet.el --- Macros for automaticating let-binding and more -*- lexical-binding: t; -*-
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
;; Define macros that automate let binding and more.
;;
;;; Code:
(require 'pcase)
(require 'cl-lib)
(require 'functions-destructure)

(defmacro o-set (match-form value)
  "Bind symbols in PATTERN to corresponding VALUE.
If MATCH-FORM is a symbol act as `setq'."
  (if (symbolp match-form)
      `(setq ,match-form ,value)
    (cl-flet ((list-marker-p (it) (and (symbolp it) (equal ?& (aref (symbol-name it) 0)))))
      (let* ((binds (o-pcase-bindings match-form value))
             (non-gensyms (cl-remove-if #'list-marker-p (o-flatten-pcase-match-form match-form)))
             (all (o-flatten-pcase-match-form (mapcar #'car binds)))
             (gensyms (cl-set-difference all non-gensyms)))
        `(let ,gensyms
           ,(macroexp-progn (mapcar (apply-partially #'cons 'pcase-setq) binds)))))))

(defmacro o-return (&optional value)
  "Exit `o-autolet' and return VALUE.
Inside an `o-autolet' form, throw a `o-return' signal, immediately terminating the
evaluation of the `o-autolet' form and return VALUE."
  `(throw 'o-return ,value))

(defmacro o-done ()
  "This is a shorthand for `(o-return nil)'.
See `o-return'."
  `(o-return nil))

(defmacro o-break (&optional value)
  "Exit the current loop and return VALUE.
Inside an `o-autolet' form, exit the current loop and return VALUE."
  `(throw 'break ,value))

(defmacro o-continue ()
  "Skip the current iteration of a loop.
Inside an `o-autolet' form, throw a `o-continue' signal to end the current
iteration and move to the next."
  `(throw 'o-continue nil))

(defmacro stub! (name args &rest body)
  "Indicator for defining local functions via `cl-flet' in `o-autolet' forms."
  (declare (indent defun))
  (ignore name args body))

(defalias 'macrolet! 'stub! "Indicator for defining local macros via `cl-macrolet' in `o-autolet' forms.")
(defalias 'mlet! 'macrolet!)
(defalias 'o-flet 'stub! "Same as `stub!'.")
(defalias 'noflet! 'stub! "Indicator for temporary overriding function definitions via `o-lef'.")
(defalias 'nflet! 'stub! "Same as `noflet!'")

(defun o-autolet-process-body (body)
  "Return a list of (LETB FORM).
LETB is a list of let-bindings.  FORM is a possibly modified version of BODY."
  (let ((letb '())
        (stack (list (cons nil body)))
        (result '())
        frame cdr-val car-val)
    (while stack
      (setq frame (pop stack))
      (cond ((equal (car frame) t)
             (setq cdr-val (pop result))
             (setq car-val (pop result))
             (push (cons car-val cdr-val) result))
            ((equal (car frame) :loop)
             (let ((loop-type (cadr frame))
                   (body (cdr (pop result)))
                   (pred (cadr (pop result))))
               (push `(catch 'break! (,loop-type ,pred (catch 'o-continue ,@body))) result)))
            ((equal (car frame) :shortcut)
             (let ((type (cadr frame))
                   (args (caddr frame)))
               (push `((,type (,args) ,@(cdr (pop result)))) result)))
            ((car frame)
             (error "Unknown frame %S" frame))
            ((atom (cdr frame))
             (push (cdr frame) result))
            ((memq (car (cdr frame)) '(quote backquote function cl-function))
             (push (cdr frame) result))
            ;; o-set
            ((and (equal (cadr frame) 'o-set)
                  (nthcdr 2 (cdr frame))
                  (caddr frame))
             (if (sequencep (caddr frame))
                 (dolist (symbol (reverse (o-flatten-pcase-match-form (caddr frame))))
                   (cl-pushnew (list symbol nil) letb :key #'car))
               (cl-pushnew (list (caddr frame) nil) letb :key #'car))
             (push (cdr frame) result))
            ;; Ing Macros
            ;; ((and (memq (cadr frame) '(maxing! maximizing!))
            ;;       (symbolp (caddr frame))
            ;;       (nthcdr 2 (cdr frame)))
            ;;  (cl-pushnew `(,(caddr frame) most-negative-fixnum) letb)
            ;;  (push (cdr frame) result))
            ;; ((and (memq (cadr frame) '(mining! minimizing!))
            ;;       (symbolp (caddr frame))
            ;;       (nthcdr 2 (cdr frame)))
            ;;  (cl-pushnew `(,(caddr frame) most-positive-fixnum) letb)
            ;;  (push (cdr frame) result))
            ;; adding! counting!
            ((and (memq (cadr frame) '(o-summing))
                  (symbolp (caddr frame))
                  (nthcdr 2 (cdr frame)))
             (push `(,(caddr frame) 0) letb)
             (push (cdr frame) result))
            ((and (symbolp (cadr frame))
                  (string-match-p "^o-[[:alpha:]-]+ing$" (symbol-name (cadr frame)))
                  (nthcdr 2 (cdr frame)))
             (cl-pushnew `(,(caddr frame) nil) letb :key #'car)
             (push (cdr frame) result))
            ;; Loop
            ((and (memq (cadr frame) '(while dolist dotimes o-for))
                  (nthcdr 1 (cdr frame)))
             (push `(:loop ,(cadr frame)) stack)
             (push `(nil . (progn ,@(cdddr frame))) stack)
             (push `(nil . (progn ,(caddr frame))) stack))
            ;; Shortcuts
            ((and (listp (cadr frame))
                  (memq (caadr frame) '(nflet! noflet!))
                  (nthcdr 1 (cadr frame)))
             (push `(:shortcut o-lef ,(cdadr frame)) stack)
             (push `(nil . (progn ,@(cddr frame))) stack))
            ((and (listp (cadr frame))
                  (memq (caadr frame) '(o-flet stub!))
                  (nthcdr 1 (cadr frame)))
             (push `(:shortcut cl-flet ,(cdadr frame)) stack)
             (push `(nil . (progn ,@(cddr frame))) stack))
            ((and (listp (cadr frame))
                  (memq (caadr frame) '(label!))
                  (nthcdr 1 (cadr frame)))
             (push `(:shortcut cl-labels ,(cdadr frame)) stack)
             (push `(nil . (progn ,@(cddr frame))) stack))
            ((and (listp (cadr frame))
                  (memq (caadr frame) '(macrolet! mlet!))
                  (nthcdr 1 (cadr frame)))
             (push `(:shortcut cl-macrolet ,(cdadr frame)) stack)
             (push `(nil . (progn ,@(cddr frame))) stack))
            ;; Generic list
            ((listp (cdr frame))
             (push (cons t nil) stack)
             (push (cons nil (cddr frame)) stack)
             (push (cons nil (cadr frame)) stack))
            (t
             (error "Unknown frame %S" frame))))
    (list (nreverse letb) (car result))))

;; Sometimes you do not want symbol to be auto let-bound to nil, you actually
;; want to just modify the original symbol without let-binding it at all.  In
;; that case use `:noinit' which tells `o-autolet' not to bind specified symbols
;; at all.  Other times you want a symbol to be bound to something else than the
;; default.  For example, counting! starts at 0 by default but maybe you want to
;; start at 10, in that case you can do `:init' ((count 10)).  I suppose init
;; can be used as a single-line alternative to `let*'.
(defmacro o-autolet (noinits &rest body)
  "Dynamically let-bind symbols and modify forms in BODY.

Process BODY by recognizing special forms and keywords for dynamically
let-binding symbols, automatically wrapping forms and enhancing the control flow
of loops.

Dynamic let-binding:
(o-set SYM _)      Let bind SYM to nil.
(maxing! SYM _)   Let bind SYM to `most-negative-fixnum'.
(minning! SYM _)  Let bind SYM to `most-positive-fixnum'.
(counting! SYM _) Let bind SYM to 0.
(...ing! SYM VAL) Let bind SYM to nil.

Wrapping forms:
(mlet!|macrolet! NAME ARGS . BODY) Wrap subsequent forms with
`(cl-macrolet ((NAME ARGS . BODY)))'.
(stub!|flet! NAME ARGS . BODY)     Same as macrolet but use `cl-letf'.
(nflet!|noflet! NAME ARGS . BODY)  Same as `stub!' but use `o-lef'.
(label!|labels! NAME ARGS . BODY)  Same as `stub!' but use `cl-labels'.

Enhanced looping control flow:
(while|dotimes|dolist CONDITION . BODY) Replace with
`(catch \='o-return (LOOP CONDITION (catch \='break! BODY)))'."
  (declare (indent defun))
  (pcase-let ((`(,bindings ,body) (o-autolet-process-body body)))
    `(let ,(cl-remove-if (lambda (it) (member (car it) noinits)) bindings)
       (catch 'o-return ,@body))))

(defmacro o-defmacro (&rest args)
  "Same as `o-defmacro' but wrap body with `o-autolet'.
NAME, ARGLIST and BODY are the same as `o-defmacro'.

\(fn NAME ARGLIST [DOCSTRING] BODY...)"
  (declare (indent defun) (doc-string 3))
  (pcase-let ((`(,name ,arglist ,metadata ,body) (o-destructure-defun-args args)))
    `(defmacro ,name ,arglist
       ,@metadata
       (o-autolet ,(o-arglist-symbols arglist)
         ,@body))))

(defmacro o-defun (&rest args)
  "Same as `defun' but wrap body with `o-autolet'.
NAME, ARGS and BODY are the same as in `defun'.

\(fn NAME ARGLIST [DOCSTRING] [DECL] [INTERACTIVE] BODY...)"
  (declare (indent defun) (doc-string 3))
  (pcase-let ((`(,name ,arglist ,metadata ,body) (o-destructure-defun-args args)))
    `(defun ,name ,arglist
       ,@metadata
       (o-autolet ,(o-arglist-symbols arglist)
         ,@body))))
;;; provide
(provide 'macros-autolet)
;;; macros-autolet.el ends here
