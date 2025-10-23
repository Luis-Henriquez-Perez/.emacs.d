;;; 004-autolet-macros.el --- Macros for automaticating let-binding and more -*- lexical-binding: t; -*-
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
(require '001-base-functions)

(defun oo-arglist-symbols (arglist)
  "Return a list of argument symbols."
  (let (symbols)
    (dolist (arg (flatten-list arglist))
      (when (and (symbolp arg)
                 (not (equal arg '_))
                 (not (string-match "^&" (symbol-name arg))))
        (push arg symbols)))
    (nreverse symbols)))

(defmacro set! (match-form value)
  "Bind symbols in PATTERN to corresponding VALUE.
If MATCH-FORM is a symbol act as `setq'."
  (if (symbolp match-form)
      `(setq ,match-form ,value)
    (cl-flet ((list-marker-p (it) (and (symbolp it) (equal ?& (aref (symbol-name it) 0)))))
      (let* ((binds (oo-pcase-bindings match-form value))
             (non-gensyms (cl-remove-if #'list-marker-p (oo-flatten-pcase-match-form match-form)))
             (all (oo-flatten-pcase-match-form (mapcar #'car binds)))
             (gensyms (cl-set-difference all non-gensyms)))
        `(let ,gensyms
           ,(macroexp-progn (mapcar (apply-partially #'cons 'pcase-setq) binds)))))))

(defmacro return! (&optional value)
  "Exit `autolet!' and return VALUE.
Inside an `autolet!' form, throw a `return!' signal, immediately terminating the
evaluation of the `autolet!' form and return VALUE."
  `(throw 'return! ,value))

(defmacro done! ()
  "This is a shorthand for `(return! nil)'.
See `return!'."
  `(return! nil))

(defmacro break! (&optional value)
  "Exit the current loop and return VALUE.
Inside an `autolet!' form, exit the current loop and return VALUE."
  `(throw 'break! ,value))

(defmacro continue! ()
  "Skip the current iteration of a loop.
Inside an `autolet!' form, throw a `continue!' signal to end the current
iteration and move to the next."
  `(throw 'continue! nil))

(defalias 'skip! 'continue!)

(defmacro stub! (name args &rest body)
  "Indicator for defining local functions via `cl-flet' in `autolet!' forms."
  (declare (indent defun))
  (ignore name args body))

(defalias 'macrolet! 'stub! "Indicator for defining local macros via `cl-macrolet' in `autolet!' forms.")
(defalias 'mlet! 'macrolet!)
(defalias 'flet! 'stub! "Same as `stub!'.")
(defalias 'noflet! 'stub! "Indicator for temporary overriding function definitions via `lef!'.")
(defalias 'nflet! 'stub! "Same as `noflet!'")

(defun oo-autolet-process-iterative-cond (body)
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
               (push `(catch 'break! (,loop-type ,pred (catch 'continue! ,@body))) result)))
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
            ;; Set!
            ((and (equal (cadr frame) 'set!)
                  (nthcdr 2 (cdr frame))
                  (caddr frame))
             (if (sequencep (caddr frame))
                 (dolist (symbol (reverse (oo-flatten-pcase-match-form (caddr frame))))
                   (cl-pushnew (list symbol nil) letb :key #'car))
               (cl-pushnew (list (caddr frame) nil) letb :key #'car))
             (push (cdr frame) result))
            ;; Ing Macros
            ((and (memq (cadr frame) '(maxing! maximizing!))
                  (symbolp (caddr frame))
                  (nthcdr 2 (cdr frame)))
             (cl-pushnew `(,(caddr frame) most-negative-fixnum) letb)
             (push (cdr frame) result))
            ((and (memq (cadr frame) '(mining! minimizing!))
                  (symbolp (caddr frame))
                  (nthcdr 2 (cdr frame)))
             (cl-pushnew `(,(caddr frame) most-positive-fixnum) letb)
             (push (cdr frame) result))
            ((and (memq (cadr frame) '(summing! adding! counting!))
                  (symbolp (caddr frame))
                  (nthcdr 2 (cdr frame)))
             (push `(,(caddr frame) 0) letb)
             (push (cdr frame) result))
            ((and (symbolp (cadr frame))
                  (string-match-p "ing!$" (symbol-name (cadr frame)))
                  (nthcdr 2 (cdr frame)))
             (cl-pushnew `(,(caddr frame) nil) letb :key #'car)
             (push (cdr frame) result))
            ;; Loop
            ((and (memq (cadr frame) '(while dolist dotimes for!))
                  (nthcdr 1 (cdr frame)))
             (push `(:loop ,(cadr frame)) stack)
             (push `(nil . (progn ,@(cdddr frame))) stack)
             (push `(nil . (progn ,(caddr frame))) stack))
            ;; Shortcuts
            ((and (listp (cadr frame))
                  (memq (caadr frame) '(nflet! noflet!))
                  (nthcdr 1 (cadr frame)))
             (push `(:shortcut lef! ,(cdadr frame)) stack)
             (push `(nil . (progn ,@(cddr frame))) stack))
            ((and (listp (cadr frame))
                  (memq (caadr frame) '(flet! stub!))
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
;; that case use `:noinit' which tells `autolet!' not to bind specified symbols
;; at all.  Other times you want a symbol to be bound to something else than the
;; default.  For example, counting! starts at 0 by default but maybe you want to
;; start at 10, in that case you can do `:init' ((count 10)).  I suppose init
;; can be used as a single-line alternative to `let*'.
(defmacro autolet! (noinits &rest body)
  "Dynamically let-bind symbols and modify forms in BODY.

Process BODY by recognizing special forms and keywords for dynamically
let-binding symbols, automatically wrapping forms and enhancing the control flow
of loops.

Dynamic let-binding:
(set! SYM _)      Let bind SYM to nil.
(maxing! SYM _)   Let bind SYM to `most-negative-fixnum'.
(minning! SYM _)  Let bind SYM to `most-positive-fixnum'.
(counting! SYM _) Let bind SYM to 0.
(...ing! SYM VAL) Let bind SYM to nil.

Wrapping forms:
(mlet!|macrolet! NAME ARGS . BODY) Wrap subsequent forms with
`(cl-macrolet ((NAME ARGS . BODY)))'.
(stub!|flet! NAME ARGS . BODY)     Same as macrolet but use `cl-letf'.
(nflet!|noflet! NAME ARGS . BODY)  Same as `stub!' but use `lef!'.
(label!|labels! NAME ARGS . BODY)  Same as `stub!' but use `cl-labels'.

Enhanced looping control flow:
(while|dotimes|dolist CONDITION . BODY) Replace with
`(catch \='return! (LOOP CONDITION (catch \='break! BODY)))'."
  (declare (indent defun))
  (pcase-let ((`(,bindings ,body) (oo-autolet-process-iterative-cond body)))
    `(let ,(cl-remove-if (lambda (it) (member (car it) noinits)) bindings)
       (catch 'return! ,@body))))

(defmacro defmacro! (&rest args)
  "Same as `defmacro!' but wrap body with `autolet!'.
NAME, ARGLIST and BODY are the same as `defmacro!'.

\(fn NAME ARGLIST [DOCSTRING] BODY...)"
  (declare (indent defun) (doc-string 3))
  (pcase-let ((`(,name ,arglist ,meta ,body) (oo-destructure-defun args)))
    `(defmacro ,name ,arglist
       ,@meta
       (autolet! ,(oo-arglist-symbols arglist)
         ,@body))))

(defmacro defun! (&rest args)
  "Same as `defun' but wrap body with `autolet!'.
NAME, ARGS and BODY are the same as in `defun'.

\(fn NAME ARGLIST [DOCSTRING] [DECL] [INTERACTIVE] BODY...)"
  (declare (indent defun) (doc-string 3))
  (pcase-let ((`(,name ,arglist ,metadata ,body) (oo-destructure-defun args)))
    `(defun ,name ,arglist
       ,@metadata
       (autolet! ,(oo-arglist-symbols arglist)
         ,@body))))
;;; provide
(provide '004-autolet-macros)
;;; 004-autolet-macros.el ends here
