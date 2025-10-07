;;; 012-autolet-macros.el --- Macros for automaticating let-binding and more -*- lexical-binding: t; -*-
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
(require '010-base-functions)

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
  "This is a shorthand for `(return! nil)'."
  `(return! nil))

(defmacro quitif! (condition &rest args)
  "Exit `autolet!' and print MESSAGE if CONDITION is true."
  `(when ,condition
     (return!)
     ,@(when args `((message ,(car args) ,@(cdr args))))))

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

(defmacro oo--autolet-inits (bodysym)
  "Extract let-bindings and excluded let-binding symbols from BODYSYM.
BODYSYM is the symbol whose value is the body."
  (let ((inits (gensym "inits"))
        (noinits (gensym "noinits"))
        (letbind (gensym "letbind")))
    `(let (,inits ,noinits)
       (while (pcase ,bodysym
                (`(:init ,(pred listp) . ,(guard t))
                 (pop ,bodysym)
                 (dolist (,letbind (pop ,bodysym))
                   (pcase ,letbind
                     ((pred symbolp)
                      (push (list ,letbind nil) ,inits))
                     (`(,_)
                      (push (append ,letbind (list nil)) ,inits))
                     (`(,_ ,_)
                      (push ,letbind ,inits))))
                 t)
                (`(:noinit ,(pred listp) . ,(guard t))
                 (pop ,bodysym)
                 (setq ,noinits (append ,noinits (pop ,bodysym)))
                 t)))
       (list ,inits ,noinits))))

(defun oo--autolet-data (body)
  "Return let-bindings and processed forms used in `autolet!'.
Identify and collect symbols needed for let bindings and return forms modified."
  (pcase-let ((`(,init ,noinit) (oo--autolet-inits body))
              (bindings nil)
              (lets '((mlet! . cl-macrolet)
                      (macrolet! . cl-macrolet)
                      (nflet! . lef!)
                      (noflet! . lef!)
                      (flet! . cl-flet)
                      (stub! . cl-flet)
                      (label! . cl-labels)
                      (labels! . cl-labels))))
    (cl-flet ((should-remove-p (x) (or (member (car x) noinit) (assoc (car x) init))))
      (cl-labels ((process-form (form)
                    (pcase form
                      ((pred atom) form)
                      ;; Leave quoted forms as-is.
                      (`(,(and it (guard (memq it '(quote function backquote cl-function)))) . ,_)
                       form)
                      ;; Match `(set! VAR VALUE)` and collect VARIABLE.
                      (`(set! ,pattern ,_ . ,(guard t))
                       (if (symbolp pattern)
                           (cl-pushnew (list pattern nil) bindings :key #'car)
                         (dolist (symbol (reverse (oo-flatten-pcase-match-form pattern)))
                           (cl-pushnew (list symbol nil) bindings :key #'car)))
                       form)
                      ;; Surround loops with a catch.
                      (`(,(and loop (guard (memq x '(while dolist dotimes for!)))) ,pred . ,(and body (guard t)))
                       `(catch 'break! (,loop ,pred (catch 'continue! ,@(process-form body)))))
                      ;; Properly initialize variables in ingmacro declarations.
                      ;; Just for brevity I use string-match to check instead
                      ;; of listing all my ing macros but there has been a clash
                      ;; with org-ml that uses some macros that end in "ing!".
                      (`(,(and name (guard (and (symbolp name) (string-match-p "ing!$" (symbol-name name))))) ,symbol . ,(guard t))
                       (cl-case name
                         ((maxing! maximizing!)
                          (cl-pushnew `(,symbol most-negative-fixnum) bindings))
                         ((minning! minimizing!)
                          (cl-pushnew `(,symbol most-positive-fixnum) bindings))
                         ((summing! adding! counting!)
                          (cl-pushnew `(,symbol 0) bindings))
                         (t
                          (cl-pushnew `(,symbol nil) bindings :key #'car)))
                       form)
                      ;; Handle special let shortcuts.
                      (`((,(and macro (guard (assoc macro lets))) . ,args) . ,(and rest (guard t)))
                       `((,(alist-get macro lets) ((,@args)) ,@(process-form rest))))
                      ;; Recurse into lists.
                      (_
                       (cons (process-form (car form)) (process-form (cdr form)))))))
        (setq body (process-form body)))
      (setq bindings (append init (cl-remove-if #'should-remove-p bindings))))
    (list bindings body)))

;; Sometimes you do not want symbol to be auto let-bound to nil, you actually
;; want to just modify the original symbol without let-binding it at all.  In
;; that case use `:noinit' which tells `autolet!' not to bind specified symbols
;; at all.  Other times you want a symbol to be bound to something else than the
;; default.  For example, counting! starts at 0 by default but maybe you want to
;; start at 10, in that case you can do `:init' ((count 10)).  I suppose init
;; can be used as a single-line alternative to `let*'.
(defmacro autolet! (&rest body)
  "Dynamically let-bind symbols and modify forms in BODY.

Process BODY by recognizing special forms and keywords for dynamically
let-binding symbols, automatically wrapping forms and enhancing the control flow
of loops.

Keywords:
:init BINDINGS    Predefine symbol bindings.  Follow the same format as `let*'.
This takes precedence over other let-binding indicators.
:noinit SYMS      Do not let bind any symbols in SYMS, regardless of whether
those symbols where specified by dynamic let-binding indicators.

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
  (pcase-let ((`(,bindings ,body) (oo--autolet-data body)))
    `(let ,bindings (catch 'return! ,@body))))

(defmacro defmacro! (&rest args)
  "Same as `defmacro!' but wrap body with `autolet!'.
NAME, ARGLIST and BODY are the same as `defmacro!'.

\(fn NAME ARGLIST [DOCSTRING] BODY...)"
  (declare (indent defun) (doc-string 3))
  (pcase-let ((`(,name ,arglist ,meta ,body) (oo-destructure-defun args)))
    `(defmacro ,name ,arglist
       ,@meta
       (autolet! :noinit ,(oo-arglist-symbols arglist)
                 ,@body))))

(defmacro defun! (&rest args)
  "Same as `defun' but wrap body with `autolet!'.
NAME, ARGS and BODY are the same as in `defun'.

\(fn NAME ARGLIST [DOCSTRING] [DECL] [INTERACTIVE] BODY...)"
  (declare (indent defun) (doc-string 3))
  (pcase-let ((`(,name ,arglist ,metadata ,body) (oo-destructure-defun args)))
    `(defun ,name ,arglist
       ,@metadata
       (autolet! :noinit ,(oo-arglist-symbols arglist)
                 ,@body))))
;;; provide
(provide '012-autolet-macros)
;;; 012-autolet-macros.el ends here
