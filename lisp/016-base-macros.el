;;; 016-base-macros.el --- Initialize 016-base-macros -*- lexical-binding: t; -*-
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
;; Initialize 016-base-macros.
;;
;;; Code:
(require! "^01[0-5]")

(defmacro nif! (cond then &rest else)
  (declare (indent 2))
  `(if (not ,cond)
       ,then
     ,@else))

(defmacro stripplist! (list)
  "Strip and return plist from the front of LIST.
LIST is a list symbol."
  (let ((plist (gensym "plist")))
    `(let (,plist)
       (while (keywordp (car ,list))
         (prepending! ,plist (list (pop ,list) (pop ,list))))
       ,plist)))

(defmacro lef! (bindings &rest body)
  "Bind each symbol in BINDINGS to its corresponding function during BODY.
BINDINGS is a list of either (SYMBOL FUNCTION), where symbol is the symbol to be
bound and FUNCTION is the function to bind it to; or (SYMBOL ARGS BODY).  In
each of BINDINGS if the symbol is an existing function symbol let-bind the
original function to `this-fn', otherwise bind `this-fn' to nil."
  (declare (indent 1))
  (let (binds orig-fn)
    (pcase-dolist (`(,sym . ,rest) bindings)
      (setq orig-fn (gensym "this-fn"))
      (push `(,orig-fn (when (fboundp ',sym) (symbol-function ',sym))) binds)
      (push (list `(symbol-function ',sym)
                  (pcase rest
                    (`(,fn . nil)
                     `(lambda (&rest args)
                        (let ((this-fn ,orig-fn)
                              (this-function ,orig-fn))
                          (ignore this-fn this-function)
                          (apply ,fn args))))
                    (`(,args . ,function-body)
                     `(lambda ,args
                        (let ((this-fn ,orig-fn)
                              (this-function ,orig-fn))
                          (ignore this-fn this-function)
                          ,@function-body)))))
            binds))
    `(cl-letf* ,(nreverse binds) ,@body)))

(defmacro quiet! (&rest body)
  "Run BODY without generating any output.
Silence calls to `message', `load', `write-region' and anything that
writes to `standard-output'."
  `(let ((inhibit-message t)
         (save-silently t)
         (standard-output #'ignore))
     (lef! ((message #'ignore)
            (load
             (lambda (file &optional noerror nomessage nosuffix must-suffix)
               (ignore nomessage)
               (funcall this-fn file noerror t nosuffix must-suffix)))
            (write-region
             (lambda (start end filename &optional append visit lockname mustbenew)
               (unless visit (setq visit 'no-message))
               (funcall this-fn start end filename append visit lockname
                        mustbenew))))
       ,@body)))

(defmacro let! (letbs &rest body)
  "Like `let*' but with support for destructuring."
  (declare (indent defun))
  `(pcase-let* ,(cl-loop for (match-form value) in letbs
                         append (oo-pcase-bindings match-form value))
     ,@body))

(defmacro opt! (symbol value)
  "Set SYMBOL to VALUE when parent feature of SYMBOL is loaded.
This is like `setq' but it is meant for configuring variables."
  (let ((value-var (gensym "value")))
    `(afterbound! ,symbol
       ,(macroexpand-all `(let ((,value-var (with-demoted-errors "Error: %S" (with-no-warnings ,value))))
                            (aif! (get ',symbol 'custom-set)
                                (funcall it ',symbol ,value-var)
                              (setq ,symbol ,value-var)))))))

(defmacro! setq-hook! (hook symbol value)
  "Add function to hook that sets the local value of SYMBOL to VALUE."
  (set! name (intern (format "oo--%s--set-local-var--%s" hook symbol)))
  (set! lambda `(lambda () (setq-local ,symbol ,value)))
  ;; (set! docstring (format "Set local variable `%S' to `%S'." ',symbol ',value))
  (appending! forms `((fset ',name ,lambda) (add-hook ',hook #',name)))
  (macroexp-progn (nreverse forms)))

(declare-function tempel-insert "tempel")
(defmacro! deftempel! (name &rest body)
  "Define a tempel template."
  (declare (doc-string 2) (indent defun))
  (set! documentation (when (stringp (car body)) (list (pop body))))
  `(progn (defun ,name ()
            ,@documentation
            (interactive)
            (require 'tempel)
            (tempel-insert ',body)
            t)
          (put ',name 'no-self-insert t)
          ',name))

(defmacro after! (expr fn &optional feature)
  "Call function after EXPR is met."
  (declare (indent 1))
  `(progn (declare-function ,fn ,(if feature (symbol-name feature) nil))
          ,@(when feature `((autoload #',fn ,(symbol-name feature) nil nil 'function)))
          (oo-call-after-load ',expr #',fn)))

(defmacro afterfeature! (feature &rest body)
  "Eval BODY after FEATURE is loaded."
  (declare (indent 1))
  `(oo-call-after-load ',feature (lambda () (with-no-warnings ,@body))))

(defmacro afterbound! (symbol &rest body)
  "Eval BODY after SYMBOL is bound."
  (declare (indent 1))
  `(oo-call-after-bound ',symbol (lambda () (with-no-warnings ,@body))))
;;; provide
(provide '016-base-macros)
;;; 016-base-macros.el ends here
