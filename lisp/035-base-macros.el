;;; 035-base-macros.el --- Initialize 035-base-macros -*- lexical-binding: t; -*-
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
;; Initialize 035-base-macros.
;;
;;; Code:
(require '032-after-load-functions)
(require '030-base-functions)
(eval-when-compile (require '031-anaphoric-macros))
(eval-when-compile (require '031-autolet-macros))
(eval-when-compile (require '031-modification-macros))
(eval-when-compile (require '031-looping-macros))

(defmacro autoload! (function file)
  `(progn (declare-function ,function ,file)
          (autoload #',function ,file nil t 'function)))

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

(defmacro! with-map-keywords! (map &rest body)
  "Let-bind bang symbols in BODY corresponding to keywords in MAP."
  (declare (indent 1))
  (set! mapsym (gensym "map"))
  (set! let-binds `((,mapsym ,map)))
  (dolist (obj (flatten-tree body))
    (when (and obj
               (symbolp obj)
               (string-match "\\(!\\{1,2\\}\\)\\([^[:space:]]+\\)" (symbol-name obj))
               (not (assoc obj let-binds)))
      (set! symbol obj)
      (set! name (symbol-name symbol))
      (set! key (oo-into-keyword (match-string 2 name)))
      (cond ((= 1 (length (match-string 1 name)))
             (collecting! let-binds `(,symbol (map-elt ,mapsym ',key))))
            (t
             (collecting! let-binds `(,symbol (map-contains-key ,mapsym ',key)))))))
  `(let* ,let-binds
     ,@body))

(defmacro opt! (symbol value)
  "Set SYMBOL to VALUE when parent feature of SYMBOL is loaded.
This is like `setq' but it is meant for configuring variables."
  (let* ((value-var (gensym "value"))
         (main-form (macroexpand-all `(let ((,value-var (with-demoted-errors "Error: %S" (with-no-warnings ,value))))
                                        (aif! (get ',symbol 'custom-set)
                                            (funcall it ',symbol ,value-var)
                                          (with-no-warnings (setq ,symbol ,value-var)))))))
    `(if (not (boundp ',symbol))
         ;; This quote on he lambda is needed to avoid infinite recursion.
         (push '(lambda () ,main-form) (gethash ',symbol oo-after-load-hash-table))
       ,main-form)))

;; I made the decision to add a hook function to a hook regardless of whether
;; the hook has already has been run.  But if the hook has been run the hook
;; function is called individually.  The idea is that I do not want to just
;; evaluate the body and have no record of it being evaluated other than it is
;; side-effects.
(defmacro defafter! (name expr &rest body)
  "Evaluate BODY after EXPR is satisfied."
  (declare (indent defun))
  `(progn
     (defun! ,name nil (with-no-warnings ,@body))
     (oo-call-after-load ',expr #',name)))

(defmacro hook! (hook function &rest args)
  "Configuration wrapper around `oo-add-hook'."
  `(progn (declare-function ,function nil)
          (oo-add-hook ',hook #',function ,@args)))

(defmacro! defhook! (name args &rest body)
  "Add function to hook as specified by NAME."
  (declare (indent defun))
  (while (aand! (car args) (symbolp it) (not (keywordp it)))
    (collecting! hooks (pop args)))
  (when (stringp (car body))
    (collecting! metadata (pop body)))
  (when (equal 'declare (car-safe (car body)))
    (collecting! metadata (pop body)))
  (while (keywordp (car body))
    (appending! args (list (pop body) (pop body))))
  (dolist (hook hooks)
    (collecting! hook-forms `(oo-add-hook ',hook ',name ,@args)))
  `(progn
     (defun! ,name nil ,@metadata ,@body)
     ,@hook-forms))

(defmacro! setq-hook! (hooks symbol value)
  "Add function to hook that sets the local value of SYMBOL to VALUE."
  (let (forms)
    (dolist (hook (ensure-list hooks))
      (set! name (intern (format "oo--%s--set-local-var--%s" hook symbol)))
      (set! lambda `(lambda () (setq-local ,symbol ,value)))
      ;; (set! docstring (format "Set local variable `%S' to `%S'." ',symbol ',value))
      (push `(oo-add-hook ',hook #',lambda :name ',name) forms))
    `(progn ,@(nreverse forms))))
;;; provide
(provide '035-base-macros)
;;; 035-base-macros.el ends here
