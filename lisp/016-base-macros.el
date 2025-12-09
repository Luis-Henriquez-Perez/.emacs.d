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
(require 'functions-call-after)
(eval-when-compile (require 'macros-base))
(eval-when-compile (require 'macros-autolet))
(eval-when-compile (require 'macros-loop))

(o-defmacro o-opt (symbol value)
  "Set SYMBOL to VALUE when parent feature of SYMBOL is loaded.
This is like `setq' but it is meant for configuring variables."
  `(o-alet (lambda ()
            (condition-case err
                (let ((value (with-no-warnings ,value)))
                  (if-let (setter (get ',symbol 'custom-set))
                      (funcall setter ',symbol value)
                    (setq ,symbol value)))
              (error
               (o-log 'failure "Failed to set %s: %S -> %S" ',symbol (car err) (cdr err)))))
     (o-call-after-bound ',symbol it)))

(defconst o-local-var-depth -50
  "Depth in hook at which to set local variables.")

(o-defun o-apply-local-vars (hook)
  "Apply local variables for hook."
  (o-set failmsg "Failed to set local variable %s: %S ->%S")
  (o-for ((symbol . value) (alist-get hook o-local-var-alist))
    (o-set bodyform `(setq-local ,symbol ,value))
    (o-set handlerbody `(o-log 'failure ,failmsg ',symbol (car err) (cdr err)))
    (o-pushing forms `(condition-case err ,bodyform (error ,handlerbody))))
  (eval (macroexp-progn (nreverse forms)) t))

(o-defmacro o-setq-mode-local (hook symbol value)
  "Add function to hook that sets the local value of SYMBOL to VALUE."
  (o-set setter (intern (format "o--%s--init-local-variables-h" hook)))
  (o-set docstring (format "Set local variable for `%s'." hook))
  `(progn (unless (fboundp ',setter)
            (defun ,setter (&rest _)
              ,docstring
              (o-apply-local-vars ',hook)))
          (setf (alist-get ',symbol (alist-get ',hook o-local-var-alist)) ',value)
          (add-hook ',hook #',setter o-local-var-depth)))

(declare-function tempel-insert "tempel")
(o-defmacro o-deftemplate (name &rest body)
  "Define a tempel template."
  (declare (doc-string 2) (indent defun))
  (o-set documentation (when (stringp (car body)) (list (pop body))))
  `(progn (defun ,name ()
            ,@documentation
            (interactive)
            (require 'tempel)
            (tempel-insert ',body)
            t)
          (put ',name 'no-self-insert t)
          ',name))

(o-defmacro o-defafter (&rest args)
  "Eval BODY after FEATURE is loaded."
  (declare (indent defun))
  (o-set (name (feature) meta body) (o-destructure-defun-args args))
  `(progn (o-defun ,name ()
            ,@meta
            (condition-case err
                (with-no-warnings ,@body)
              (error
               (o-log 'failure "Failed to call `%s': %S -> %S" ',name (car err) (cdr err)))))
          (o-call-after-load ',feature #',name)))
;;; provide
(provide '016-base-macros)
;;; 016-base-macros.el ends here
