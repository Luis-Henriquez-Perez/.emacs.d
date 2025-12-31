;;; macros-config-helpers.el --- Initialize macros-config-helpers -*- lexical-binding: t; -*-
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
;; Initialize macros-config-helpers.
;;
;;; Code:
(require 'base-vars)
(require 'base-log)
(require 'functions-call-after)
(require 'functions-2)
(eval-when-compile (require 'macros-base))
(eval-when-compile (require 'macros-autolet))

(defmacro o-opt (symbol value)
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

(o-defmacro o-setq-mode-local (mode symbol value)
  "Add function to hook that sets the local value of SYMBOL to VALUE."
  (o-set hook (intern (format "%s-hook" mode)))
  (o-set setter (intern (format "o--%s--set-local-variables-h" hook)))
  (o-set docstring (format "Set local variable for `%s'." hook))
  `(progn (defun ,setter (&rest _)
            ,docstring
            (o--set-mode-local-vars ',hook))
          (setf (alist-get ',symbol (alist-get ',hook o-local-var-alist)) ',value)
          (add-hook ',hook #',setter -50)))

(defmacro o-after (feature &rest body)
  "Evaluate BODY after FEATURE has been loaded.
If FEATURE is already loaded, evaluate BODY immediately."
  (declare (indent 1))
  `(o-call-after-load ',feature (lambda () ,@body)))

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

(o-defmacro o-defvar-keymap (keymap &rest pairs)
  "Wrapper around `defvar-keymap'.
In contrast to `defvar-keymap' this macro declares to avoid byte-compilation
warnings.  Also it auto defines a prefix with the same name as KEYMAP."
  (declare (indent 1))
  (o-set plist (o-stripplist pairs))
  (o-set copy pairs)
  (while (consp copy)
    (pop copy)
    (pcase (pop copy)
      (`(function ,fn)
       (o-pushing declareforms `(declare-function ,fn nil)))))
  `(progn ,@(nreverse declareforms)
          (defvar-keymap ,keymap
            :prefix ',keymap
            ,@plist
            ,@pairs)))
;;; provide
(provide 'macros-config-helpers)
;;; macros-config-helpers.el ends here
