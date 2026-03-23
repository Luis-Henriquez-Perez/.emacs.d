;;; init-mac-config-utils.el --- Initialize init-mac-config-utils -*- lexical-binding: t; -*-
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
;; Initialize init-mac-config-utils.
;;
;;; Code:
(require 'init-core-vars)
(require 'init-core-log)
(require 'init-fn-call-after)
(require 'init-fn-2)
(eval-when-compile (require 'init-mac-base))
(eval-when-compile (require 'init-mac-autolet))

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

(defun o--local-register-var-form (hook symbol value)
  "Return the form to store the SYMBOL and VALUE for HOOK."
  (let ((temp (gensym "temp")))
    `(let ((,temp '(,symbol ,value)))
       (unless (member ,temp (alist-get ',hook o-local-settings-alist))
         (push ,temp (alist-get ',hook o-local-settings-alist))))))

(defmacro o-local-mode-setq (mode &rest pairs)
  "Locally set each symbol to its corresponding value when mode is enabled.
\n(FN MODE [SYMBOL VALUE]...)"
  (declare (indent 2))
  (let ((hook (intern (format "%s-hook" mode)))
        (body nil))
    (while pairs
      (setq symbol (pop pairs))
      (cl-assert (symbolp symbol))
      (unless pairs
        (error "Unmatched symbol %s" symbol))
      (setq value (pop pairs))
      (push (o--local-register-var-form hook symbol value) body))
    `(progn (add-hook ',hook (o-local-gen-setter ',mode) -50)
            ,@body)))

(defmacro o-local-add-hook (mode hook fn &optional depth)
  "Locally add FN to HOOK at DEPTH when MODE is enabled."
  (let ((mode-hook (intern (format "%s-hook" mode))))
    `(progn (push '(,hook ,fn ,depth) (alist-get ',mode-hook o-local-settings-alist))
            (add-hook ',hook (o-local-gen-setter ',hook) -50))))

(defmacro o-after (feature &rest body)
  "Evaluate BODY after FEATURE has been loaded.
If FEATURE is already loaded, evaluate BODY immediately."
  (declare (indent 1))
  `(o-call-after-load ',feature (lambda () ,@body)))

(o-defmacro o-defafter (&rest args)
  "Evaluate BODY after FEATURE is loaded."
  (declare (indent defun))
  (o-set (name (feature) meta body) (o-destructure-defun-args args))
  `(progn (o-defun ,name ()
            ,@meta
            (condition-case err
                (progn (with-no-warnings ,@body)
                       (o-log 'success "after %s -> %s" ',feature ',name))
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
(provide 'init-mac-config-utils)
;;; init-mac-config-utils.el ends here
