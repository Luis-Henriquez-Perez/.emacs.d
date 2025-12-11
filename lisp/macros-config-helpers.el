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
(eval-when-compile (require 'macros-loop))

(defmacro o-require (feature)
  "Require feature in lisp directory.
If FEATURE is a regexp, require all features in lisp directory that match
FEATURE."
  (pcase feature
    ((pred stringp)
     (let (forms filename (regexp feature))
       (dolist (file (directory-files (expand-file-name "lisp/" user-emacs-directory) 'full ".+\\.el$"))
         (setq filename (file-name-sans-extension (file-name-nondirectory (directory-file-name file))))
         (when (string-match-p regexp filename)
           (setq feature (intern filename))
           (push `(o-require ,feature) forms)))
       (macroexp-progn (reverse forms))))
    ((pred symbolp)
     (let (forms)
       (setq forms `((require ',feature)))
       (setq forms (let ((err (gensym "error")))
                     `((if o-init-noerrors-p
                           (condition-case ,err
                               ,(macroexp-progn forms)
                             (error
                              (o-log 'failure "Failed to require %S: %s -> %s." ',feature (car ,err) (cdr ,err))))
                         ,(macroexp-progn forms)))))
       (setq forms `((if o-init-profile-p
                         (o-aprog1 (o-time-elapsed ,(macroexp-progn forms))
                           (o-log 'success "Required %s in %.2f seconds" ',feature it)
                           (push (list ',feature it) o-init-data))
                       ,(macroexp-progn forms))))
       ;; Ensure main forms are not evaluated more than once.
       (setq forms `((unless (featurep ',feature)
                       ,(macroexp-progn forms))))
       (when (string-match-p "macros$" (symbol-name feature))
         (setq forms `((eval-when-compile ,(macroexp-progn forms)))))
       (macroexp-progn forms)))
    (_
     (signal 'wrong-type-argument `(or stringp symbolp ,feature)))))

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
(provide 'macros-config-helpers)
;;; macros-config-helpers.el ends here
