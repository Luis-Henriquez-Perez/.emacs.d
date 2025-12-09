;;; functions-call-after.el --- Define functions for deferred loading -*- lexical-binding: t; -*-
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
;; Define functions for deferred loading
;;
;;; Code:
(require 'subr-x)
(eval-when-compile (require 'macros-base))
(require 'base-log)

(defvar o-after-bound-forms nil
  "An alist whose elements are (SYMBOL . FORMS).
SYMBOL is a variable symbol.  FORMS are a list of lisp forms that should be
evaluated when symbol is bound.")

(defvar o-after-load-forms (make-hash-table :size 100)
  "A hash table whose elements are (FEATURE . FORMS).
FEATURE is a feature symbol.  FORMS are alist of lisp forms to be evaluated
after FEATURE is loaded.")

(defun o-eval-after-bound-forms (&rest _)
  "Evaluate forms of any bound symbols in `o-after-bound-forms'."
  (let (updated symbol forms)
    (dolist (elt (reverse o-after-bound-forms))
      (setq symbol (car elt))
      (setq forms (cdr elt))
      (if (boundp symbol)
          (eval `(progn ,@(nreverse forms)) 'lexical)
        (push elt updated)))
    (setq o-after-bound-forms updated)))

(defun o-call-after-bound (symbol fn)
  "Call FN after SYMBOL is bound.
If SYMBOL is already bound FN is called immediately."
  (if (boundp symbol)
      (funcall fn)
    (push `(ignore-errors (funcall ',fn)) (alist-get symbol o-after-bound-forms))))

(defun o-call-after-load (feature fn)
  "Call FN after FEATURE is loaded."
  (if (featurep feature)
      (funcall fn)
    (push `(ignore-errors (funcall ',fn)) (gethash feature o-after-load-forms))
    (eval-after-load feature
      ;; Cannot use my macros here because when compiled Emacs will not know how
      ;; to macroexpand them.
      `(let ((it (gethash ',feature o-after-load-forms)))
         (when it
           (eval (macroexp-progn (nreverse it)) 'lexical)
           (remhash ',feature o-after-load-forms))))))

(defun o-require-config (feature)
  "Load and log the loading of FEATURE."
  (condition-case err
      (o-aprog1 (o-time-elapsed (require feature))
        (o-log 'success "Applied %s in %0.2f seconds" feature it))
    (error
     (o-log 'failure "Failed to apply %s : %S -> %S" feature (car err) (cdr err)))))

(defun o-require-after-load (feature1 feature2)
  "Load FEATURE2 at FEATURE1 has been loaded."
  (o-call-after-load feature1 (apply-partially #'o-require-config feature2)))
;;; provide
(provide 'functions-call-after)
;;; functions-call-after.el ends here
