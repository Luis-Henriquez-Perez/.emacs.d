;;; 002-init-loader.el --- Macro for loading numbered files -*- lexical-binding: t; -*-
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
;; Provide tools to profile my configuration as well as to gracefully handle
;; errors in initialization.  Specifically, provide the macro `load!'.  Load is
;; designed to free me from having to explicitly manage the.  I cannot
;; make `load' a function the compiler will not detect the `require' calls.
;;
;;; Code:
(require 'cl-lib)
(require '001-init-log)

(defmacro time-elapsed! (&rest forms)
  "Eval forms and return the time elapsed."
  (cl-with-gensyms (start)
    `(let ((,start (float-time)))
       ,(macroexp-progn forms)
       (/ (fround (* (- (float-time) ,start) 100)) 100.0))))

(defun oo-features (regexp)
  "Return list of lisp features from user lisp directory."
  (cl-flet* ((base (path) (file-name-sans-extension (file-name-nondirectory (directory-file-name path))))
             (feature (path) (intern (base path))))
    (mapcar #'feature (directory-files (expand-file-name "lisp/" user-emacs-directory) 'full regexp))))

(cl-defmacro require! (feature &key profile error-check)
  "Require feature in lisp directory.
If FEATURE is a regexp, require all features in lisp directory that match FEATURE."
  (let (forms)
    (pcase feature
      ((pred stringp)
       (dolist (feature (oo-features feature))
         (push `(require! ,feature :profile ,profile :error-check ,error-check) forms))
       (setq forms (nreverse forms)))
      ((pred symbolp)
       (setq forms `((require ',feature)))
       (when error-check
         (setq forms (let ((err (gensym "error")))
                       `((condition-case ,err
                             ,(macroexp-progn forms)
                           (error
                            (oo-log 'error "requiring %S: %s -> %s." ',feature (car ,err) (cdr ,err))))))))
       (when profile
         (setq forms `((oo-log 'info "Required %s in %.2f seconds" ',feature (time-elapsed! ,(macroexp-progn forms))))))
       (when (string-match-p "macros$" (symbol-name feature))
         (setq forms `((eval-when-compile ,(macroexp-progn forms))))))
      (_
       (error "wrong type argument")))
    (macroexp-progn forms)))
;;; provide
(provide '002-init-loader)
;;; 002-init-loader.el ends here
