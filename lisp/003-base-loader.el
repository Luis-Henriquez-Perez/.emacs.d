;;; 003-base-loader.el --- Macro for loading numbered files -*- lexical-binding: t; -*-
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
(require '001-base-vars)
(require '002-base-log)

(defmacro time-elapsed! (&rest forms)
  "Eval forms and return the time elapsed."
  (cl-with-gensyms (start)
    `(let ((,start (float-time)))
       ,(macroexp-progn forms)
       (/ (fround (* (- (float-time) ,start) 100)) 100.0))))

(cl-defmacro require! (feature)
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
           (push `(require! ,feature) forms)))
       (macroexp-progn (reverse forms))))
    ((pred symbolp)
     (let (forms)
       (setq forms `((require ',feature)))
       (setq forms (let ((err (gensym "error")))
                     `((if oo-init-noerrors-p
                           (condition-case ,err
                               ,(macroexp-progn forms)
                             (error
                              (oo-log 'error "requiring %S: %s -> %s." ',feature (car ,err) (cdr ,err))))
                         ,(macroexp-progn forms)))))
       (setq forms `((if oo-init-profile-p
                         (aprog1! (time-elapsed! ,(macroexp-progn forms))
                           (oo-log 'info "Required %s in %.2f seconds" ',feature it)
                           (push (list ',feature it) oo-init-data))
                       ,(macroexp-progn forms))))
       ;; Ensure main forms are not evaluated more than once.
       (setq forms `((unless (featurep ',feature)
                       ,(macroexp-progn forms))))
       (when (string-match-p "macros$" (symbol-name feature))
         (setq forms `((eval-when-compile ,(macroexp-progn forms)))))
       (macroexp-progn forms)))
    (_
     (signal 'wrong-type-argument `(or stringp symbolp ,feature)))))
;;; provide
(provide '003-base-loader)
;;; 003-base-loader.el ends here
