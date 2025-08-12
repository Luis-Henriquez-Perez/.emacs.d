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

;; For some reason `eval-when-compile' is evaluated during macroexpansion.  So I
;; will just leave it to the files themselves to handle the macros.
(cl-defmacro require! (&key (from 0) (to most-positive-fixnum) profile)
  "Load numbered Emacs Lisp files from DIR in lexicographical order.

A file is \"numbered\" if it is prefixed by three digits ranging from 010 to 899
inclusive (e.g., '810-foo.el').  The files are loaded with `require!'."
  (cl-flet* ((time-elapsed-form (form)
               (let ((start (make-symbol "start")))
                 `(let ((,start (float-time)))
                    ,form
                    (- (float-time) ,start))))
             (profile-form (feature form)
               (let ((time-elapsed (gensym "time-elapsed")))
                 `(let ((,time-elapsed ,(time-elapsed-form form)))
                    (setq ,time-elapsed (/ (fround (* ,time-elapsed 100)) 100.0))
                    (oo-log 'info "Required %s in %.2f seconds" ',feature ,time-elapsed)
                    (push (list ',feature ,time-elapsed) (get-register :init-data)))))
             (check-errors-form (feature form)
               (let ((err (gensym "error")))
                 `(condition-case ,err
                      ,form
                    (error
                     (oo-log 'error "requiring %S: %s -> %s." ',feature (car ,err) (cdr ,err)))))))
    (let (dir form forms feature number base)
      (setq dir (expand-file-name "lisp/" user-emacs-directory))
      (dolist (path (directory-files dir t "^[0-8][1-9][0-9]-.+\\.el$"))
        (setq base (file-name-sans-extension (file-name-nondirectory (directory-file-name path))))
        (setq feature (intern base))
        (string-match "\\`\\(?1:[0-8][1-9][0-9]\\)-.+$" base)
        (setq number (string-to-number (match-string 1 base)))
        (setq form `(require ',feature))
        (when (and (> number from) (< number to))
          (unless oo-debug-p (setq form (check-errors-form feature form)))
          (if (string-match-p "macros$" base)
              (setq form `(eval-when-compile ,form))
            (when profile
              (setq form (profile-form feature form))))
          (push form forms)))
      (if profile
          `(let ((oo-log-format-fn (apply-partially #'oo-startup-format-fn (float-time))))
             (oo-log 'info "Finished loading features in %.2f seconds." ,(time-elapsed-form (macroexp-progn (nreverse forms)))))
        (macroexp-progn (nreverse forms))))))
;;; provide
(provide '002-init-loader)
;;; 002-init-loader.el ends here
