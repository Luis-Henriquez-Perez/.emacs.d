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
(require '001-init-log)

(defvar oo-init-data nil
  "A record of data concerning loaded files.

This is an alist where each element is of the form (feature start-time end-time
error).  Feature is an init feature, start and end time, and error is any
error.  If an error occurs start and end are nil.  Conversely, if start and end
are non-nil, then error is nil.")

(defvar oo-load-start-time nil
  "Time just before files in the lisp directory are loaded.")

(defvar oo-load-end-time nil
  "Time just after files in the lisp directory are loaded.")

(defvar oo-startup-end-time nil
  "Time after `emacs-startup-hook' is done.")

(defun oo--record-init-end-time-h ()
  "Record the end of `emacs-startup-hook'."
  (setq oo-startup-end-time (current-time)))

(add-hook 'emacs-startup-hook 'oo--record-init-end-time-h 100)

(defmacro require! (feature path)
  "Require FEATURE from PATH, logging any errors.

If FEATURE is successfully loaded, return the elapsed time in seconds
as a floating-point number with two decimal places. Otherwise, log an
error message without interrupting execution."
  `(let ((start (current-time)))
     (condition-case err
         (let (end time-elapsed)
           (require ',feature ,path)
           (setq end (current-time))
           (setq time-elapsed (float-time (time-subtract end start)))
           (setq time-elapsed (/ (fround (* time-elapsed 100)) 100.0))
           (oo-log 'info "Required %s in %.2f seconds" ',feature time-elapsed)
           (push (list ',feature start end nil) oo-init-data))
       (error (oo-log 'error "Error requiring '%s: %s" ',feature err)
              (push (list ',feature nil nil err) oo-init-data)))))

(defun oo--init-log-format-fn (type message meta)
  "Format function for startup."
  (let ((time (float-time (time-subtract (current-time) oo-load-start-time))))
    (setq time (/ (fround (* time 100)) 100.0))
    (format "[%s] %.2f %s" (upcase (symbol-name type)) time (apply #'format message meta))))

(defmacro load! (dir)
  "Load numbered Emacs Lisp files from DIR in lexicographical order.

A file is \"numbered\" if it is prefixed by three digits ranging from 010 to 899
inclusive (e.g., '810-foo.el').  The files are loaded with `require!'."
  (let (forms feature)
    (setq dir (expand-file-name dir user-emacs-directory))
    (dolist (path (directory-files dir t "^[0-8][1-9][[:digit:]]-.+\\.el$"))
      (setq feature (intern (file-name-sans-extension (file-name-nondirectory (directory-file-name path)))))
      (push `(require! ,feature ,path) forms))
    `(let (total-time)
       (let ((oo-log-format-fn #'oo--init-log-format-fn))
         (setq oo-load-start-time (current-time))
         ,@(nreverse forms)
         (setq oo-load-end-time (current-time))
         (setq total-time (float-time (time-subtract oo-load-end-time
                                                     oo-load-start-time))))
       (setq total-time (/ (fround (* total-time 100)) 100.0))
       (oo-log 'info "Finished loading files in %.2f seconds." total-time))))
;;; provide
(provide '002-init-loader)
;;; 002-init-loader.el ends here
