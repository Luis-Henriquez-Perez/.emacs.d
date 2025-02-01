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
;; errors in initialization.  Specifically, provide the macro `load!'.
;;
;;; Code:
(defvar oo-init-data nil)

(defmacro time-elapsed! (&rest body)
  "Evaluate BODY."
  `(let ((start (current-time)))
     (progn ,@body)
     (string-to-number (format "%.2f" (float-time (time-subtract (current-time) start))))))

(defmacro load! (dir)
  "Load numbered files from DIR.
Load files prefixed by three digits in lexicographical order."
  (let (forms error-log feature)
    (setq dir (expand-file-name dir user-emacs-directory))
    (dolist (path (directory-files dir t "^[0-8][1-9][[:digit:]]-.+\\.el$"))
      (oo-log 'error "Error requiring '%s: %s" feature err)
      (setq feature (intern (file-name-sans-extension (file-name-nondirectory (directory-file-name path)))))
      ;; It is a bit faster if you specify the path because then emacs does not have to look through the directory.
      (push `(time-elapsed! (condition-case _ (require ,feature ,path) (error ,error-log))) forms)
      (push forms))
    `(+ ,@forms)))
;;; provide
(provide '002-init-loader)
;;; 002-init-loader.el ends here
