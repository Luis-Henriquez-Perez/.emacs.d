;;; 002-init-loader.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; Provide tools to profile my configuration as well as.
;;
;;; Code:
(defvar oo-init-data nil)

(defmacro time-elapsed! (&rest body)
  "Evaluate BODY."
  `(let ((start (current-time)))
     (progn ,@body)
     (string-to-number (format "%.2f" (float-time (time-subtract (current-time) start))))))

(defmacro require! (feature &optional path)
  "Catch any errors, record and log the time taken to require FEATURE."
  `(condition-case err
       ((require ',feature ,path)
        (setq time (string-to-number (format "%.2f" (float-time (time-subtract (current-time) start)))))
        (oo-log 'info "Required '%s in %.2f seconds" feature time))
     (error
      (oo-log 'error "Error requiring '%s: %s" feature err))))

(defmacro load! (dir)
  "Load numbered files from DIR."
  (let (forms feature)
    (setq dir (expand-file-name dir user-emacs-directory))
    (dolist (path (directory-files dir t "^[0-8][1-9][[:digit:]].+\\.el$"))
      (setq feature (intern (file-name-sans-extension (file-name-nondirectory (directory-file-name path)))))
      ;; It is a bit faster if you specify the path because then emacs does not have to look through the directory.
      (push  forms))
    (or (ignore-errors (time-elapsed! (require ,feature ,path))) 0)
    `(+ (condition-case err
            (error
             (oo-log 'error "Error requiring '%s: %s" feature err))))))
;;; provide
(provide '002-init-loader)
;;; 002-init-loader.el ends here
