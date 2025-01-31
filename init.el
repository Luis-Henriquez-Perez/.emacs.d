;;; init.el --- My emacs configuration -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Free Software Foundation, Inc.
;;
;; Author: Luis Henriquez-Perez <luis@luishp.xyz>
;; Maintainer: Luis Henriquez-Perez <luis@luishp.xyz>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
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
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This is my personal emacs configuration.  Please refer to the
;; README for information on how to run and modify them.
;;
;;; Code:
;;;; disable garbage collection until after startup
;; https://medium.com/@danielorihuelarodriguez/optimize-emacs-start-up-time-ae314201e04f
;; https://news.ycombinator.com/item?id=39127859
;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.8)
;;;; don't search for whenever a package is loaded
;; Credits to irreal for sharing that keywords can be used as registers in his
;; blog post (https://irreal.org/blog/?p=12386).
(set-register :file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
;;;; prevent flashing of unstyled modeline
;; Don't render the modeline on startup.  For one thing, the startup looks
;; better without flashing stuff on the screen.  Additionally, the more that's
;; saved on rendering, the faster the startup.
(set-register :mode-line-format mode-line-format)
(setq-default mode-line-format nil)
;;;; logging
(defvar oo-start (current-time))

(defvar oo-log-buffer "*log*"
  "Name of the log buffer.")

;; This is important because it prevents the buffer from growing indefinitely and causing performance problems.
(defvar oo-log-buffer-max 500
  "Maximum number of lines in log buffer.")

(defun oo-log (type message &rest meta)
  "Log a formatted MESSAGE of a given TYPE to the `oo-log-buffer`.

Append a log entry to the buffer specified by `oo-log-buffer`.
If the last log entry in the buffer matches the new message, it increments a
repeat count at the end of the line displayed instead of creating a new entry.
The count is displayed as '(N)' where N is the number of times the message was
logged."
  (let* ((log (apply #'format message meta))
         (timestamp (float-time (time-subtract (current-time) oo-start)))
         (buffer (get-buffer-create oo-log-buffer))
         (output (format "[%s] %.3f %s" (upcase (symbol-name type)) timestamp log))
         (excess nil))
    (with-current-buffer buffer
      (unless view-mode (view-mode t))
      ;; Add to buffer without constantly moving focus to the end.
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (or (save-excursion
                (and (zerop (forward-line -1))
                     (looking-at (rx bol (literal output)))
                     (goto-char (match-end 0))
                     (or (and (looking-at (rx space "(" space (group (1+ digit)) space ")" eol))
                              (let ((it (match-string 1)))
                                (replace-match (number-to-string (+ 1 (string-to-number it))) nil nil nil 1)
                                t))
                         (progn (insert " ( 2 )") t))))
              (progn (insert output)
                     (insert "\n")))
          (setq excess (- (line-number-at-pos (point-max)) (1+ oo-log-buffer-max)))
          (when (> excess 0)
            (goto-char (point-min))
            (dotimes (_ excess) (delete-line))))))))
;;;; loader
(defvar oo-init-data nil)

;; (defmacro time-elapsed! (&rest body)
;;   `(let ((start (current-time))
;;          (time nil))
;;      (condition-case err
;;          (prog1 (progn ,@body)
;;            (setq time (string-to-number (format "%.2f" (float-time (time-subtract (current-time) start)))))
;;            (message "Form %s in %f seconds" (string-truncate-left (format "%S" ',body) 50) time))
;;        (error
;;         (message "Error evaluating %S: %s" ',body err)))))

;; Sort init info by the amount of time taken.  Then put the longest times on
;; top.  Compute the total and place the percentages.
(defmacro require! (feature &optional path)
  "Catch any errors, record and log the time taken to require FEATURE."
  `(let ((start (current-time))
         (feature ',feature)
         (time nil))
     (condition-case err
         (progn
           (require ',feature ,path)
           (setq time (string-to-number (format "%.2f" (float-time (time-subtract (current-time) start)))))
           (oo-log 'info "Required '%s in %.2f seconds" feature time))
       (error
        (oo-log 'error "Error requiring '%s: %s" feature err)))
     (push (list feature time) oo-init-data)))

(defmacro load! (dir)
  (let (forms feature)
    (setq dir (expand-file-name dir user-emacs-directory))
    (dolist (path (directory-files dir t "^[0-8][[:digit:]][[:digit:]].+\\.el$"))
      (setq feature (intern (file-name-sans-extension (file-name-nondirectory (directory-file-name path)))))
      ;; It is a bit faster if you specify the path because then emacs does not have to look through the directory.
      (push `(require! ,feature ,path) forms))
    `(progn ,@(nreverse forms))))
;;;; rest
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

;; (eval-when-compile (require 'init-loader "./init-loader.el"))

(load! "lisp/")
;;; provide init
(provide 'init)
;;; init.el ends here
