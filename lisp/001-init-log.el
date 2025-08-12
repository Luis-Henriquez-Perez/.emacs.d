;;; 001-init-log.el --- A simple log function -*- lexical-binding: t; -*-
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
;; Provide a simple log function.
;;
;;; Code:
(defvar oo-debug-p (or (getenv "DEBUG") init-file-debug)
  "When non-nil print debug messages.
The --debug-init flag and setting the DEBUG envar will enable this at startup.")(defvar oo-log-buffer "*log*"
  "Name of the log buffer.")

(defvar oo-log-buffer-max 500
  "Maximum number of lines in log buffer.")

(defvar oo-log-format-fn #'oo--default-log-formatter
  "Function that formats the log messsage.")

(defvar oo-log-level-alist '((fatal . 6)
                             (error . 5)
                             (warn  . 4)
                             (info  . 3)
                             (debug . 2)
                             (trace . 1))
  "Alist of log level value.")

(defvar oo-log-level 3
  "Current log level.")

(defun oo--default-log-formatter (type message meta)
  (format "[%s] %s" (upcase (symbol-name type)) (apply #'format message meta)))

(defun oo-startup-format-fn (start-time type message meta)
  (let ((time (float-time (time-subtract (current-time) start-time))))
    (setq time (/ (fround (* time 100)) 100.0))
    (format "[%s] %.2f %s" (upcase (symbol-name type)) time (apply #'format message meta))))

(defun oo-log (level message &rest meta)
  "Log a formatted MESSAGE of a given TYPE to the `oo-log-buffer`.

Append a log entry to the buffer specified by `oo-log-buffer`.
If the last log entry in the buffer matches the new message, it increments a
repeat count at the end of the line displayed instead of creating a new entry.
The count is displayed as ‘(N)’ where N is the number of times the message was
logged."
  (when (>= (alist-get level oo-log-level-alist) oo-log-level)
    (let* ((buffer (get-buffer-create oo-log-buffer))
           (output (funcall oo-log-format-fn level message meta))
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
              (dotimes (_ excess) (delete-line)))))))))
;;; provide
(provide '001-init-log)
;;; 001-init-log.el ends here
