;;; init-core-log.el --- A simple log function -*- lexical-binding: t; -*-
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
(defvar o-debug-p (or (getenv "DEBUG") init-file-debug)
  "When non-nil print debug messages.
The --debug-init flag and setting the DEBUG envar will enable this at startup.")(defvar o-log-buffer "*log*"
  "Name of the log buffer.")

(defvar o-log-buffer-max 500
  "Maximum number of lines in log buffer.")

(defvar o-log-format-fn #'o--default-log-formatter
  "Function that formats the log messsage.")

(defvar o-log-level-alist '((fatal . 6)
                            (failure . 5)
                            (error . 5)
                            (warn  . 4)
                            (success . 3)
                            (info  . 3)
                            (debug . 2)
                            (trace . 1))
  "Alist of log level value.")

;; Define log icons for success and failure
(defvar o-log-icons '((success . "🟢")  ; Green Circle for success
                      (failure . "🔴")  ; Red Circle for failure
                      (error   . "❌")  ; Red Cross for error
                      (warn    . "🟠")  ; Orange Circle for warnings
                      (info    . "🔵")  ; Blue Circle for info
                      (debug   . "🟣")  ; Purple Circle for debug
                      (trace   . "⚪")) ; White Circle for trace
  "Alist of icons to display based on the log level.")

(defvar o-log-level 3
  "Current log level.")

(defun o--default-log-formatter (type message meta)
  (let* ((icon (alist-get type o-log-icons))
         (indicator (or icon (format "[%s]" (upcase (symbol-name type))))))
    (format "%s %s" indicator (apply #'format message meta))))

(defun o-startup-format-fn (start-time type message meta)
  (let ((time (float-time (time-subtract (current-time) start-time))))
    (setq time (/ (fround (* time 100)) 100.0))
    (format "[%s] %.2f %s" (upcase (symbol-name type)) time (apply #'format message meta))))

(defun o-log (level message &rest meta)
  "Log a formatted MESSAGE of a given TYPE to the `o-log-buffer`.

Append a log entry to the buffer specified by `o-log-buffer`.
If the last log entry in the buffer matches the new message, it increments a
repeat count at the end of the line displayed instead of creating a new entry.
The count is displayed as ‘(N)’ where N is the number of times the message was
logged."
  (when (>= (alist-get level o-log-level-alist) o-log-level)
    (let* ((buffer (get-buffer-create o-log-buffer))
           (output (funcall o-log-format-fn level message meta))
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
            (setq excess (- (line-number-at-pos (point-max)) (1+ o-log-buffer-max)))
            (when (> excess 0)
              (goto-char (point-min))
              (dotimes (_ excess) (delete-line)))))))))
;;; provide
(provide 'init-core-log)
;;; init-core-log.el ends here
