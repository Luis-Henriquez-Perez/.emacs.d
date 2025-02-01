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
;;; provide
(provide '001-init-log)
;;; 001-init-log.el ends here
