;;; init-fn-init-core.el -*- lexical-binding: t; -*-
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
;; This file contains utility functions.
;;
;;; Code:
(require 'pcase)
(eval-when-compile (require 'init-mac-base))

(defsubst o-hundredths (n)
  "Return N rounded to the nearest hundredth."
  (/ (fround (* n 100)) 100.0))

;; I don't yet know where to put this function.  So for now, here it goes.
(defun o-popup-at-bottom (regexp)
  "Open buffers at bottom that match regexp."
  (push `(,regexp
          (display-buffer-at-bottom)
          (side bottom)
          (slot 1)
          (window-height 0.5)
          (window-parameters ((no-other-window t))))
        display-buffer-alist))

(defun o-eval-and-replace-region (beg end)
  "Evaluate the region between BEG and END as Elisp, and replace it with the result.
If there's an error during evaluation, restore the original region and display the error message."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties beg end))
         (result (condition-case err
                     (eval (read text))
                   (error (progn
                            (message "Eval error: %s" (error-message-string err))
                            nil)))))
    (when result
      (delete-region beg end)
      (prin1 result (current-buffer)))))

(defun o-in-string-or-comment-p ()
  "Return non-nil if point is in a string or comment.
Specifically, return the symbol `string' if point is in a string, the symbol
`comment' if in a comment and nil otherwise."
  (declare (side-effect-free error-free))
  (let ((ppss (syntax-ppss)))
    (cond ((nth 3 ppss) 'string)
          ((nth 4 ppss) 'comment)
          (t nil))))

(defun o-advice--silence-output (fn &rest args)
  "Call FN with ARGS without producing any output."
  (o-quiet (apply fn args)))
;;; provide
(provide 'init-fn-base)
;;; init-fn-init-core.el ends here
