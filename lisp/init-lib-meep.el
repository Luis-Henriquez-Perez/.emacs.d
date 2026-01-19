;;; init-lib-meep.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; Extension for meep.
;;
(defun o--what-face (&optional pos)
  "Show all faces at point."
  (let* ((pos (or pos (point)))
         (face (get-text-property pos 'face)))
    (unless (keywordp (car-safe face)) (list face))))

(defun o--whitespacep (c)
  "Return non-nil if c is white spaces, nil otherwise."
  (= 32 (char-syntax c)))

(defun o--syntax-bounds (&optional inclusive)
  (let ((point-face (o--what-face))
        (backward-point (point)) ; last char when stop, including white space
        (backward-none-space-point (point)) ; last none white space char
        (forward-point (point)) ; last char when stop, including white space
        (forward-none-space-point (point)) ; last none white space char
        (start (point))
        (end (point)))

    ;; check chars backward,
    ;; stop when char is not white space and has different face
    (save-excursion
      (let ((continue t))
        (while (and continue (>= (- (point) 1) (point-min)))
          (backward-char)
          (let ((backward-point-face (o--what-face)))
            (if (o--whitespacep (char-after))
                (setq backward-point (point))
              (if (equal point-face backward-point-face)
                  (progn (setq backward-point (point))
                         (setq backward-none-space-point (point)))
                (setq continue nil)))))))

    ;; check chars forward,
    ;; stop when char is not white space and has different face
    (save-excursion
      (let ((continue t))
        (while (and continue (< (+ (point) 1) (point-max)))
          (forward-char)
          (let ((forward-point-face (o--what-face)))
            (if (o--whitespacep (char-after))
                (setq forward-point (point))
              (if (equal point-face forward-point-face)
                  (progn (setq forward-point (point))
                         (setq forward-none-space-point (point)))
                (setq continue nil)))))))

    (if inclusive
        ;; for outer object,
        ;; if both leading and trailing white spaces exist,
        ;; only trailing whitespaces are included.
        ;; otherwise, leading/trailing/none white spaces are included.
        (progn
          (if (and (/= backward-none-space-point backward-point)
                   (/= forward-none-space-point forward-point))
              (setq start backward-none-space-point)
            (setq start backward-point))
          (setq end forward-point))
      ;; for inner object,
      ;; no leading and trailing white spaces are included
      (setq start backward-none-space-point)
      (setq end forward-none-space-point))

    (cons start end)))

(defun meep-region-mark-syntax ()
  (interactive)
  (let ((bounds (o--syntax-bounds)))
    (cond (bounds
           (meep--region-mark-bounds-to-region bounds t))
          (t
           (message "No syntax bounds.")))))
;;; provide
(provide 'init-lib-meep)
;;; init-lib-meep.el ends here
