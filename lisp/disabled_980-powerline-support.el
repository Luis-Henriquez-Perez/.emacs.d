;;; 980-powerline-support.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; TODO: add commentary
;;
;;; Code:
(require 'powerline)

;; Resetting the modeline after a theme change ensures separator colors are
;; updated to match the current theme.
(hook! enable-theme-functions powerline-reset :ignore-args t)

(setq powerline-default-separator 'zigzag)

;; A height of 40 also looks good, but I am fine with 33.  By the way, for this
;; to take effect you need to call `powerline-reset' after setting it.
(setq powerline-height 33)

(defface oo-mode-line-segment-1 '((t (:background "white" :foreground "black")))
  "Face for the first modeline segment.")

(defun! oo-mode-line-component (name)
  "Return string representation of component named NAME.
If an error is raised from component function."
  (condition-case err
      (set! return-value (funcall (intern (format "oo-mode-line-segment--%s" name))))
    (error
     (oo-log 'error "MODELINE %s : %s -> %s" name (car err) (cdr err))
     (return! "")))
  (pcase return-value
    ('nil
     "")
    ((pred stringp)
     return-value)
    (_
     (error "Modeline component %s returned a non-string %s." name return-value)))
  return-value)

(defun oo-mode-line-left-separator (&optional right-p)
  "Return the left separator."
  (intern (format "powerline-%s-%s"
                  (powerline-current-separator)
                  (funcall (if right-p #'cdr #'car)
                           powerline-default-separator-dir))))

(defun oo-mode-line-right-separator ()
  "Return the right separator."
  (oo-mode-line-left-separator 'right))

(defun! oo--modeline-render-lhs (segment-names faces)
  "Render the left-hand side of the modeline."
  (set! sep (or sep (oo-mode-line-left-separator)))
  (set! prev-face (pop faces))
  (alet2! (length segment-names) (length faces)
    (when (> it other)
      (set! faces (cons (car faces) (seq-take (oo-cycle (cdr faces)) (1- it))))))
  (for! (reverse (name . face) (oo-zip-pair segment-names faces))
    (set! segment (funcall (intern (format "oo-mode-line-segment--%s" name))))
    (when (and (stringp segment) (not (string-empty-p segment)))
      (pushing! lhs (funcall sep face prev-face))
      (add-face-text-property 0 (length segment) face t segment)
      (pushing! lhs segment)
      (set! prev-face face)))
  lhs)

(defun! oo--modeline-render-rhs (segment-names faces)
  "Render the right-hand side of the modeline."
  (set! sep (oo-mode-line-right-separator))
  (set! prev-face (pop faces))
  (alet2! (length segment-names) (length faces)
    (when (> it other)
      (set! faces (cons (car faces) (seq-take (oo-cycle (cdr faces)) (1- it))))))
  (for! (reverse (name . face) (oo-zip-pair (reverse segment-names) faces))
    (set! segment (funcall (intern (format "oo-mode-line-segment--%s" name))))
    (when (and (stringp segment) (not (string-empty-p segment)))
      (collecting! rhs (funcall sep prev-face face))
      (add-face-text-property 0 (length segment) face t segment)
      (collecting! rhs segment)
      (set! prev-face face)))
  rhs)

(defun oo-mode-line-increment-height ()
  "Update the mode line in all buffers to reflect the default `mode-line-format'."
  (interactive)
  (cl-incf powerline-height 2)
  (powerline-reset))

(defun oo-mode-line-decrement-height ()
  "Update the mode line in all buffers to reflect the default `mode-line-format'."
  (interactive)
  (cl-incf powerline-height 2)
  (powerline-reset))

(defun! oo-mode-line-cycle-separators (select-p)
  "Cycle through available powerline separators.
With prefix argument, SELECT-P, select one explicitly."
  (interactive "P")
  (set! separators '(alternate
                     arrow
                     arrow-fade
                     bar
                     box
                     brace
                     butt
                     chamfer
                     contour
                     curve
                     rounded
                     roundstub
                     wave
                     zigzag
                     slant
                     utf-8))
  (if select-p
      (awhen! (completing-read "Choose separator: " separators)
        (setq powerline-default-separator (seq-random-elt it))
        (powerline-reset))
    (setq powerline-default-separator (seq-random-elt separators))
    (powerline-reset)))

(defun! oo-mode-line-main ()
  "Return my main modeline."
  (set! active (powerline-selected-window-active))
  (set! face1 (if active 'oo-mode-line-segment-1 'powerline-inactive1))
  (set! face2 (if active 'mode-line 'mode-line-inactive))
  (set! face3 (if active 'powerline-active2 'powerline-inactive2))
  (set! fill-face (if active 'powerline-active0 'powerline-inactive0))
  (set! evil-face (+evil-state-face))
  (set! buffer-info '(narrow read-only kbd-macro buffer-modified buffer-name))
  (oo-mode-line-render `(evil-state ,buffer-info version-control)
                       '(text-scale clocked-in pomodoro battery (line-number percentage-of-buffer) current-time)
                       `(,fill-face ,evil-face ,face1 ,face2 ,face3)))
;;; provide
(provide '980-powerline-support)
;;; 980-powerline-support.el ends here
