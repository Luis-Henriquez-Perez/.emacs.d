;;; 130-init-meep.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
(require 'mark-thing-at)

(mark-thing-at-make-functions)

(defun oo-dwim-escape ()
  "Exit out of whatever is happening after escape.
Enter normal state.  If in minibuffer, exit the minibuffer.  When in a
non-readonly file buffer, save the buffer."
  (interactive)
  (when (bound-and-true-p evil-mode)
    (evil-normal-state 1))
  (cond ((minibuffer-window-active-p (minibuffer-window))
		 (if (or defining-kbd-macro executing-kbd-macro)
			 (minibuffer-keyboard-quit)
           (abort-recursive-edit)))
		((or defining-kbd-macro executing-kbd-macro) nil)
        (t
         (when (and (not buffer-read-only)
                    (buffer-file-name)
                    (buffer-modified-p))
           (save-buffer))
		 (keyboard-quit))))

;; (defun oo-pulse-region-a (orig-fn &rest args)
;;   "Highlight the region."
;;   (require 'pulse nil t)
;;   (pulse-momentary-highlight-region (mark-marker) (point) 'mode-line)
;;   (apply orig-fn args))

;; (advice-add 'meep-insert-change :around #'oo-pulse-region-a)
(defvar-keymap meep-state-keymap-motion
  "+" #'text-scale-increase
  "-" #'text-scale-decrease

  "1" #'meep-digit-argument-repeat
  "2" #'meep-digit-argument-repeat
  "3" #'meep-digit-argument-repeat
  "4" #'meep-digit-argument-repeat
  "5" #'meep-digit-argument-repeat
  "6" #'meep-digit-argument-repeat
  "7" #'meep-digit-argument-repeat
  "8" #'meep-digit-argument-repeat
  "9" #'meep-digit-argument-repeat
  "0" #'meep-digit-argument-repeat

  "q" #'meep-register-kmacro-start-or-end
  "Q" #'repeat-fu-execute

  "w" #'meep-move-word-next
  "W" #'meep-move-symbol-next

  "e" #'meep-move-word-next-end
  "E" #'meep-move-symbol-next-end

  "r" #'meep-clipboard-killring-yank
  "R" #'meep-clipboard-only-yank

  "t" #'meep-move-matching-syntax-inner
  "T" #'meep-move-matching-syntax-outer

  "a a" #'meep-insert
  "a h" #'meep-insert-line-beginning
  "a s" #'meep-insert-append
  "a l" #'meep-insert-line-end
  "a k" #'meep-insert-open-above
  "a j" #'meep-insert-open-below

  "s w" #'mark-word
  "s a" #'mark-word
  "s l" #'mark-line
  "s n" #'mark-line-this
  "s h" #'mark-line-this
  "s o" #'mark-symbol
  "s d" #'mark-defun
  "s j" #'mark-symbol
  "s s" #'mark-sentence
  "s p" #'mark-paragraph
  "s k" #'mark-list
  "s t" #'mark-whitespace

  "d" #'meep-clipboard-only-cut
  "D" #'meep-clipboard-killring-cut

  "f" #'meep-insert-change
  "F" #'meep-insert-change-lines

  "g" #'meep-exchange-point-and-mark
  "G" #'meep-exchange-point-and-mark-motion

  ;; Right Hand: Row 2.
  "h" #'meep-move-char-prev
  "H" #'meep-move-line-beginning

  "j" #'meep-move-line-next
  "J" #'undo-only

  "k" #'meep-move-line-prev
  "K" #'undo-redo

  "l" #'meep-move-char-next
  "L" #'meep-move-line-end

  ";" #'execute-extended-command
  ":" #'meep-move-matching-bracket-outer

  "y" #'meep-clipboard-killring-copy
  "Y" #'meep-clipboard-only-copy

  "u" #'meep-clipboard-killring-yank
  "U" #'meep-clipboard-only-yank

  "i" #'meep-clipboard-killring-cut
  "I" #'meep-clipboard-only-cut

  "z" #'meep-region-toggle
  "Z" #'meep-clipboard-killring-copy

  "x" #'meep-move-matching-syntax-inner
  "X" #'meep-clipboard-killring-copy

  "c" #'meep-move-matching-syntax-inner
  "C" #'meep-clipboard-killring-copy

  "v" #'meep-region-toggle
  "V" #'meep-move-matching-syntax-outer

  "b" #'meep-insert-change
  "B" #'meep-insert-change-lines

  "o" #'meep-insert-open-below
  "O" #'meep-insert-open-above

  "n" #'meep-isearch-at-point-next
  "N" #'meep-isearch-at-point-prev

  "m" #'meep-isearch-at-point-next
  "M" #'meep-isearch-at-point-prev

  "<escape>" #'oo-dwim-escape
  oo-normal-leader-key #'oo-leader-map)

(defun my-key-free ()
  (interactive)
  (let ((keys (this-command-keys-vector)))
    (message "Key Free: %s" (format-kbd-macro keys))))

(defun my-meep-basis-keys ()
  (defvar-keymap meep-clipboard-register-map
    "e" #'meep-clipboard-register-cut
    "r" #'meep-clipboard-register-yank
    "t" #'meep-clipboard-register-copy))

(defun meep-mark-hook-activate ()
  "Activate visual state."
  (when (bray-state-derived-p 'normal)
    (bray-state-stack-push 'visual)))

(defun meep-mark-hook-deactivate ()
  "Activate visual state."
  (when (bray-state-derived-p 'visual)
    (bray-state-stack-pop)))

(defun oo-setup-visual-state ()
  (cond
   (bray-mode
    (add-hook 'activate-mark-hook #'meep-mark-hook-activate)
    (add-hook 'deactivate-mark-hook #'meep-mark-hook-deactivate))
   (t
    (remove-hook 'activate-mark-hook #'meep-mark-hook-activate)
    (remove-hook 'deactivate-mark-hook #'meep-mark-hook-deactivate))))

(defun my-meep-setup-once ()
  ;; Extended functions.
  (require 'bray)

  (setq meep-state-insert 'insert)
  (setq bray-state-default 'normal)

  (defvar meep-state-hook-insert-enter nil)
  (defvar meep-state-hook-insert-exit nil)

  (defvar meep-state-hook-normal-enter nil)
  (defvar meep-state-hook-normal-exit nil)

  ;; Visual mode.
  (add-hook 'bray-mode-hook #'oo-setup-visual-state)
  ;; End visual mode support.

  (add-hook 'meep-state-hook-insert-enter (lambda () (set-mark (point)) (deactivate-mark)))

  ;; (set-mark (point))
  ;; Testing this out!
  ;; VIM style '^' register for when we leave insert mode.
  (add-hook 'meep-state-hook-insert-exit (lambda () (deactivate-mark) (let ((reg ?^)) (let ((reg-val (get-register reg))) (cond ((and reg-val (markerp reg-val)) (set-marker reg-val (point) (current-buffer))) (t (set-register reg (point-marker))))))))

  (defvar meep-state-keymap-motion (make-keymap))
  (defvar meep-state-keymap-normal (make-keymap))
  (defvar meep-state-keymap-visual (make-keymap))
  (defvar meep-state-keymap-insert (make-keymap))

  ;; Optional, a quick way to mask insertion.
  (define-key meep-state-keymap-motion [remap self-insert-command] 'my-key-free)

  (keymap-set meep-state-keymap-insert "<escape>" #'oo-dwim-escape)

  (setq bray-state-definitions
        (list
         (list
          :id 'normal
          ;; Define.
          :cursor-type 'box
          :lighter "<N>"
          :keymaps (list (cons t 'meep-state-keymap-motion) (cons t 'meep-state-keymap-normal))

          :enter-hook 'meep-state-hook-normal-enter
          :exit-hook 'meep-state-hook-normal-exit)

         (list
          :id 'visual
          ;; Define.
          :cursor-type 'hollow
          :lighter "<V>"
          :keymaps (list (cons t 'meep-state-keymap-motion) (cons t 'meep-state-keymap-visual))

          :enter-hook 'meep-state-hook-visual-enter
          :exit-hook 'meep-state-hook-visual-exit)

         (list
          :id 'insert
          ;; Define.
          :cursor-type 'bar
          :lighter "<I>"
          :keymaps (list (cons t 'meep-state-keymap-insert))

          :enter-hook 'meep-state-hook-insert-enter
          :exit-hook 'meep-state-hook-insert-exit

          ;; Optional.
          :is-input t)))

  (add-hook
   'after-change-major-mode-hook
   (lambda ()
     ;; Enable it in the minibuffer.
     (when (not (derived-mode-p 'special-mode))
       (bray-mode)
       (when (minibufferp)
         (bray-state-set 'insert)))))

  (dolist (buffer (buffer-list))
    (with-current-buffer buffer (bray-mode 1))))

(add-hook 'emacs-startup-hook #'my-meep-setup-once 80)
;;; provide
(provide '130-init-meep)
;;; 130-init-meep.el ends here
