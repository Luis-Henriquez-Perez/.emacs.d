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
(defun oo-evil-dwim-escape ()
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

(defvar-keymap meep-state-keymap-motion
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
  ;; "-" #'meep-digit-argument-repeat

  "`" #'meep-region-to-secondary-selection
  "~" #'meep-region-swap
  ;; ----
  ;; Left Hand: Row 1.
  ;; ----

  "q" #'repeat-fu-execute
  "Q" #'my-key-free

  "w" #'meep-move-symbol-next
  "W" #'meep-move-symbol-prev
  ;; "w" #'meep-clipboard-register-actions
  ;; "W" #'my-key-free

  "e" #'meep-clipboard-killring-cut
  "E" #'meep-clipboard-only-cut

  "r" #'meep-clipboard-killring-yank
  "R" #'meep-clipboard-only-yank

  "t" #'meep-clipboard-killring-copy
  "T" #'meep-clipboard-only-copy

  ;; Left Hand: Row 2.

  ;; NOTE: a more comprehensive surround map is really needed.
  ;; This is only character level surround insertion.
  "a" #'meep-char-surround-insert
  "A" #'meep-char-surround-insert-lines

  "s s" #'meep-insert-at-last
  "s d" #'rectangle-mark-mode

  "s j" #'meep-insert-open-below
  "s k" #'meep-insert-open-above
  "s l" #'meep-insert-line-end
  "s h" #'meep-insert-line-beginning

  "s e" #'oo-eval-and-replace-region
  "s m" #'downcase-region
  "s ," #'upcase-region

  "s <return>" #'fill-region
  "s <end>" #'end-of-buffer
  "s <home>" #'beginning-of-buffer

  ;; Run commands "after" numeric has been set.
  ;; Unlike the default to re-running N times.
  "s 1" #'digit-argument
  "s 2" #'digit-argument
  "s 3" #'digit-argument
  "s 4" #'digit-argument
  "s 5" #'digit-argument
  "s 6" #'digit-argument
  "s 7" #'digit-argument
  "s 8" #'digit-argument
  "s 9" #'digit-argument
  "s 0" #'digit-argument
  "s -" #'negative-argument

  "d" #'meep-region-toggle
  "D" #'meep-region-expand-to-line-bounds

  "f h" #'meep-move-find-char-on-line-at-prev
  "f H" #'meep-move-find-char-on-line-till-prev
  "f j" #'meep-isearch-regexp-next
  "f k" #'meep-isearch-regexp-prev
  "f l" #'meep-move-find-char-on-line-at-next
  "f L" #'meep-move-find-char-on-line-till-next

  ;; Find "repeat" are below the keys for find.
  "f ." #'meep-move-find-char-on-line-repeat-at-next
  "f n" #'meep-move-find-char-on-line-repeat-at-prev
  "f >" #'meep-move-find-char-on-line-repeat-till-next
  "f N" #'meep-move-find-char-on-line-repeat-till-prev

  "f m" #'meep-isearch-at-point-prev
  "f ," #'meep-isearch-at-point-next

  "f u" #'avy-goto-symbol-1-below
  "f i" #'avy-goto-symbol-1-above

  ;; Alternative to VIM's ":" to go to line numbers (frees up a key).
  "f ;" #'goto-line
  "f :" #'goto-char

  "F" #'my-key-free

  "g" #'meep-char-replace
  "G" #'meep-char-insert

  ;; Left Hand: Row 3.
  "z" #'undo-only
  "Z" #'undo-redo

  "x" #'meep-insert
  "X" #'meep-insert-overwrite

  "c" #'meep-delete-char-ring-next
  "C" #'meep-delete-char-ring-prev

  "v" #'meep-delete-char-ring-yank
  "V" #'meep-clipboard-killring-cut-line ; Odd-one out, locate for convenience.

  "b" #'meep-insert-change
  "B" #'meep-insert-change-lines

  ;; Right Hand: Row 1.
  "y" #'meep-move-line-non-space-beginning
  "Y" #'meep-move-by-sexp-any-prev

  "u" #'meep-exchange-point-and-mark
  "U" #'meep-move-by-sexp-over-next

  "i" #'meep-exchange-point-and-mark-motion
  "I" #'meep-move-by-sexp-over-prev

  "o" #'meep-move-line-non-space-end
  "O" #'meep-move-by-sexp-any-next

  "p" #'meep-keypad
  "P" #'my-key-free

  ;; Right Hand: Row 2.
  "h" #'meep-move-char-prev
  "H" #'meep-move-same-syntax-or-symbol-prev

  "j" #'meep-move-line-next
  "J" #'meep-move-by-sexp-out-next

  "k" #'meep-move-line-prev
  "K" #'meep-move-by-sexp-out-prev

  "l" #'meep-move-char-next
  "L" #'meep-move-same-syntax-or-symbol-next

  ";" #'meep-move-matching-bracket-inner
  ":" #'meep-move-matching-bracket-outer

  "'" #'meep-move-matching-syntax-inner
  "\"" #'meep-move-matching-syntax-outer

  ;; Right Hand: Row 3.
  "n" #'meep-move-symbol-prev
  "N" #'meep-move-same-syntax-and-space-prev

  "m" #'meep-isearch-repeat-next
  "M" #'meep-move-paragraph-next

  "," #'meep-isearch-repeat-prev
  "<" #'meep-move-paragraph-prev

  "." #'meep-move-symbol-next
  ">" #'meep-move-same-syntax-and-space-next

  "/" #'meep-move-symbol-next-end
  "?" #'meep-move-same-syntax-and-space-next-end

  ;; Other keys.
  "\\" #'meep-register-jump-to
  "|" #'meep-register-kmacro-start-or-end

  "[" #'meep-move-to-bounds-of-thing-beginning
  "]" #'meep-move-to-bounds-of-thing-end

  "-" #'meep-region-syntax-contract
  "=" #'meep-region-syntax-expand

  "<tab>" #'meep-indent-rigidly

  "<escape" #'oo-evil-dwim-escape
  "S-<delete>" #'meep-join-line-prev
  "S-<backspace>" #'meep-join-line-next

  "<home>" #'meep-move-line-beginning
  "<end>" #'meep-move-line-end)

(defun my-key-free ()
  (interactive)
  (let ((keys (this-command-keys-vector)))
    (message "Key Free: %s" (format-kbd-macro keys))))

(defun my-meep-basis-keys ()
  ;; (define-key meep-state-keymap-normal nil nil)

  ;; (define-key meep-state-keymap-visual nil nil)

  (define-key meep-state-keymap-insert (kbd "<escape>") #'bray-state-stack-pop)

  (defvar-keymap meep-clipboard-register-map
    "e" #'meep-clipboard-register-cut
    "r" #'meep-clipboard-register-yank
    "t" #'meep-clipboard-register-copy))

(defun my-meep-setup-once ()
  ;; Extended functions.
  (require 'bray)
  (meep-bootstrap-once)

  (setq meep-state-insert 'insert)
  (setq bray-state-default 'normal)

  (defvar meep-state-hook-insert-enter nil)
  (defvar meep-state-hook-insert-exit nil)

  (defvar meep-state-hook-normal-enter nil)
  (defvar meep-state-hook-normal-exit nil)

  ;; Visual mode.
  (defun meep-mark-hook-activate ()
    "Activate visual state."
    (when (bray-state-derived-p 'normal)
      (bray-state-stack-push 'visual)))
  (defun meep-mark-hook-deactivate ()
    "Activate visual state."
    (when (bray-state-derived-p 'visual)
      (bray-state-stack-pop)))

  (add-hook
   'bray-mode-hook
   (lambda ()
     (cond
      (bray-mode
       (add-hook 'activate-mark-hook #'meep-mark-hook-activate)
       (add-hook 'deactivate-mark-hook #'meep-mark-hook-deactivate))
      (t
       (remove-hook 'activate-mark-hook #'meep-mark-hook-activate)
       (remove-hook 'deactivate-mark-hook #'meep-mark-hook-deactivate)))))
  ;; End visual mode support.

  (add-hook
   'meep-state-hook-insert-enter
   (lambda ()
     (set-mark (point))
     (deactivate-mark)))

  (add-hook
   'meep-state-hook-insert-exit
   (lambda ()
     ;; (set-mark (point))
     (deactivate-mark)

     ;; Testing this out!
     ;; VIM style '^' register for when we leave insert mode.
     (let ((reg ?^))
       (let ((reg-val (get-register reg)))
         (cond
          ((and reg-val (markerp reg-val))
           (set-marker reg-val (point) (current-buffer)))
          (t
           (set-register reg (point-marker))))))))


  (defvar meep-state-keymap-motion (make-keymap))
  (defvar meep-state-keymap-normal (make-keymap))
  (defvar meep-state-keymap-visual (make-keymap))
  (defvar meep-state-keymap-insert (make-keymap))

  ;; Optional, a quick way to mask insertion.
  (define-key meep-state-keymap-motion [remap self-insert-command] 'my-key-free)

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

  (my-meep-basis-keys)
  (add-hook
   'after-change-major-mode-hook
   (lambda ()
     (when (and (not (minibufferp)) (not (derived-mode-p 'special-mode)))
       (bray-mode)))))

(add-hook 'emacs-startup-hook #'my-meep-setup-once)
;;; provide
(provide '130-init-meep)
;;; 130-init-meep.el ends here
