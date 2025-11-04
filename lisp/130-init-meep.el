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
(require 'bray)
(require 'meep)
(require 'mark-thing-at)

(mark-thing-at-make-functions)

(setq modaled-init-state-fn #'oo-init-state-fn)

(defun oo-init-state-fn ()
  (cond ((minibufferp)
         "insert")
        (t
         "normal")))

(defun oo-dwim-escape ()
  "Exit out of whatever is happening after escape.
Enter normal state.  If in minibuffer, exit the minibuffer.  When in a
non-readonly file buffer, save the buffer."
  (interactive)
  (when (bound-and-true-p evil-mode)
    (evil-normal-state 1))
  (bray-state-stack-pop)
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

(defun my-key-free ()
  (interactive)
  (let ((keys (this-command-keys-vector)))
    (message "Key Free: %s" (format-kbd-macro keys))))

(defvar meep-state-hook-insert-enter nil)
(defvar meep-state-hook-insert-exit nil)

(defvar meep-state-hook-normal-enter nil)
(defvar meep-state-hook-normal-exit nil)

(defvar meep-state-hook-motion-enter nil)
(defvar meep-state-hook-motion-exit nil)

(defvar meep-state-hook-visual-enter nil)
(defvar meep-state-hook-visual-exit nil)

(defvar-keymap meep-state-keymap-motion
  "<escape>" #'oo-dwim-escape)

(define-key meep-state-keymap-motion [remap self-insert-command] #'my-key-free)

(defvar-keymap meep-state-keymap-normal
  "1" #'digit-argument
  "2" #'digit-argument
  "3" #'digit-argument
  "4" #'digit-argument
  "5" #'digit-argument
  "6" #'digit-argument
  "7" #'digit-argument
  "8" #'digit-argument
  "9" #'digit-argument
  "0" #'digit-argument
  "-" #'digit-argument

  "`" #'meep-region-to-secondary-selection
  "~" #'meep-region-swap

  ;; ----
  ;; Left Hand: Row 1.
  ;; ----

  "q" #'repeat-fu-execute
  "Q" #'meep-register-kmacro-start-or-end

  "w" #'meep-move-word-next
  "W" #'meep-move-symbol-next

  "e" #'meep-move-word-next-end
  "E" #'meep-move-symbol-next-end

  "r" #'meep-clipboard-killring-yank-pop-stack
  "R" #'meep-clipboard-only-yank

  "t" #'meep-char-surround-insert
  "T" #'meep-char-surround-insert-lines

  ;; Left Hand: Row 2.

  "a a" #'meep-insert
  "a s" #'meep-insert-append
  "a l" #'meep-insert-append
  ;; "a" #'meep-keypad
  ;; "A" #'my-key-free

  ;; "s r" #'meep-delete-char-ring-yank

  ;; "s s" #'meep-insert-at-last
  "s d" #'rectangle-mark-mode

  "s s" #'meep-insert
  "s a" #'meep-insert-append
  "s j" #'meep-insert-open-below
  "s k" #'meep-insert-open-above
  "s l" #'meep-insert-line-end
  "s h" #'meep-insert-line-beginning

  "s c" #'comment-or-uncomment-region
  "s M" #'morse-region
  "s m" #'downcase-region
  "s ," #'upcase-region
  "s o" #'rot13-region
  "s r" #'oo-eval-and-replace-region
  "s e" #'eval-region

  "s <return>" #'fill-region
  "s <end>" #'end-of-buffer
  "s <home>" #'beginning-of-buffer

  ;; Run commands "after" numeric has been set.
  ;; Unlike the default to re-running N times.
  "s 1" #'meep-digit-argument-repeat
  "s 2" #'meep-digit-argument-repeat
  "s 3" #'meep-digit-argument-repeat
  "s 4" #'meep-digit-argument-repeat
  "s 5" #'meep-digit-argument-repeat
  "s 6" #'meep-digit-argument-repeat
  "s 7" #'meep-digit-argument-repeat
  "s 8" #'meep-digit-argument-repeat
  "s 9" #'meep-digit-argument-repeat
  "s 0" #'meep-digit-argument-repeat
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

  "f m" #'meep-isearch-at-point-next
  "f ," #'meep-isearch-at-point-prev

  "f u" #'avy-goto-symbol-1-below
  "f i" #'avy-goto-symbol-1-above

  ;; Alternative to VIM's ":" to go to line numbers (frees up a key).
  "f ;" #'goto-line
  "f :" #'goto-char

  "F" #'my-key-free

  "g g" #'meep-clipboard-killring-copy
  "g j" #'meep-clipboard-killring-copy
  "g k" #'meep-clipboard-killring-cut
  "g h" #'meep-clipboard-killring-yank
  "g l" #'meep-clipboard-killring-yank

  "G" #'meep-char-insert
  ;; "g" #'meep-char-replace
  ;; "G" #'meep-char-insert

  ;; Left Hand: Row 3.
  "z" #'undo-only
  "Z" #'undo-redo

  "x" #'meep-insert
  "X" #'meep-insert-overwrite

  "c" #'meep-delete-char-ring-next
  "C" #'meep-delete-char-ring-prev

  "v" #'meep-transpose
  "V" #'meep-clipboard-killring-cut-line

  "b" #'meep-insert-change
  "B" #'meep-insert-change-lines

  ;; Right Hand: Row 1.
  "y" #'meep-clipboard-killring-copy
  "Y" #'meep-clipboard-only-copy
  ;; "y" #'my-key-free
  ;; "Y" #'my-key-free

  "u" #'meep-clipboard-killring-cut
  "U" #'meep-clipboard-only-cut
  ;; "u" #'meep-exchange-point-and-mark
  ;; "U" #'my-key-free

  "i" #'meep-exchange-point-and-mark-motion
  "I" #'my-key-free

  "o" #'meep-region-mark-bounds-of-char-contextual-inner
  "O" #'meep-region-mark-bounds-of-char-contextual-outer
  ;; "o" #'meep-region-mark-bounds-of-char-inner
  ;; "O" #'meep-region-mark-bounds-of-char-outer

  "p" #'meep-clipboard-register-actions
  "P" #'point-to-register

  ;; Right Hand: Row 2.
  "h" #'meep-move-char-prev
  "H" #'meep-move-line-non-space-beginning

  "j" #'meep-move-line-next
  "J" #'meep-move-paragraph-next

  "k" #'meep-move-line-prev
  "K" #'meep-move-paragraph-prev

  "l" #'meep-move-char-next
  "L" #'meep-move-line-non-space-end

  ";" #'execute-extended-command
  ;; ";" #'meep-move-matching-bracket-inner
  ":" #'meep-move-matching-bracket-outer

  "'" #'meep-move-matching-syntax-inner
  "\"" #'meep-move-matching-syntax-outer

  ;; Right Hand: Row 3.
  "n" #'meep-move-symbol-prev
  "N" #'meep-move-same-syntax-and-space-prev

  "m" #'meep-isearch-repeat-next
  "M" #'my-key-free

  "," #'meep-isearch-repeat-prev
  "<" #'my-key-free

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
  "<escape>" #'oo-dwim-escape
  oo-normal-leader-key #'oo-leader-map)

(defvar-keymap meep-state-keymap-insert
  "<escape>" #'oo-dwim-escape)

(defvar-keymap meep-state-keymap-visual
  "e" #'eval-region
  "E" #'oo-eval-and-replace-region
  "<escape>" #'oo-dwim-escape)

;; In insert state when the minibuffer is activated and `vertico-mode' is
;; enabled, make a keymap that has priority over `meep-state-keymap-insert' and
;; have the vertico bindings there.

(defvar-keymap oo-vertico-state-insert-keymap
  "C-n" #'vertico-scroll-up
  "C-p" #'vertico-scroll-down
  "TAB" #'vertico-next
  "C-k" #'vertico-previous
  "C-j" #'vertico-next
  ";" #'vertico-quick-exit
  "C-;" #'vertico-quick-exit
  "<backtab>" #'vertico-previous
  "C-o" #'embark-act)

(defvar oo-mode-maps-alist nil
  "Alist of modes to keymaps.")

(add-to-list 'emulation-mode-map-alists 'oo-mode-maps-alist)

(defun oo-enable-bindings-maybe ()
  "If in minibuffer and Vertico is active, give Vertico bindings highest priority."
  (cond ((and (minibufferp) (bound-and-true-p vertico-mode))
         (setq-local oo-mode-maps-alist (cons (cons t oo-vertico-state-insert-keymap)
                                              oo-mode-maps-alist)))))

(add-hook 'meep-state-hook-insert-enter #'oo-enable-bindings-maybe)

;; Probably make the other bindings high priority too.

(defvar-keymap meep-clipboard-register-map
  "e" #'meep-clipboard-register-cut
  "r" #'meep-clipboard-register-yank
  "t" #'meep-clipboard-register-copy)

;; Visual mode.
;; (defun meep-mark-hook-activate ()
;;   "Activate visual state."
;;   (when (bray-state-derived-p 'normal)
;;     (bray-state-stack-push 'visual)))

;; (defun meep-mark-hook-deactivate ()
;;   "Activate visual state."
;;   (when (bray-state-derived-p 'visual)
;;     (bray-state-stack-pop)))

;; (defun oo-setup-visual-state ()
;;   (cond
;;    (bray-mode
;;     (add-hook 'activate-mark-hook #'meep-mark-hook-activate)
;;     (add-hook 'deactivate-mark-hook #'meep-mark-hook-deactivate))
;;    (t
;;     (remove-hook 'activate-mark-hook #'meep-mark-hook-activate)
;;     (remove-hook 'deactivate-mark-hook #'meep-mark-hook-deactivate))))

;; (add-hook 'bray-mode-hook #'oo-setup-visual-state)
;; End visual mode support.

(setq meep-state-insert 'insert)
(setq bray-state-default 'normal)

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

(defun oo-init-bray ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (unless bray-mode (bray-mode 1)))))

(defun oo-init-meep ()
  (add-hook 'meep-state-hook-insert-enter (lambda () (set-mark (point)) (deactivate-mark)))
  ;; Testing this out!
  ;; VIM style '^' register for when we leave insert mode.
  (add-hook 'meep-state-hook-insert-exit (lambda () (deactivate-mark) (let ((reg ?^)) (let ((reg-val (get-register reg))) (cond ((and reg-val (markerp reg-val)) (set-marker reg-val (point) (current-buffer))) (t (set-register reg (point-marker))))))))

  (add-hook 'buffer-list-update-hook #'oo-init-bray)
  ;; Optional, a quick way to mask insertion.
  (add-hook
   'after-change-major-mode-hook
   (lambda ()
     ;; Enable it in the minibuffer.
     (when (not (derived-mode-p 'special-mode))
       (bray-mode)
       (when (minibufferp)
         (bray-state-set 'insert)))))
  (oo-init-bray))

(add-hook 'emacs-startup-hook #'oo-init-meep 80)
;;; provide
(provide '130-init-meep)
;;; 130-init-meep.el ends here
