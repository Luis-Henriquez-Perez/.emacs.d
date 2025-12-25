;;; init-bray.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;;
;;; Code:
(push "/home/luis/.config/emacs/elpaca/builds/bray" load-path)

(require 'bray)
(require 'bray-state-map)
(require 'meep)
(require 'init-meep)
(require 'base)

(setq bray-state-default 'normal)
(setq bray-state-map-enabled t)

(defvar o-bray-state-normal-enter-hook nil)
(defvar o-bray-state-normal-exit-hook nil)

(defvar o-bray-state-insert-enter-hook nil)
(defvar o-bray-state-insert-exit-hook nil)

(defvar o-bray-state-visual-enter-hook nil)
(defvar o-bray-state-visual-exit-hook nil)

(defvar-keymap o-bray-state-normal-map
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
  ;; "-" #'digit-argument

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
  "r" #'meep-move-symbol-prev
  "R" #'meep-move-symbol-prev-end
  ;; "t" #'meep-char-surround-insert
  ;; "T" #'meep-char-surround-insert-lines

  ;; Left Hand: Row 2.
  "a a" #'meep-insert
  "a s" #'meep-insert-append
  "a l" #'meep-insert-append

  ;; "s s" #'meep-insert-at-last
  "s d" #'rectangle-mark-mode
  "s s" #'meep-insert
  "s a" #'meep-insert-append
  "s j" #'meep-insert-open-below
  "s k" #'meep-insert-open-above
  "s l" #'meep-insert-line-end
  "s h" #'meep-insert-line-beginning

  "s c" #'comment-or-uncomment-region

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

  "d d" #'meep-clipboard-only-cut
  "d j" #'meep-insert-change
  "d f" #'meep-insert-change
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

  "F" #'o-bray-unbound-key

  "g J" #'meep-clipboard-only-copy
  "g k" #'meep-clipboard-killring-cut
  "g K" #'meep-clipboard-only-cut
  "g l" #'meep-clipboard-killring-yank
  "g L" #'meep-clipboard-only-yank

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
  ;; "y" #'o-bray-unbound-key
  ;; "Y" #'o-bray-unbound-key

  "u" #'meep-clipboard-killring-cut
  "U" #'meep-clipboard-only-cut
  ;; "u" #'meep-exchange-point-and-mark
  ;; "U" #'o-bray-unbound-key

  "i" #'meep-region-activate-or-reverse
  "I" #'meep-region-toggle

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
  "M" #'o-bray-unbound-key

  "," #'meep-isearch-repeat-prev
  "<" #'o-bray-unbound-key

  "." #'meep-move-symbol-next
  ">" #'meep-move-same-syntax-and-space-next

  "/" #'meep-move-symbol-next-end
  "?" #'meep-move-same-syntax-and-space-next-end

  ;; Other keys.
  "\\" #'meep-register-jump-to
  "|" #'meep-register-kmacro-start-or-end

  "[" #'meep-move-to-bounds-of-thing-beginning
  "]" #'meep-move-to-bounds-of-thing-end

  "+" #'text-scale-increase
  "-" #'text-scale-decrease
  "<tab>" #'meep-indent-rigidly
  "<escape>" #'o-bray-dwim-escape
  o-key-leader-normal #'o-leader-map)

(defvar-keymap o-bray-state-insert-map
  "C-f" #'scroll-down
  "<escape>" #'o-bray-dwim-escape)

(defvar-keymap o-bray-state-motion-map
  "C-f" #'scroll-down
  "<escape>"
  #'o-bray-dwim-escape)

(defvar-keymap o-bray-state-visual-map
  "u" #'downcase-region
  "U" #'upcase-region
  "a" #'meep-clipboard-only-copy
  "A" #'meep-clipboard-killring-copy
  "s" #'meep-clipboard-only-cut
  "S" #'meep-clipboard-killring-cut
  "e" #'eval-region
  "E" #'o-eval-and-replace-region
  "<escape>" #'o-bray-dwim-escape)

(keymap-set o-bray-state-motion-map "<remap> <self-insert-command>" #'o-bray-unbound-key)

(defun o-bray-unbound-key ()
  "Indicate that current key is unbound."
  (interactive)
  (let ((keys (this-command-keys-vector)))
    (message "Unbound Key: %s" (format-kbd-macro keys))))

(defun o-bray-dwim-escape ()
  "Exit out of whatever is happening after escape.
Enter normal state.  If in minibuffer, exit the minibuffer.  When in a
non-readonly file buffer, save the buffer."
  (interactive)
  (when (bound-and-true-p evil-mode)
    (evil-normal-state 1))
  (bray-state-stack-pop)
  (cond
   ((minibuffer-window-active-p (minibuffer-window))
    (if (or defining-kbd-macro executing-kbd-macro)
        (minibuffer-keyboard-quit)
      (abort-recursive-edit)))
   ((or defining-kbd-macro executing-kbd-macro)
    nil)
   (t
    (when (and (not buffer-read-only) (buffer-file-name) (buffer-modified-p))
      (save-buffer))
    (keyboard-quit))))

(defun o-bray-ensure ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (unless bray-mode
        (bray-mode 1)))))

(defun o-bray-init ()
  "Ensure bray is enabled in all buffers."
  (cond
   (bray-mode
    (add-hook 'post-command-hook #'o-bray-update-cursor-color)
    (add-hook 'buffer-list-update-hook #'o-bray-ensure)
    ;; Optional, a quick way to mask insertion.
    (add-hook 'after-change-major-mode-hook #'o-bray-dwim)
    (add-hook 'activate-mark-hook #'o-bray-mark-hook-activate)
    (add-hook 'deactivate-mark-hook #'o-bray-mark-hook-deactivate)
    ;; (add-hook 'bray-mode-hook #'o-bray-init-visual-state)
    )
   (t
    (remove-hook 'post-command-hook #'o-bray-update-cursor-color)
    (remove-hook 'buffer-list-update-hook #'o-bray-ensure)
    (remove-hook 'after-change-major-mode-hook #'o-bray-dwim)
    (remove-hook 'activate-mark-hook #'o-bray-mark-hook-activate)
    (remove-hook 'deactivate-mark-hook #'o-bray-mark-hook-deactivate))))

(add-hook 'minibuffer-setup-hook (lambda () (bray-state-set 'insert)))

(defun o-bray-dwim ()
  (when (not (derived-mode-p 'special-mode))
    (unless bray-mode
      (bray-mode 1))))

(defun o-bray-update-cursor-color ()
  "Update the cursor color based on the current state."
  (let (fg)
    (pcase (bray-state)
      ('insert
       (setq fg (face-attribute font-lock-type-face :foreground))
       (set-cursor-color
        (if (stringp fg)
            fg
          "#228b22")))
      ('normal
       (setq fg (face-attribute font-lock-keyword-face :foreground))
       (set-cursor-color
        (if (stringp fg)
            fg
          "purple")))
      ('visual
       (setq fg (face-attribute font-lock-string-face :foreground))
       (set-cursor-color
        (if (stringp fg)
            fg
          "#8b2252"))))))

(setq bray-state-definitions
      (list
       (list
        :id 'normal
        ;; Define.
        :cursor-type 'box
        :lighter "<N>"
        :keymaps
        (list
         (cons t 'o-bray-state-motion-map) (cons t 'o-bray-state-normal-map))

        :enter-hook 'o-bray-state-normal-enter-hook
        :exit-hook 'o-bray-state-normal-exit-hook)

       (list
        :id 'visual
        ;; Define.
        :cursor-type 'box
        :lighter "<V>"
        :keymaps
        (list (cons t 'o-bray-state-normal-map) (cons t 'o-bray-state-visual-map))

        :enter-hook 'o-bray-state-visual-enter-hook
        :exit-hook 'o-bray-state-visual-exit-hook)

       (list
        :id 'insert
        ;; Define.
        :cursor-type 'bar
        :lighter "<I>"
        :keymaps (list (cons t 'o-bray-state-insert-map))

        :enter-hook 'o-bray-state-insert-enter-hook
        :exit-hook 'o-bray-state-insert-exit-hook

        ;; Optional.
        :is-input t)))

(defun o-bray-mark-hook-activate ()
  "Activate visual state."
  (when (bray-state-derived-p 'normal)
    (bray-state-stack-push 'visual)))

(defun o-bray-mark-hook-deactivate ()
  "Activate visual state."
  (when (bray-state-derived-p 'visual)
    (bray-state-stack-pop)))

(add-hook 'bray-mode-hook #'o-bray-init)
(add-hook 'emacs-startup-hook #'o-bray-ensure 80)

(o-after vertico
  (bray-state-map-set 'insert vertico-map "C-j" #'vertico-next)
  (bray-state-map-set 'insert vertico-map "C-k" #'vertico-previous)
  (bray-state-map-set 'insert vertico-map "C-n" #'vertico-scroll-up)
  (bray-state-map-set 'insert vertico-map "C-p" #'vertico-scroll-down)
  (bray-state-map-set 'insert vertico-map "TAB" #'vertico-next)
  (bray-state-map-set 'insert vertico-map ";" #'vertico-quick-exit)
  (bray-state-map-set 'insert vertico-map "C-;" #'vertico-quick-exit)
  (bray-state-map-set 'insert vertico-map "<backtab>" #'vertico-previous)
  (bray-state-map-set 'insert vertico-map "C-o" #'embark-act))

(o-after helm
  (bray-state-map-set 'insert helm-map "TAB" #'helm-next-line)
  ;; (bray-state-map-set 'insert 'helm-map [backtab] #'helm-previous-line)
  (bray-state-map-set 'insert helm-map "C-j" #'helm-next-line)
  (bray-state-map-set 'insert helm-map "C-k" #'helm-previous-line)
  (bray-state-map-set 'insert helm-map "C-a" #'helm-select-action)
  (bray-state-map-set 'insert helm-map "C-m" #'helm-toggle-visible-mark-forward)
  ;; (bray-state-map-set 'insert 'helm-map "RET" #'+helm-select-nth-action)
  ;; (bray-state-map-set 'insert 'helm-map "RET" #'+helm-select-nth-action)
  (bray-state-map-set 'insert helm-map "S-TAB" #'helm-mark-current-line)
  (bray-state-map-set 'insert helm-map "C-;" #'ace-jump-helm-line))

(o-after corfu
  (bray-state-map-set 'insert corfu-map "<tab>" #'corfu-next)
  ;; (bray-state-map-set 'insert 'corfu-map [backtab] #'corfu-previous)
  (bray-state-map-set 'insert corfu-map "S-TAB" #'corfu-previous)
  (bray-state-map-set 'insert corfu-map "C-;" #'corfu-quick-complete)
  (bray-state-map-set 'insert corfu-map "C-j" #'corfu-next)
  (bray-state-map-set 'insert corfu-map "C-k" #'corfu-previous)
  (bray-state-map-set 'insert corfu-map "C-p" #'corfu-previous)
  (bray-state-map-set 'insert corfu-map ";" #'corfu-quick-complete)
  (bray-state-map-set 'insert corfu-map "SPC" #'corfu-insert))

(o-after tempel
  (bray-state-map-set 'insert tempel-map "C-l" #'tempel-abort)
  (bray-state-map-set 'insert tempel-map "C-j" #'tempel-next)
  (bray-state-map-set 'insert tempel-map "C-k" #'tempel-previous)
  (bray-state-map-set 'insert tempel-map "TAB" #'tempel-next)
  (bray-state-map-set 'insert tempel-map [backtab] #'tempel-previous))

(o-after dired
  (bray-state-map-set 'normal dired-mode-map "h" #'dired-up-directory)
  (bray-state-map-set 'normal dired-mode-map "l" #'dired-find-file)
  (bray-state-map-set 'normal dired-mode-map "RET" #'dired-find-file)
  (bray-state-map-set 'normal dired-mode-map "o" #'dired-omit-mode))
;;; provide
(provide 'init-bray)
;;; init-bray.el ends here
