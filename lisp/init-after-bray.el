;;; init-after-bray.el --- Configure bray -*- lexical-binding: t; -*-
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
(require 'init-core)
(require 'bray)
(require 'bray-state-map)
(require 'meep)
(require 'meep-mark-commands)
(require 'smartparens)
(require 'init-core-commands)
;;;; utilities
;;;; normal map
;;;;; 0..9
(keymap-set o-bray-state-normal-map "1" #'digit-argument)
(keymap-set o-bray-state-normal-map "2" #'digit-argument)
(keymap-set o-bray-state-normal-map "3" #'digit-argument)
(keymap-set o-bray-state-normal-map "4" #'digit-argument)
(keymap-set o-bray-state-normal-map "5" #'digit-argument)
(keymap-set o-bray-state-normal-map "6" #'digit-argument)
(keymap-set o-bray-state-normal-map "7" #'digit-argument)
(keymap-set o-bray-state-normal-map "8" #'digit-argument)
(keymap-set o-bray-state-normal-map "9" #'digit-argument)
(keymap-set o-bray-state-normal-map "0" #'digit-argument)

;; Run commands "after" numeric has been set.
(keymap-set o-bray-state-normal-map "s 1" #'meep-digit-argument-repeat)
(keymap-set o-bray-state-normal-map "s 2" #'meep-digit-argument-repeat)
(keymap-set o-bray-state-normal-map "s 3" #'meep-digit-argument-repeat)
(keymap-set o-bray-state-normal-map "s 4" #'meep-digit-argument-repeat)
(keymap-set o-bray-state-normal-map "s 5" #'meep-digit-argument-repeat)
(keymap-set o-bray-state-normal-map "s 6" #'meep-digit-argument-repeat)
(keymap-set o-bray-state-normal-map "s 7" #'meep-digit-argument-repeat)
(keymap-set o-bray-state-normal-map "s 8" #'meep-digit-argument-repeat)
(keymap-set o-bray-state-normal-map "s 9" #'meep-digit-argument-repeat)
(keymap-set o-bray-state-normal-map "s 0" #'meep-digit-argument-repeat)
(keymap-set o-bray-state-normal-map "s -" #'negative-argument)
;;;;; +-;*
(keymap-set o-bray-state-normal-map "<escape>" #'o-bray-dwim-escape)
(keymap-set o-bray-state-normal-map "<tab>" #'outline-toggle-children)

(keymap-set o-bray-state-normal-map "*" #'meep-isearch-at-point-next)

(keymap-set o-bray-state-normal-map "+" #'text-scale-increase)
(keymap-set o-bray-state-normal-map "-" #'text-scale-decrease)
(keymap-set o-bray-state-normal-map ";" #'execute-extended-command)
(keymap-set o-bray-state-normal-map o-key-leader-normal #'o-leader-map)
(keymap-set o-bray-state-normal-map ":" #'meep-move-matching-bracket-outer)
(keymap-set o-bray-state-normal-map "<remap> <self-insert-command>" #'ignore)

(keymap-set o-bray-state-normal-map "[" #'meep-isearch-at-point-prev)
(keymap-set o-bray-state-normal-map "]" #'meep-isearch-at-point-next)
;;;;; werthjkl - movement
(keymap-set o-bray-state-normal-map "w" #'meep-move-word-next)
(keymap-set o-bray-state-normal-map "W" #'meep-move-symbol-next)
(keymap-set o-bray-state-normal-map "e" #'meep-move-word-next-end)
(keymap-set o-bray-state-normal-map "E" #'meep-move-symbol-next-end)
(keymap-set o-bray-state-normal-map "r" #'meep-move-word-prev)
(keymap-set o-bray-state-normal-map "R" #'meep-move-symbol-prev-end)

(keymap-set o-bray-state-normal-map "h" #'meep-move-char-prev)
(keymap-set o-bray-state-normal-map "H" #'meep-move-line-non-space-beginning)

(keymap-set o-bray-state-normal-map "j" #'meep-move-line-next)
(keymap-set o-bray-state-normal-map "J" #'scroll-up)

(keymap-set o-bray-state-normal-map "k" #'meep-move-line-prev)
(keymap-set o-bray-state-normal-map "K" #'scroll-down)

(keymap-set o-bray-state-normal-map "l" #'meep-move-char-next)
(keymap-set o-bray-state-normal-map "L" #'meep-move-line-non-space-end)
;;;;; t - char replace
;; (keymap-set o-bray-state-normal-map "t" #'meep-char-replace)
;; (keymap-set o-bray-state-normal-map "T" #'o-bray-unbound-key)
(keymap-set o-bray-state-normal-map "t" #'better-jumper-jump-backward)
(keymap-set o-bray-state-normal-map "T" #'better-jumper-jump-forward)
;;;;; q - keyboard macro
(keymap-set o-bray-state-normal-map "q" #'o-kmacro-start-or-end)
(keymap-set o-bray-state-normal-map "Q" #'o-bray-unbound-key)
;;;;; g - miscellaneous
(keymap-set o-bray-state-normal-map "g g" #'beginning-of-buffer)
(keymap-set o-bray-state-normal-map "g h" #'beginning-of-buffer)
(keymap-set o-bray-state-normal-map "g G" #'end-of-buffer)
(keymap-set o-bray-state-normal-map "g i" #'kmacro-insert-counter)
(keymap-set o-bray-state-normal-map "G" #'end-of-buffer)
;;;;; s - select
(keymap-set o-bray-state-normal-map "s k" #'o-region-mark-character)
(keymap-set o-bray-state-normal-map "s w" #'meep-region-mark-word)
(keymap-set o-bray-state-normal-map "s j" #'meep-region-mark-word)
(keymap-set o-bray-state-normal-map "s s" #'meep-region-mark-symbol)
(keymap-set o-bray-state-normal-map "s m" #'meep-region-mark-symbol)
(keymap-set o-bray-state-normal-map "s l" #'meep-region-expand-to-line-bounds)
(keymap-set o-bray-state-normal-map "s L" #'meep-region-expand-to-line-bounds)
(keymap-set o-bray-state-normal-map "s ;" #'meep-region-mark-line-inner)
(keymap-set o-bray-state-normal-map "s r" #'meep-region-mark-string-inner)
(keymap-set o-bray-state-normal-map "s S" #'meep-region-mark-string-outer)
(keymap-set o-bray-state-normal-map "s t" #'meep-region-mark-sentence-inner)
(keymap-set o-bray-state-normal-map "s T" #'meep-region-mark-sentence-outer)
(keymap-set o-bray-state-normal-map "s d" #'meep-region-mark-defun-inner)
(keymap-set o-bray-state-normal-map "s D" #'meep-region-mark-defun-outer)
(keymap-set o-bray-state-normal-map "s c" #'meep-region-mark-comment-inner)
(keymap-set o-bray-state-normal-map "s C" #'meep-region-mark-comment-outer)
(keymap-set o-bray-state-normal-map "s p" #'meep-region-mark-paragraph-inner)
(keymap-set o-bray-state-normal-map "s P" #'meep-region-mark-paragraph-outer)
(keymap-set o-bray-state-normal-map "s f" #'o-region-mark-delim-inner)
(keymap-set o-bray-state-normal-map "s F" #'o-region-mark-delim-outer)

(keymap-set o-bray-state-normal-map "s i w" #'meep-region-mark-word)
(keymap-set o-bray-state-normal-map "s i m" #'meep-region-mark-symbol)
(keymap-set o-bray-state-normal-map "s i j" #'meep-region-mark-symbol)
(keymap-set o-bray-state-normal-map "s i l" #'meep-region-mark-line-inner)
(keymap-set o-bray-state-normal-map "s i p" #'meep-region-mark-paragraph-inner)
(keymap-set o-bray-state-normal-map "s i t" #'meep-region-mark-sentence-inner)
(keymap-set o-bray-state-normal-map "s i c" #'meep-region-mark-comment-inner)
(keymap-set o-bray-state-normal-map "s i d" #'meep-region-mark-defun-inner)
(keymap-set o-bray-state-normal-map "s i r" #'meep-region-mark-string-inner)
(keymap-set o-bray-state-normal-map "s i f" #'o-region-mark-delim-inner)

(keymap-set o-bray-state-normal-map "s o w" #'meep-region-mark-word)
(keymap-set o-bray-state-normal-map "s o m" #'meep-region-mark-symbol)
(keymap-set o-bray-state-normal-map "s o s" #'meep-region-mark-symbol)
(keymap-set o-bray-state-normal-map "s o j" #'meep-region-mark-symbol)
(keymap-set o-bray-state-normal-map "s o l" #'meep-region-expand-to-line-bounds)
(keymap-set o-bray-state-normal-map "s o p" #'meep-region-mark-paragraph-outer)
(keymap-set o-bray-state-normal-map "s o t" #'meep-region-mark-sentence-outer)
(keymap-set o-bray-state-normal-map "s o c" #'meep-region-mark-comment-outer)
(keymap-set o-bray-state-normal-map "s o r" #'meep-region-mark-string-outer)
(keymap-set o-bray-state-normal-map "s o f" #'o-region-mark-delim-outer)
;;;;; i - invert point and mark
(keymap-set o-bray-state-normal-map "i" #'meep-region-activate-or-reverse)
(keymap-set o-bray-state-normal-map "I" #'o-bray-unbound-key)
;;;;; a - insert
(keymap-set o-bray-state-normal-map "A" #'meep-insert-line-end)
(keymap-set o-bray-state-normal-map "a a" #'meep-insert)
(keymap-set o-bray-state-normal-map "a s" #'meep-insert-append)
(keymap-set o-bray-state-normal-map "a j" #'meep-insert-open-below)
(keymap-set o-bray-state-normal-map "a k" #'meep-insert-open-above)
(keymap-set o-bray-state-normal-map "a l" #'meep-insert-append)
(keymap-set o-bray-state-normal-map "a h" #'meep-insert)
(keymap-set o-bray-state-normal-map "a L" #'meep-insert-line-end)
(keymap-set o-bray-state-normal-map "a ;" #'meep-insert-line-end)
(keymap-set o-bray-state-normal-map "a H" #'meep-insert-line-beginning)
;;;;; d - act on region
(keymap-set o-bray-state-normal-map "d a" #'o-expand-region-abbrevs-no-query)
;; Maintain a similarity with vim's "dd"
(keymap-set o-bray-state-normal-map "d d" #'o-safe-kill-lne)
(keymap-set o-bray-state-normal-map "d j" #'o-eval-region)
(keymap-set o-bray-state-normal-map "d k" #'comment-or-uncomment-region)
(keymap-set o-bray-state-normal-map "d l" #'duplicate-line)
(keymap-set o-bray-state-normal-map "d r" #'o-eval-and-replace-region)
(keymap-set o-bray-state-normal-map "d ;" #'iedit-mode)
(keymap-set o-bray-state-normal-map "d c" #'capitalize-region)
(keymap-set o-bray-state-normal-map "d h" #'helpful-at-point)
(keymap-set o-bray-state-normal-map "d o" #'sort-lines)
(keymap-set o-bray-state-normal-map "d e" #'o-eval-region)
(keymap-set o-bray-state-normal-map "d n" #'narrow-to-region)
(keymap-set o-bray-state-normal-map "d u" #'downcase-region)
(keymap-set o-bray-state-normal-map "d U" #'upcase-region)
(keymap-set o-bray-state-normal-map "d y" #'flyspell-region)
(keymap-set o-bray-state-normal-map "d s" #'meep-char-surround-insert)
(keymap-set o-bray-state-normal-map "d w" #'widen)
;; Uncommon therefore I give these keybindings the harder to press keys.
(keymap-set o-bray-state-normal-map "d R" #'rot13-region)
(keymap-set o-bray-state-normal-map "d m" #'unmorse-region)
(keymap-set o-bray-state-normal-map "d M" #'morse-region)
(keymap-set o-bray-state-normal-map "d f" #'grugru-forward)

(defvar o-grugru-repeat-map
  (let ((map (make-sparse-keymap)))
    ;; (keymap-set map "g" #'grugru-forward)
    (keymap-set map "f" #'grugru-forward)
    map))

(put 'grugru-forward 'repeat-map 'o-grugru-repeat-map)
;;;;; f - search
(keymap-set o-bray-state-normal-map "f" #'flash-jump)
(keymap-set o-bray-state-normal-map "F" #'o-bray-unbound-key)
;;;;; x - delete
(keymap-set o-bray-state-normal-map "x" #'o-region-safe-kill)
(keymap-set o-bray-state-normal-map "X" #'o-region-safe-delete)
;;;;; m - page
(keymap-set o-bray-state-normal-map "m m" #'recenter)
(keymap-set o-bray-state-normal-map "m j" #'o-scroll-to-bottom)
(keymap-set o-bray-state-normal-map "m k" #'o-scroll-to-top)

(keymap-set o-bray-state-normal-map "M" #'o-bray-unbound-key)
;;;;; u - undo
(keymap-set o-bray-state-normal-map "u" #'undo-only)
(keymap-set o-bray-state-normal-map "U" #'undo-redo)
;;;;; v - visual
;; (keymap-set o-bray-state-normal-map "v" #'meep-region-toggle)
(keymap-set o-bray-state-normal-map "v" #'expreg-expand)
(keymap-set o-bray-state-normal-map "V" #'meep-clipboard-killring-cut-line)
;;;;; c - change
(keymap-set o-bray-state-normal-map "c" #'meep-insert-change)
(keymap-set o-bray-state-normal-map "C" #'meep-insert-change-lines)
;;;;; y - copy
(keymap-set o-bray-state-normal-map "y" #'o-copy-region-as-kill)
(keymap-set o-bray-state-normal-map "Y" #'meep-clipboard-only-copy)
;;;;; p - paste
(keymap-set o-bray-state-normal-map "p" #'yank)
;; puni-splice
;; register-to-point
;; point-to-register
;; ffap
(keymap-set o-bray-state-normal-map "P" #'pop-to-mark-command)
;;;;; z - undo
;; (keymap-set o-bray-state-normal-map "z" #'undo-only)
;; (keymap-set o-bray-state-normal-map "Z" #'undo-redo)
;;;;; /?nb - search
(keymap-set o-bray-state-normal-map "/" #'meep-isearch-regexp-next)
(keymap-set o-bray-state-normal-map "?" #'meep-isearch-regexp-prev)

(keymap-set o-bray-state-normal-map "n" #'meep-isearch-repeat-next)
(keymap-set o-bray-state-normal-map "N" #'meep-isearch-repeat-prev)

(keymap-set o-bray-state-normal-map "b" #'meep-isearch-at-point-next)
(keymap-set o-bray-state-normal-map "B" #'meep-isearch-at-point-prev)
;;;;; other
(keymap-set o-bray-state-normal-map "'" #'o-bray-unbound-key)
(keymap-set o-bray-state-normal-map "\"" #'o-bray-unbound-key)

(keymap-set o-bray-state-normal-map "o" #'o-region-mark-delim-inner)
(keymap-set o-bray-state-normal-map "O" #'o-region-mark-delim-outer)

;; Do not do anything to "," because it is used for local leader.
;; (keymap-set o-bray-state-normal-map "," #'o-bray-unbound-key)
(keymap-set o-bray-state-normal-map "<" #'o-bray-unbound-key)

(keymap-set o-bray-state-normal-map "." #'kmacro-call-macro)
(keymap-set o-bray-state-normal-map ">" #'o-bray-unbound-key)

(keymap-set o-bray-state-insert-map "C-j" #'unexpand-abbrev)
(keymap-set o-bray-state-normal-map "C-j" #'unexpand-abbrev)

(keymap-set o-bray-state-normal-map "C-f" #'scroll-up)
(keymap-set o-bray-state-normal-map "C-b" #'scroll-down)
;;;; visual map
;;;;; duplicate (different behavior)
(keymap-set o-bray-state-visual-map "d l" #'duplicate-dwim)
;;;;; expand-region
(keymap-set o-bray-state-visual-map "v" #'expreg-expand)
(keymap-set o-bray-state-visual-map "V" #'expreg-contract)
;;;;; exchanging keys
(keymap-set o-bray-state-normal-map "g a" #'transpose-mark-region)
(keymap-set o-bray-state-normal-map "g A" #'transpose-mark-region-abort)
;;;;; surround
(keymap-set o-bray-state-normal-map "S r" #'o-delim-wrap-round)
(keymap-set o-bray-state-visual-map "S c" #'o-delim-wrap-curly)
(keymap-set o-bray-state-visual-map "S s" #'o-delim-wrap-square)
(keymap-set o-bray-state-visual-map "S a" #'o-delim-wrap-angle)

(defvar-keymap o-surround-map
  "r" #'o-delim-wrap-round
  "c" #'o-delim-wrap-curly
  "s" #'o-delim-wrap-square
  "a" #'o-delim-wrap-angle)
;;;; insert map
(keymap-set o-bray-state-normal-map "C-v" #'rectangle-mark-mode)
(keymap-set o-bray-state-insert-map "C-j" #'o-inverse-add-abbrev)
(keymap-set o-bray-state-normal-map "C-j" #'o-inverse-add-abbrev)
(keymap-set o-bray-state-insert-map "<escape>" #'o-bray-dwim-escape)

(bray-state-map-set 'insert emacs-lisp-mode-map ";" #'lispy-comment)
(bray-state-map-set 'insert emacs-lisp-mode-map "SPC" #'lispy-space)
;;;; mimic folding in other packages
;; Configure Kirigami to replace the default Evil-mode folding key bindings
;; TODO: add outline navigation.  do not know if it should be its own state or if I should
;; just include bindings under a prefix.
(keymap-set o-bray-state-normal-map "z z" 'kirigami-toggle-fold)
(keymap-set o-bray-state-normal-map "z o" 'kirigami-open-fold)
(keymap-set o-bray-state-normal-map "z O" 'kirigami-open-fold-rec)
(keymap-set o-bray-state-normal-map "z c" 'kirigami-close-fold)
(keymap-set o-bray-state-normal-map "z a" 'kirigami-toggle-fold)
(keymap-set o-bray-state-normal-map "z r" 'kirigami-open-folds)
(keymap-set o-bray-state-normal-map "z m" 'kirigami-close-folds)
;;;; external packages
;; (o-bray-state-map-set 'normal 'eamcs-lisp-mode-map "x" #'lispy-delete)
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

(o-after vertico
  (bray-state-map-set 'insert vertico-map "<escape>" #'o-bray-dwim-escape)
  (bray-state-map-set 'insert vertico-map "C-j" #'vertico-next)
  (bray-state-map-set 'insert vertico-map "C-k" #'vertico-previous)
  (bray-state-map-set 'insert vertico-map "C-n" #'vertico-scroll-up)
  (bray-state-map-set 'insert vertico-map "C-p" #'vertico-scroll-down)
  (bray-state-map-set 'insert vertico-map "TAB" #'vertico-next)
  (bray-state-map-set 'insert vertico-map ";" #'vertico-quick-exit)
  (bray-state-map-set 'insert vertico-map "C-;" #'vertico-quick-exit)
  (bray-state-map-set 'insert vertico-map "<backtab>" #'vertico-previous)
  (bray-state-map-set 'insert vertico-map "C-o" #'embark-act))

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

(o-defafter o-after--bray-define-macrostep-binds (macrostep)
  (let ((prefixes (list o-key-localleader-normal o-key-localleader-normal-alt)))
    (dolist (prefix prefixes)
      (bray-state-map-set 'normal emacs-lisp-mode-map (concat prefix "\s" "m") nil)
      (bray-state-map-set 'normal emacs-lisp-mode-map (concat prefix "\s" "m e") #'macrostep-expand)
      (bray-state-map-set 'normal emacs-lisp-mode-map (concat prefix "\s" "m c") #'macrostep-collapse)
      (bray-state-map-set 'normal emacs-lisp-mode-map (concat prefix "\s" "m C") #'macrostep-collapse-all)
      (bray-state-map-set 'normal emacs-lisp-mode-map (concat prefix "\s" "m a") #'macrostep-collapse-all))))
;;; provide
(provide 'init-after-bray)
;;; init-after-bray.el ends here
