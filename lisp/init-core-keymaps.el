;;; init-core-keymaps.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
(defvar-keymap o-keymap-state-normal
  ;; 0..9
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
  ;; +-;*
  "<escape>" #'o-dwim-escape
  "<tab>" #'outline-toggle-children
  "*" #'meep-isearch-at-point-next
  "+" #'text-scale-increase
  "-" #'text-scale-decrease
  ";" #'execute-extended-command
  o-key-leader-normal #'o-prefix-command-leader
  ":" #'meep-move-matching-bracket-outer
  "<remap> <self-insert-command>" #'ignore

  "[" #'meep-isearch-at-point-prev
  "]" #'meep-isearch-at-point-next
  ;; q - keyboard macro
  "q" #'o-dwim-kmacro-start-or-end
  "Q" #'repeat-complex-command
  ;; werhjkl - movement
  "w" #'meep-move-word-next
  "W" #'meep-move-symbol-next
  "e" #'meep-move-word-next-end
  "E" #'meep-move-symbol-next-end
  "r" #'meep-move-word-prev
  "R" #'meep-move-symbol-prev

  "h" #'meep-move-char-prev
  "H" #'meep-move-line-non-space-beginning

  "j" #'meep-move-line-next
  "J" #'scroll-up

  "k" #'meep-move-line-prev
  "K" #'scroll-down

  "l" #'meep-move-char-next
  "L" #'meep-move-line-non-space-end
  ;; t - next/previous jumplist
  "t" #'better-jumper-jump-backward
  "T" #'better-jumper-jump-forward
  ;; y - copy
  "y" #'copy-region-as-kill
  ;; I want a line variant here.
  "Y" #'undefined
  ;; Have - visual selection
  "v" #'meep-region-toggle
  "V" #'expreg-expand
  ;; p - paste
  "p" #'yank
  "P" #'point-to-register
  ;; g - miscellaneous
  "g x" #'transpose-mark-region
  "g X" #'transpose-mark-region-abort
  "g a" #'transpose-mark-region
  "g A" #'transpose-mark-region-abort
  "g g" #'beginning-of-buffer
  "g h" #'beginning-of-buffer
  "g G" #'end-of-buffer
  "g i" #'kmacro-insert-counter
  "G" #'end-of-buffer
  ;; s - mark (select)
  "s k" #'meep-region-mark-char-outer
  "s w" #'meep-region-mark-word
  "s j" #'meep-region-mark-word
  "s s" #'meep-region-mark-symbol
  "s m" #'meep-region-mark-symbol
  "s l" #'meep-region-mark-line-outer
  "s L" #'meep-region-mark-line-outer
  "s ;" #'meep-region-mark-line-inner
  "s r" #'meep-region-mark-string-inner
  "s S" #'meep-region-mark-string-outer
  "s t" #'meep-region-mark-sentence-inner
  "s T" #'meep-region-mark-sentence-outer
  "s d" #'meep-region-mark-defun-inner
  "s D" #'meep-region-mark-defun-outer
  "s c" #'meep-region-mark-comment-inner
  "s C" #'meep-region-mark-comment-outer
  "s p" #'meep-region-mark-paragraph-inner
  "s P" #'meep-region-mark-paragraph-outer
  "s f" #'meep-region-mark-bounds-of-char-contextual-inner
  "s F" #'meep-region-mark-bounds-of-char-contextual-outer

  "s i w" #'meep-region-mark-word
  "s i m" #'meep-region-mark-symbol
  "s i j" #'meep-region-mark-symbol
  "s i l" #'meep-region-mark-line-inner
  "s i p" #'meep-region-mark-paragraph-inner
  "s i t" #'meep-region-mark-sentence-inner
  "s i c" #'meep-region-mark-comment-inner
  "s i d" #'meep-region-mark-defun-inner
  "s i r" #'meep-region-mark-string-inner
  "s i f" #'meep-region-mark-bounds-of-char-contextual-inner

  "s o w" #'meep-region-mark-word
  "s o m" #'meep-region-mark-symbol
  "s o s" #'meep-region-mark-symbol
  "s o j" #'meep-region-mark-word
  "s o l" #'meep-region-mark-line-outer
  "s o p" #'meep-region-mark-paragraph-outer
  "s o t" #'meep-region-mark-sentence-outer
  "s o c" #'meep-region-mark-comment-outer
  "s o r" #'meep-region-mark-string-outer
  "s o f" #'meep-region-mark-bounds-of-char-contextual-outer
  ;; i - invert point and mark
  "i" #'meep-region-activate-or-reverse
  "I" #'iedit-mode
  ;; o - mark contextual delimiters
  "o" #'meep-region-mark-bounds-of-char-contextual-inner
  "O" #'meep-region-mark-bounds-of-char-contextual-outer
  ;; a - insert
  "A" #'meep-insert-line-end
  "a a" #'meep-insert
  "a s" #'meep-insert-append
  "a j" #'meep-insert-open-below
  "a k" #'meep-insert-open-above
  "a l" #'meep-insert-append
  "a h" #'meep-insert
  "a L" #'meep-insert-line-end
  "a ;" #'meep-insert-line-end
  "a H" #'meep-insert-line-beginning
  ;; d - act on region
  "d a" #'o-expand-region-abbrevs-no-query
  ;; Maintain a similarity with vim's "dd"
  "d ;" #'iedit-mode
  "d S" #'sort-lines
  "d U" #'upcase-region
  "d c" #'capitalize-region
  "d d" #'kill-whole-line
  "d e" #'eval-region
  "d h" #'helpful-at-point
  "d j" #'eval-region
  "d k" #'comment-or-uncomment-region
  "d l" #'duplicate-dwim
  "d n" #'o-dwim-narrow-or-widen
  "d o" #'open-line
  "d r" #'o-eval-and-replace-region
  "d t" #'transpose-sexps
  "d u" #'downcase-region
  "d y" #'flyspell-region
  "d i" #'eval-defun
  "d w" #'widen
  ;; Uncommon therefore I give these keybindings the harder to press keys.
  "d R" #'rot13-region
  "d m" #'unmorse-region
  "d M" #'morse-region
  "d f" #'grugru-forward
  "d g" #'grugru-forward
  "D" #'kill-whole-line
  ;; f - search
  "f" #'meep-move-find-char-on-line-at-next
  "F" #'meep-move-find-char-on-line-at-prev
  ;; x - delete
  "x" #'kill-region
  "X" #'delete-region
  ;; c -change
  "c" #'meep-insert-change
  "C" #'meep-insert-change-lines
  ;; m - page
  "m m" #'recenter
  "m j" #'o-scroll-to-bottom
  "m t" #'o-scroll-to-top
  "m k" #'o-scroll-to-top

  "M" #'transpose-sexps
  ;; u - undo
  "u" #'undo-only
  "U" #'undo-redo

  "/" #'meep-isearch-regexp-next
  "?" #'meep-isearch-regexp-prev

  "n" #'meep-isearch-repeat-next
  "N" #'meep-isearch-repeat-prev

  "b" #'switch-to-buffer
  "B" #'meep-isearch-at-point-prev
  "'" #'puni-splice

  "\"" #'undefined

  ;; Do not do anything to "," because it is used for local leader.
  ;; "," #'undefined
  "<" #'join-line

  "." #'kmacro-call-macro
  ">" #'split-line

  "C-j" #'unexpand-abbrev

  "C-f" #'scroll-up
  "C-b" #'scroll-down)

(defvar-keymap o-keymap-state-visual
  "C-v" #'rectangle-mark-mode
  "v" #'expreg-expand
  "V" #'expreg-contract
  "s" #'o-prefix-command-delim-surround
  "S" #'o-prefix-command-delim-surround)

(defvar-keymap o-keymap-delim-surround
  :prefix 'o-prefix-command-delim-surround
  ")" #'o-delim-wrap-round
  "(" #'o-delim-wrap-round
  "f" #'o-delim-wrap-round
  "r" #'o-delim-wrap-round
  "}" #'o-delim-wrap-curly
  "{" #'o-delim-wrap-curly
  "c" #'o-delim-wrap-curly
  "[" #'o-delim-wrap-square
  "]" #'o-delim-wrap-square
  "s" #'o-delim-wrap-square
  "<" #'o-delim-wrap-angle
  ">" #'o-delim-wrap-angle
  "a" #'o-delim-wrap-angle
  "'" #'o-delim-wrap-single-quote
  "\"" #'o-delim-wrap-double-quote
  "d" #'o-delim-wrap-double-quote)

(defvar-keymap o-keymap-state-insert
  "C-v" #'rectangle-mark-mode
  "C-j" #'o-abbrev-inverse-add
  "C-k" #'unexpand-abbrev
  "<escape>" #'o-dwim-escape)

(defvar-keymap o-keymap-leader-window
  :prefix 'o-prefix-command-leader-window
  "D" #'delete-other-windows
  "M" #'maximize-window
  "S" #'burly-bookmark-windows
  "b" #'balance-windows
  "d" #'delete-window
  "h" #'split-window-vertically
  "j" #'ace-window
  "k" #'display-buffer
  "o" #'ace-window
  "s" #'ace-swap-window
  "t" #'transpose-frame
  "u" #'winner-undo
  "v" #'split-window-horizontally
  "w" #'ace-window)

(defvar-keymap o-keymap-leader-git
  :prefix 'o-prefix-command-leader-git
  "B" #'magit-branch
  "b" #'vc-switch-branch
  "c" #'magit-commit
  "g" #'magit-status
  ;; "j" #'o-dwim-vc-action
  "l" #'vc-switch-branch
  ;; "n" #'o-dwim-vc-action
  "p" #'magit-push
  "r" #'vc-register
  "s" #'magit-status)

(defvar-keymap o-keymap-leader-toggle
  :prefix 'o-prefix-command-leader-toggle
  "c" #'blink-cursor-mode
  "g" #'grugru
  "s" #'smartparens-mode
  "r" #'o-random-load-theme
  "t" #'load-theme
  "h" #'whitespace-mode
  "W" #'whitespace-mode
  "w" #'widen
  "l" #'display-line-numbers-mode
  "u" #'toggle-truncate-lines
  "n" #'o-dwim-narrow
  "e" #'eval-expression
  "f" #'o-emacs-set-font-face
  "d" #'toggle-debug-on-error
  "S" #'profiler-start
  "P" #'profiler-stop)

(defvar-keymap o-keymap-leader-help
  :prefix 'o-prefix-command-leader-help
  "m" #'describe-mode
  "l" #'describe-function
  "f" #'describe-function
  "j" #'describe-variable
  "v" #'describe-variable
  "h" #'describe-variable
  "c" #'describe-char
  "C" #'describe-char
  "k" #'describe-key
  "a" #'describe-face
  "F" #'describe-face)

(defvar-keymap o-keymap-leader-buffer
  :prefix 'o-prefix-command-leader-buffer
  "X" #'kill-current-buffer
  "K" #'kill-current-buffer
  "x" #'bury-buffer
  "b" #'switch-to-buffer
  "j" #'next-buffer
  "k" #'previous-buffer)

(defvar-keymap o-keymap-leader-find
  :prefix 'o-prefix-command-leader-find
  "t" #'tab-switch
  ";" #'save-buffer
  "f" #'find-file
  "E" #'o-emacs-open-config
  "I" #'o-emacs-open-init-file
  "L" #'o-emacs-open-lisp-dir
  "G" #'rgrep
  "p" #'consult-yank-pop
  "k" #'consult-bookmark
  "l" #'consult-line
  "h" #'consult-outline
  "g" #'consult-grep
  "r" #'consult-register
  "z" #'ace-link
  "b" #'burly-open-bookmark
  "i" #'imenu
  "n" #'o-new-buffer
  "w" #'switch-to-buffer
  "o" #'occur
  "a" #'find-library
  "d" #'pop-to-buffer)

(defvar-keymap o-keymap-leader-quit
  :prefix 'o-prefix-command-leader-quit
  "R" #'restart-emacs
  "E" #'restart-emacs-start-new-emacs
  "r" #'restart-emacs
  "k" #'o-emacs-kill-no-errors
  "Q" #'o-emacs-kill-no-hook
  "q" #'save-buffers-kill-emacs)

(defvar-keymap o-keymap-leader-package
  :prefix 'o-prefix-command-leader-package
  "l" #'elpaca-log
  "i" #'elpaca-try
  "d" #'elpaca-delete)

(defvar-keymap o-keymap-leader-app
  :prefix 'o-prefix-command-leader-app
  "E" #'restart-emacs-start-new-emacs
  "d" #'dired-jump
  "j" #'org-capture|todo
  "n" #'notmuch
  "e" #'eshell
  "m" #'mistty
  "f" #'elfeed)

(defvar-keymap o-keymap-leader
  :prefix 'o-prefix-command-leader
  "SPC" #'execute-extended-command
  "a" #'o-prefix-command-leader-app
  "b" #'o-prefix-command-leader-buffer
  "f" #'o-prefix-command-leader-find
  "g" #'o-prefix-command-leader-git
  "h" #'o-prefix-command-leader-help
  "j" #'o-prefix-command-leader-emacs
  "p" #'o-prefix-command-leader-package
  "t" #'o-prefix-command-leader-toggle
  "w" #'o-prefix-command-leader-window
  "q" #'o-prefix-command-leader-quit
  "l" #'consult-buffer
  "y" #'o-emacs-random-load-theme
  "s" #'o-emacs-random-load-theme
  "d" #'transwin-toggle)
;;; provide
(provide 'init-core-keymaps)
;;; init-core-keymaps.el ends here
