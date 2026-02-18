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
;;;; utilities
;; kmacro-end-and-call-macro
(defun o-meep-dwim-swap-selection ()
  "Do what I mean."
  (interactive "*")
  (cond ((secondary-selection-exist-p)
         (meep-region-swap))
        (t
         (meep-region-to-secondary-selection))))

(defun o-meep-cancel-secondary-selection ()
  "Cancel secondary selection."
  (interactive)
  (delete-overlay mouse-secondary-overlay))

;; right now this is the best function to select the inner bounds.
(defun o--meep-region-contextual-bounds (&optional inner)
  "Get bounds of contextual string or sexp.
This function is based on `evil-cleverparens'."
  (sp-get (if (sp-point-in-string (point))
              (sp-get-string t)
            (sp-get-enclosing-sexp))
    (if inner (cons (1+ :beg) (1- :end)) (cons :beg :end))))

(defun o-meep-region-contextual-inner ()
  ""
  (interactive)
  (o-awhen (o--meep-region-contextual-bounds 'inner)
    (meep--region-mark-bounds-to-region it nil)))

(defun o-meep-region-contextual-outer ()
  ""
  (interactive)
  (o-awhen (o--meep-region-contextual-bounds nil)
    (meep--region-mark-bounds-to-region it nil)))

(defun o-scroll-to-bottom ()
  "Scroll line to bottom of page."
  (interactive)
  (recenter -1))

(defun o-scroll-to-top ()
  "Scroll line to top of page."
  (interactive)
  (recenter 1))

(defun o-eval-region (beg end)
  "Same as `eval-region'."
  (interactive "r")
  (unless executing-kbd-macro
    (pulse-momentary-highlight-region beg end))
  (eval-region beg end))

;; meep's variant does not seem to save it to the system clipboard properly.
;; I know that `kill-ring-save' does also give visual indication but in my
;; opinion its indication is not very good (it is like a little pause and the
;; cursor going to beg and end)
(defun o-copy-region-as-kill (beg end)
  "Same as `copy-region-as-kill' but give visual feedback."
  (interactive "r")
  (unless executing-kbd-macro
    (pulse-momentary-highlight-region beg end))
  (copy-region-as-kill beg end))

(defun o-kmacro-start-or-end ()
  "Start kboard macro if not started othw."
  (interactive)
  (cond (defining-kbd-macro
         (kmacro-end-macro nil))
        (t
         (kmacro-start-macro nil))))

(defun o-meep-region-mark-character ()
  "Mark the character after point."
  (interactive)
  (unless (eobp)
    (set-mark (point))
    (forward-char 1)
    (activate-mark)))
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
;;;;; + - ; *
(keymap-set o-bray-state-normal-map "<escape>" #'o-bray-dwim-escape)
(keymap-set o-bray-state-normal-map "<tab>" #'outline-toggle-children)

(keymap-set o-bray-state-normal-map "*" #'meep-isearch-at-point-next)

(keymap-set o-bray-state-normal-map "+" #'text-scale-increase)
(keymap-set o-bray-state-normal-map "-" #'text-scale-decrease)
(keymap-set o-bray-state-normal-map ";" #'execute-extended-command)
(keymap-set o-bray-state-normal-map o-key-leader-normal #'o-leader-map)
(keymap-set o-bray-state-normal-map ":" #'meep-move-matching-bracket-outer)
(keymap-set o-bray-state-normal-map "'" #'o-bray-unbound-key)
(keymap-set o-bray-state-normal-map "\"" #'o-bray-unbound-key)

(keymap-set o-bray-state-normal-map "<remap> <self-insert-command>" #'ignore)

(keymap-set o-bray-state-normal-map "[" #'meep-isearch-at-point-prev)
(keymap-set o-bray-state-normal-map "]" #'meep-isearch-at-point-next)
;;;;; werthjkl
(keymap-set o-bray-state-normal-map "w" #'meep-move-word-next)
(keymap-set o-bray-state-normal-map "W" #'meep-move-symbol-next)
(keymap-set o-bray-state-normal-map "e" #'meep-move-word-next-end)
(keymap-set o-bray-state-normal-map "E" #'meep-move-symbol-next-end)
(keymap-set o-bray-state-normal-map "r" #'meep-move-word-prev)
(keymap-set o-bray-state-normal-map "R" #'meep-move-symbol-prev-end)
(keymap-set o-bray-state-normal-map "t" #'meep-char-replace)
(keymap-set o-bray-state-normal-map "T" #'o-bray-unbound-key)

(keymap-set o-bray-state-normal-map "h" #'meep-move-char-prev)
(keymap-set o-bray-state-normal-map "H" #'meep-move-line-non-space-beginning)

(keymap-set o-bray-state-normal-map "j" #'meep-move-line-next)
(keymap-set o-bray-state-normal-map "J" #'scroll-up)

(keymap-set o-bray-state-normal-map "k" #'meep-move-line-prev)
(keymap-set o-bray-state-normal-map "K" #'scroll-down)

(keymap-set o-bray-state-normal-map "l" #'meep-move-char-next)
(keymap-set o-bray-state-normal-map "L" #'meep-move-line-non-space-end)
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
(keymap-set o-bray-state-normal-map "s k" #'o-meep-region-mark-character)
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
(keymap-set o-bray-state-normal-map "s f" #'o-meep-region-contextual-inner)
(keymap-set o-bray-state-normal-map "s F" #'o-meep-region-contextual-outer)

(keymap-set o-bray-state-normal-map "s i w" #'meep-region-mark-word)
(keymap-set o-bray-state-normal-map "s i m" #'meep-region-mark-symbol)
(keymap-set o-bray-state-normal-map "s i j" #'meep-region-mark-symbol)
(keymap-set o-bray-state-normal-map "s i l" #'meep-region-mark-line-inner)
(keymap-set o-bray-state-normal-map "s i p" #'meep-region-mark-paragraph-inner)
(keymap-set o-bray-state-normal-map "s i t" #'meep-region-mark-sentence-inner)
(keymap-set o-bray-state-normal-map "s i c" #'meep-region-mark-comment-inner)
(keymap-set o-bray-state-normal-map "s i d" #'meep-region-mark-defun-inner)
(keymap-set o-bray-state-normal-map "s i r" #'meep-region-mark-string-inner)
(keymap-set o-bray-state-normal-map "s i f" #'o-meep-region-contextual-inner)

(keymap-set o-bray-state-normal-map "s o w" #'meep-region-mark-word)
(keymap-set o-bray-state-normal-map "s o m" #'meep-region-mark-symbol)
(keymap-set o-bray-state-normal-map "s o s" #'meep-region-mark-symbol)
(keymap-set o-bray-state-normal-map "s o j" #'meep-region-mark-symbol)
(keymap-set o-bray-state-normal-map "s o l" #'meep-region-expand-to-line-bounds)
(keymap-set o-bray-state-normal-map "s o p" #'meep-region-mark-paragraph-outer)
(keymap-set o-bray-state-normal-map "s o t" #'meep-region-mark-sentence-outer)
(keymap-set o-bray-state-normal-map "s o c" #'meep-region-mark-comment-outer)
(keymap-set o-bray-state-normal-map "s o r" #'meep-region-mark-string-outer)
(keymap-set o-bray-state-normal-map "s o f" #'o-meep-region-contextual-outer)
;;;;; I - invert point and mark
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
(keymap-set o-bray-state-normal-map "a H" #'meep-insert-line-beginning)
;;;;; d - act on region
(keymap-set o-bray-state-normal-map "d a" #'o-expand-region-abbrevs-no-query)
;; maintain a similarity with vim's "dd"
(keymap-set o-bray-state-normal-map "d d" #'kill-whole-line)
(keymap-set o-bray-state-normal-map "d j" #'o-eval-region)
(keymap-set o-bray-state-normal-map "d k" #'comment-or-uncomment-region)
(keymap-set o-bray-state-normal-map "d l" #'duplicate-line)
(keymap-set o-bray-state-normal-map "d r" #'o-eval-and-replace-region)
(keymap-set o-bray-state-normal-map "d ;" #'iedit-mode)
(keymap-set o-bray-state-normal-map "d h" #'helpful-at-point)
(keymap-set o-bray-state-normal-map "d o" #'sort-lines)
(keymap-set o-bray-state-normal-map "d e" #'o-eval-region)
(keymap-set o-bray-state-normal-map "d n" #'narrow-to-region)
(keymap-set o-bray-state-normal-map "d u" #'upcase-region)
(keymap-set o-bray-state-normal-map "d U" #'downcase-region)
(keymap-set o-bray-state-normal-map "d f" #'flyspell-region)
(keymap-set o-bray-state-normal-map "d s" #'meep-char-surround-insert)
(keymap-set o-bray-state-normal-map "d w" #'widen)
;; Uncommon therefore I give these keybindings the harder to press keys.
(keymap-set o-bray-state-normal-map "d R" #'rot13-region)
(keymap-set o-bray-state-normal-map "d m" #'unmorse-region)
(keymap-set o-bray-state-normal-map "d M" #'morse-region)
(keymap-set o-bray-state-normal-map "d g" #'grugru-forward)

(defvar o-grugru-repeat-map
  (let ((map (make-sparse-keymap)))
    ;; (keymap-set map "g" #'grugru-forward)
    (keymap-set map "f" #'grugru-forward)
    map))

(put 'grugru-forward 'repeat-map 'o-grugru-repeat-map)
;;;;; f - search
(keymap-set o-bray-state-normal-map "f f" #'flash-jump)

(keymap-set o-bray-state-normal-map "f j" #'meep-isearch-regexp-next)
(keymap-set o-bray-state-normal-map "f k" #'meep-isearch-regexp-prev)

(keymap-set o-bray-state-normal-map "n" #'meep-isearch-repeat-next)
(keymap-set o-bray-state-normal-map "N" #'meep-isearch-repeat-prev)

(keymap-set o-bray-state-normal-map "f m" #'meep-isearch-at-point-next)
(keymap-set o-bray-state-normal-map "f ," #'meep-isearch-at-point-prev)

(keymap-set o-bray-state-normal-map "f d" #'meep-move-find-char-on-line-at-prev)
(keymap-set o-bray-state-normal-map "f h" #'meep-move-find-char-on-line-at-prev)
(keymap-set o-bray-state-normal-map "f H" #'meep-move-find-char-on-line-till-prev)
(keymap-set o-bray-state-normal-map "f l" #'avy-goto-line)

;; (keymap-set o-bray-state-normal-map "f l" #'meep-move-find-char-on-line-at-next)
(keymap-set o-bray-state-normal-map "f L" #'meep-move-find-char-on-line-till-next)

;; Find "repeat" are below the keys for find.
(keymap-set o-bray-state-normal-map "f ." #'meep-move-find-char-on-line-repeat-at-next)
(keymap-set o-bray-state-normal-map "f n" #'meep-move-find-char-on-line-repeat-at-prev)
(keymap-set o-bray-state-normal-map "f >" #'meep-move-find-char-on-line-repeat-till-next)
(keymap-set o-bray-state-normal-map "f N" #'meep-move-find-char-on-line-repeat-till-prev)

(keymap-set o-bray-state-normal-map "f u" #'avy-goto-char)
(keymap-set o-bray-state-normal-map "f i" #'avy-goto-symbol-1-above)

(keymap-set o-bray-state-normal-map "F" #'o-bray-unbound-key)
;;;;; x - delete
(keymap-set o-bray-state-normal-map "x" #'kill-region)
(keymap-set o-bray-state-normal-map "X" #'delete-region)
;;;;; m - page
(keymap-set o-bray-state-normal-map "m m" #'recenter)
(keymap-set o-bray-state-normal-map "m j" #'o-scroll-to-bottom)
(keymap-set o-bray-state-normal-map "m k" #'o-scroll-to-top)
;;;;; u - undo
(keymap-set o-bray-state-normal-map "u" #'undo-only)
(keymap-set o-bray-state-normal-map "U" #'undo-redo)
;;;;; c - change
(keymap-set o-bray-state-normal-map "c" #'meep-insert-change)
(keymap-set o-bray-state-normal-map "C" #'meep-insert-change-lines)
(keymap-set o-bray-state-normal-map "b" #'meep-insert-change)
(keymap-set o-bray-state-normal-map "B" #'meep-insert-change-lines)
;;;;; y
;;;;; /
(keymap-set o-bray-state-normal-map "/" #'isearch-forward-regexp)
(keymap-set o-bray-state-normal-map "?" #'isearch-backward-regexp)
;;;;; other
(keymap-set o-bray-state-normal-map "z" #'undo-only)
(keymap-set o-bray-state-normal-map "Z" #'undo-redo)

(keymap-set o-bray-state-normal-map "v" #'meep-region-toggle)
(keymap-set o-bray-state-normal-map "V" #'meep-clipboard-killring-cut-line)

(keymap-set o-bray-state-normal-map "y" #'o-copy-region-as-kill)
(keymap-set o-bray-state-normal-map "Y" #'meep-clipboard-only-copy)

(keymap-set o-bray-state-normal-map "o" #'o-meep-region-contextual-inner)
(keymap-set o-bray-state-normal-map "O" #'o-meep-region-contextual-outer)

(keymap-set o-bray-state-normal-map "p" #'yank)
(keymap-set o-bray-state-normal-map "P" #'point-to-register)

;; (keymap-unset o-bray-state-normal-map "m" #'o-bray-unbound-key)
(keymap-set o-bray-state-normal-map "M" #'o-bray-unbound-key)

(keymap-set o-bray-state-normal-map "," #'o-bray-unbound-key)
(keymap-set o-bray-state-normal-map "<" #'o-bray-unbound-key)

(keymap-set o-bray-state-normal-map "." #'meep-move-symbol-next)
(keymap-set o-bray-state-normal-map ">" #'meep-move-same-syntax-and-space-next)

(keymap-set o-bray-state-normal-map "?" #'meep-move-same-syntax-and-space-next-end)
(keymap-set o-bray-state-normal-map "C-j" #'unexpand-abbrev)
;;;; visual map
;;;;; exchanging keys
(keymap-set o-bray-state-visual-map "g a" #'o-meep-dwim-swap-selection)
(keymap-set o-bray-state-visual-map "g A" #'meep-region-to-secondary-selection)
;;;;; surround
(keymap-set o-bray-state-visual-map "S" #'meep-char-surround-insert)
;;;; insert map
(keymap-set o-bray-state-normal-map "C-v" #'rectangle-mark-mode)
(keymap-set o-bray-state-insert-map "C-j" #'abbrev/inverse-add)
(keymap-set o-bray-state-insert-map "<escape>" #'o-bray-dwim-escape)

(bray-state-map-set 'insert emacs-lisp-mode-map ";" #'lispy-comment)
(bray-state-map-set 'insert emacs-lisp-mode-map "SPC" #'lispy-space)
;;;; mimic folding in other packages
;; Configure Kirigami to replace the default Evil-mode folding key bindings
;; TODO: add outline navigation.  do not know if it should be its own state or if I should
;; just include bindings under a prefix.
;; (keymap-set o-bray-state-normal-map "z o" 'kirigami-open-fold)
;; (keymap-set o-bray-state-normal-map "z O" 'kirigami-open-fold-rec)
;; (keymap-set o-bray-state-normal-map "z c" 'kirigami-close-fold)
;; (keymap-set o-bray-state-normal-map "z a" 'kirigami-toggle-fold)
;; (keymap-set o-bray-state-normal-map "z r" 'kirigami-open-folds)
;; (keymap-set o-bray-state-normal-map "z m" 'kirigami-close-folds)
;;;; external packages
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
