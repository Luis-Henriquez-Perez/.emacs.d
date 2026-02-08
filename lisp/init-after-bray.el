;;; init-after-bray.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;;;; utilities
(defun o-meep-dwim-swap-selection ()
  "Do what I mean."
  (interactive "*")
  (cond ((secondary-selection-exist-p)
         (meep-region-swap))
        (t
         (meep-region-to-secondary-selection))))

(defun o-meep-cancel-secondary-selection ()
  (interactive)
  (delete-overlay mouse-secondary-overlay))

(defun o--meep-region-contextual-bounds (&optional inner prefixp)
  (sp-get (if (sp-point-in-string (point))
              (sp-get-string t)
            (sp-get-enclosing-sexp))
    (cons (if (and prefixp (not (string-empty-p :prefix)))
              (- :beg (length :prefix))
            :beg)
          :end)))

(defun o-meep-region-contextual-inner ()
  (interactive)
  (o-awhen (o--meep-region-contextual-bounds)
    (meep--region-mark-bounds-to-region it nil)))

(defun o-scroll-to-bottom ()
  "Scroll line to bottom of page."
  (interactive)
  (recenter -1))

(defun o-scroll-to-top ()
  "Scroll line to top of page."
  j wo
  (interactive)
  (recenter 1))
;;;; normal map
;;;;; digit args
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
;; Unlike the default to re-running N times.
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
;;;;; miscellaneous + - ; *
(keymap-set o-bray-state-normal-map "*" #'meep-isearch-at-point-next)

(keymap-set o-bray-state-normal-map "[" #'meep-isearch-at-point-prev)
(keymap-set o-bray-state-normal-map "]" #'meep-isearch-at-point-next)

(keymap-set o-bray-state-normal-map "<tab>" #'meep-indent-rigidly)
(keymap-set o-bray-state-normal-map "<escape>" #'o-bray-dwim-escape)

(keymap-set o-bray-state-normal-map "+" #'text-scale-increase)
(keymap-set o-bray-state-normal-map "-" #'text-scale-decrease)
(keymap-set o-bray-state-normal-map ";" #'execute-extended-command)
(keymap-set o-bray-state-normal-map o-key-leader-normal #'o-leader-map)

(keymap-set o-bray-state-normal-map "g g" #'beginning-of-buffer)
(keymap-set o-bray-state-normal-map "g h" #'beginning-of-buffer)
(keymap-set o-bray-state-normal-map "g G" #'end-of-buffer)
;; Fn just keep this similar to vim kbd
(keymap-set o-bray-state-normal-map "G" #'end-of-buffer)

(keymap-set o-bray-state-normal-map "<remap> <self-insert-command>" #'ignore)
;;;;; miscellaneous
(keymap-set o-bray-state-normal-map "q" #'meep-register-kmacro-start-or-end)
(keymap-set o-bray-state-normal-map "Q" #'o-bray-unbound-key)

(keymap-set o-bray-state-normal-map ":" #'meep-move-matching-bracket-outer)

(keymap-set o-bray-state-normal-map "'" #'meep-move-matching-syntax-inner)
(keymap-set o-bray-state-normal-map "\"" #'meep-move-matching-syntax-outer)
;;;;; movement w W e E r R h k j l
;; It is impractical to have a key for every single movm--there is alot.  So I
;; prioritize the most common movements that are useful in practically
;; every situation.
(keymap-set o-bray-state-normal-map "h" #'meep-move-char-prev)
(keymap-set o-bray-state-normal-map "H" #'meep-move-line-non-space-beginning)

(keymap-set o-bray-state-normal-map "j" #'meep-move-line-next)
;; Scrolling up and down is more useful than going down by paragraphs because
;; paragraphs can be of different lengths.
(keymap-set o-bray-state-normal-map "J" #'scroll-up)

(keymap-set o-bray-state-normal-map "k" #'meep-move-line-prev)
(keymap-set o-bray-state-normal-map "K" #'scroll-down)

(keymap-set o-bray-state-normal-map "l" #'meep-move-char-next)
(keymap-set o-bray-state-normal-map "L" #'meep-move-line-non-space-end)

(keymap-set o-bray-state-normal-map "w" #'meep-move-word-next)
(keymap-set o-bray-state-normal-map "W" #'meep-move-symbol-next)
(keymap-set o-bray-state-normal-map "e" #'meep-move-word-next-end)
(keymap-set o-bray-state-normal-map "E" #'meep-move-symbol-next-end)
(keymap-set o-bray-state-normal-map "r" #'meep-move-word-prev)
(keymap-set o-bray-state-normal-map "R" #'meep-move-symbol-prev-end)
;;;;; operate on region
;; with this binding I can active the inactive region and in combination with
;; the `o-bray-state-visual-map' keybinding I can reverse the location of the
;; active region.
;; TODO: idle load lispy in lisp modes
(keymap-set o-bray-state-normal-map "i" #'meep-region-activate-or-reverse)
;; Meep utilizes point and mark a lot and it is one of the major "themes" you
;; could say of.
(keymap-set o-bray-state-normal-map "s h" #'helpful-at-point)
(keymap-set o-bray-state-normal-map "s r" #'o-eval-and-replace-region)
(keymap-set o-bray-state-normal-map "s e" #'eval-region)
;; dupling a lne is so common that it is worth aving it is own keybinding.
;; note: imnsi I am going to keep using eshell.  I might replace it with mistty.
(keymap-set o-bray-state-normal-map "s w" #'duplicate-line)
;; The logic is that most of the time I will want to make lowercase text
;; upercase instead of the other way around, thus I want an easier binding to
;; press.
(keymap-set o-bray-state-normal-map "s u" #'upcase-region)
(keymap-set o-bray-state-normal-map "s U" #'downcase-region)
(keymap-set o-bray-state-normal-map "s d" #'downcase-region)
(keymap-set o-bray-state-normal-map "s g" #'duplicate-dwim)
(keymap-set o-bray-state-normal-map "s a" #'flyspell-region)
(keymap-set o-bray-state-normal-map "s s" #'meep-char-surround-insert)
(keymap-set o-bray-state-normal-map "s l" #'ispell-region)
(keymap-set o-bray-state-normal-map "s a" #'capitalize-region)
;; (keymap-set o-bray-state-normal-map "s R" #'narrow-to-region)
;; Uncommon therefore I give these keybindings the harder to press keys.
(keymap-set o-bray-state-normal-map "s R" #'rot13-region)
(keymap-set o-bray-state-normal-map "s m" #'unmorse-region)
(keymap-set o-bray-state-normal-map "s M" #'morse-region)
;;;;; selection
;; These are keys that select so-called "things".
;; Also have shortcuts for the selection.  this is a tradeoff.  on the onehand
;; the shortcuts are shorter but they may also be less mnemonic.
(keymap-set o-bray-state-normal-map "s w" #'meep-region-mark-word)
(keymap-set o-bray-state-normal-map "s j" #'meep-region-mark-word)
(keymap-set o-bray-state-normal-map "s s" #'meep-region-mark-symbol)
(keymap-set o-bray-state-normal-map "s d" #'meep-region-expand-to-line-bounds)
(keymap-set o-bray-state-normal-map "s l" #'meep-region-expand-to-line-bounds)
(keymap-set o-bray-state-normal-map "s ;" #'meep-region-mark-line-inner)
(keymap-set o-bray-state-normal-map "s t" #'meep-region-mark-sentence-inner)
(keymap-set o-bray-state-normal-map "s n" #'meep-region-mark-sentence-outer)
(keymap-set o-bray-state-normal-map "s g" #'meep-region-mark-defun-inner)
(keymap-set o-bray-state-normal-map "s c" #'meep-region-mark-comment-inner)
(keymap-set o-bray-state-normal-map "s f" #'meep-region-mark-bounds-of-char-contextual-inner)

(keymap-set o-bray-state-normal-map "s i w" #'meep-region-mark-word)
(keymap-set o-bray-state-normal-map "s i m" #'meep-region-mark-symbol)
(keymap-set o-bray-state-normal-map "s i j" #'meep-region-mark-symbol)
(keymap-set o-bray-state-normal-map "s i l" #'meep-region-mark-line-inner)
(keymap-set o-bray-state-normal-map "s i p" #'meep-region-mark-paragraph-inner)
(keymap-set o-bray-state-normal-map "s i s" #'meep-region-mark-sentence-inner)
(keymap-set o-bray-state-normal-map "s i c" #'meep-region-mark-comment-inner)
(keymap-set o-bray-state-normal-map "s i f" #'meep-region-mark-bounds-of-char-contextual-inner)

(keymap-set o-bray-state-normal-map "s o w" #'meep-region-mark-word)
(keymap-set o-bray-state-normal-map "s o m" #'meep-region-mark-symbol)
(keymap-set o-bray-state-normal-map "s o j" #'meep-region-mark-symbol)
(keymap-set o-bray-state-normal-map "s o l" #'meep-region-expand-to-line-bounds)
(keymap-set o-bray-state-normal-map "s o p" #'meep-region-mark-paragraph-outer)
(keymap-set o-bray-state-normal-map "s o n" #'meep-region-mark-sentence-outer)
(keymap-set o-bray-state-normal-map "s o c" #'meep-region-mark-comment-outer)
(keymap-set o-bray-state-normal-map "s o s" #'meep-region-mark-string-outer)
(keymap-set o-bray-state-normal-map "s o f" #'meep-region-mark-bounds-of-char-contextual-outer)
;;;;; insertion
;; This is the list of things I need to completely replace evil with meep.
;; 1. movement
;; 2. cut copy paste
;; 3. copy a line and paste it above
;; (keymap-set o-bray-state-normal-map "d g" #'grugru-forward)
(keymap-set o-bray-state-normal-map "d f" #'grugru-forward)

(defvar o-grugru-repeat-map
  (let ((map (make-sparse-keymap)))
    ;; (keymap-set map "g" #'grugru-forward)
    (keymap-set map "f" #'grugru-forward)
    map))

(put 'grugru-forward 'repeat-map 'o-grugru-repeat-map)

(keymap-set o-bray-state-normal-map "d j" #'meep-insert-open-below)
(keymap-set o-bray-state-normal-map "d k" #'meep-insert-open-above)
;; Make this keybinding similar to cutting a line.
(keymap-set o-bray-state-normal-map "d d" #'meep-clipboard-killring-cut-line)
(keymap-set o-bray-state-normal-map "d h" #'copy-region-as-kill-line)
;; Left Hand: Row 2.
;; Inserting
(keymap-set o-bray-state-normal-map "a a" #'meep-insert)
(keymap-set o-bray-state-normal-map "a s" #'meep-insert-append)
(keymap-set o-bray-state-normal-map "a j" #'meep-insert-open-below)
(keymap-set o-bray-state-normal-map "a k" #'meep-insert-open-above)
(keymap-set o-bray-state-normal-map "a l" #'meep-insert-append)
(keymap-set o-bray-state-normal-map "a h" #'meep-insert)
(keymap-set o-bray-state-normal-map "a L" #'meep-insert-line-end)
(keymap-set o-bray-state-normal-map "a H" #'meep-insert-line-beginning)
;;;;; deleting a character
(keymap-set o-bray-state-normal-map "x" #'meep-delete-char-next)
(keymap-set o-bray-state-normal-map "X" #'meep-insert-overwrite)
;;;;; searching
;; Right Hand: Row 3.
(keymap-set o-bray-state-normal-map "f j" #'meep-isearch-regexp-next)
(keymap-set o-bray-state-normal-map "f k" #'meep-isearch-regexp-prev)

(keymap-set o-bray-state-normal-map "n" #'meep-isearch-repeat-next)
(keymap-set o-bray-state-normal-map "N" #'meep-isearch-repeat-prev)

(keymap-set o-bray-state-normal-map "f m" #'meep-isearch-at-point-next)
(keymap-set o-bray-state-normal-map "f ," #'meep-isearch-at-point-prev)

(keymap-set o-bray-state-normal-map "f f" #'meep-move-find-char-on-line-at-prev)
(keymap-set o-bray-state-normal-map "f h" #'meep-move-find-char-on-line-at-prev)
(keymap-set o-bray-state-normal-map "f H" #'meep-move-find-char-on-line-till-prev)
(keymap-set o-bray-state-normal-map "f l" #'avy-goto-line)

(keymap-set o-bray-state-normal-map "f d" #'avy-goto-char)

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

;; Left Hand: Row 3.
;; (keymap-unset o-bray-state-normal-map "z" #'undo-only)
(keymap-set o-bray-state-normal-map "Z" #'undo-redo)
;; (keymap-set o-bray-state-normal-map "c" #'meep-delete-char-ring-next)
;; (keymap-set o-bray-state-normal-map "C" #'meep-delete-char-ring-prev)

(keymap-set o-bray-state-normal-map "v" #'meep-region-toggle)
(keymap-set o-bray-state-normal-map "V" #'meep-clipboard-killring-cut-line)

(keymap-set o-bray-state-normal-map "c" #'meep-insert-change)
(keymap-set o-bray-state-normal-map "b" #'meep-insert-change)
(keymap-set o-bray-state-normal-map "B" #'meep-insert-change-lines)

;; Right Hand: Row 1.
;; meep's variant does not seem to save it to the system clipboard properly.
(keymap-set o-bray-state-normal-map "y" #'copy-region-as-kill)
(keymap-set o-bray-state-normal-map "Y" #'meep-clipboard-only-copy)
;; "y" #'o-bray-unbound-key
;; "Y" #'o-bray-unbound-key

(keymap-set o-bray-state-normal-map "u" #'meep-clipboard-killring-cut)
(keymap-set o-bray-state-normal-map "U" #'meep-clipboard-only-cut)
;; "u" #'meep-exchange-point-and-mark
;; "U" #'o-bray-unbound-key

(keymap-set o-bray-state-normal-map "o" #'meep-region-mark-bounds-of-char-contextual-inner)
(keymap-set o-bray-state-normal-map "O" #'meep-region-mark-bounds-of-char-contextual-outer)

(keymap-set o-bray-state-normal-map "p" #'meep-clipboard-killring-yank)
(keymap-set o-bray-state-normal-map "P" #'point-to-register)

;; (keymap-unset o-bray-state-normal-map "m" #'o-bray-unbound-key)
(keymap-set o-bray-state-normal-map "M" #'o-bray-unbound-key)

(keymap-set o-bray-state-normal-map "," #'o-bray-unbound-key)
(keymap-set o-bray-state-normal-map "<" #'o-bray-unbound-key)

(keymap-set o-bray-state-normal-map "." #'meep-move-symbol-next)
(keymap-set o-bray-state-normal-map ">" #'meep-move-same-syntax-and-space-next)

(keymap-set o-bray-state-normal-map "/" #'meep-move-symbol-next-end)
(keymap-set o-bray-state-normal-map "?" #'meep-move-same-syntax-and-space-next-end)
;;;;; registers
(keymap-set o-bray-state-normal-map "d l" #'meep-register-jump-to)
;;;; visual map
;; Although optional I think that having a visual map is quite useful.
(keymap-set o-bray-state-visual-map "<escape>" #'o-bray-dwim-escape)
;;;;; copy, cut, paste
(keymap-set o-bray-state-visual-map "y" #'copy-region-as-kill)
(keymap-set o-bray-state-visual-map "Y" #'meep-clipboard-only-copy)
(keymap-set o-bray-state-visual-map "u" #'meep-clipboard-killring-cut)
(keymap-set o-bray-state-visual-map "U" #'meep-clipboard-only-cut)
(keymap-set o-bray-state-visual-map "i" #'meep-clipboard-killring-yank)
(keymap-set o-bray-state-visual-map "I" #'meep-clipboard-only-yank)
;;;;; comment
(keymap-set o-bray-state-visual-map "a" #'meep-region-activate-or-reverse)
(keymap-set o-bray-state-visual-map "A" #'copy-region-as-kill)
(keymap-set o-bray-state-visual-map "d" #'meep-clipboard-killring-cut)
;;;;; surround
;; TODO: change surround insert so that it does not create unlikely pairs.
(keymap-set o-bray-state-visual-map "S" #'meep-char-surround-insert)
(keymap-set o-bray-state-visual-map "g s" #'meep-char-surround-insert)
;;;;; miscellaneous utility
(keymap-set o-bray-state-visual-map "s h" #'helpful-at-point)
(keymap-set o-bray-state-visual-map "s r" #'o-eval-and-replace-region)
(keymap-set o-bray-state-visual-map "s e" #'eval-region)
(keymap-set o-bray-state-visual-map "s u" #'upcase-region)
(keymap-set o-bray-state-visual-map "s U" #'downcase-region)
(keymap-set o-bray-state-visual-map "s d" #'downcase-region)
(keymap-set o-bray-state-visual-map "s g" #'duplicate-dwim)
(keymap-set o-bray-state-visual-map "s a" #'flyspell-region)
(keymap-set o-bray-state-visual-map "s s" #'meep-char-surround-insert)

(keymap-set o-bray-state-visual-map "m" #'comment-or-uncomment-region)
(keymap-set o-bray-state-visual-map "R" #'o-eval-and-replace-region)
(keymap-set o-bray-state-visual-map "u" #'downcase-region)
(keymap-set o-bray-state-visual-map "U" #'upcase-region)
(keymap-set o-bray-state-visual-map "b" #'meep-insert-change)
(keymap-set o-bray-state-visual-map "i" #'meep-region-activate-or-reverse)
;;;;; swapping text
(keymap-set o-bray-state-normal-map "g j" #'scroll-up)
(keymap-set o-bray-state-normal-map "g k" #'scroll-down)
(keymap-set o-bray-state-visual-map "g a" #'o-meep-dwim-swap-selection)
;; Use this to cancel the secondary selection.
(keymap-set o-bray-state-visual-map "g A" #'meep-region-to-secondary-selection)
;;;; motion map
;; (keymap-set o-bray-state-motion-map "<remap> <self-insert-command>" #'o-bray-unbound-key)
(keymap-set o-bray-state-motion-map "C-f" #'scroll-up)
(keymap-set o-bray-state-motion-map "<escape>" #'o-bray-dwim-escape)
(keymap-set o-bray-state-motion-map "/" #'isearch-forward-regexp)
;;;; insert map
(keymap-set o-bray-state-insert-map "C-j" #'abbrev/inverse-add)
(keymap-set o-bray-state-insert-map "<escape>" #'o-bray-dwim-escape)

;; TODO: put a list of "lispy" modes into a list and loop through them to set
;; the same binds.
;; (dolist (mode o-lisp-modes)
;;   ())
(bray-state-map-set 'insert emacs-lisp-mode-map ";" #'lispy-comment)
(bray-state-map-set 'insert emacs-lisp-mode-map "SPC" #'lispy-space)
;; These are controversial bindings for a new mode.
;; (bray-state-map-set 'normal emacs-lisp-mode-map "j" #'lispy-down)
;; (bray-state-map-set 'normal emacs-lisp-mode-map "k" #'lispy-up)
;;;; mimik folding in other packages
;; Configure Kirigami to replace the default Evil-mode folding key bindings
;; TODO: add outline navigation.  do not know ifisb its own state or if I should
;; just include bindings under a prefix.
(keymap-set o-bray-state-normal-map "m m" #'recenter)
(keymap-set o-bray-state-normal-map "z z" #'recenter)
(keymap-set o-bray-state-normal-map "z j" #'o-scroll-to-bottom)
(keymap-set o-bray-state-normal-map "z k" #'o-scroll-to-top)
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
