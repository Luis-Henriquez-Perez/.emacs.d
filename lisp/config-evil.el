;;; config-evil.el --- evil configuration -*- lexical-binding: t; -*-
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
;; This is my configuration for evil.
;;
;;; Code:
(require 'base)
(require 'evil)
;;;; settings
;; By default =evil= displays the current state in the echo area.  I think some
;; indicator for the current state is necessary but I don't want to do it via
;; echoing.  Instead I plan to do it primarily via cursor colors; and possibly the
;; modeline as well.
(opt! evil-echo-state nil)

(opt! evil-move-cursor-back nil)

(opt! evil-move-beyond-eol nil)

(opt! evil-search-wrap nil)

(opt! evil-visualstar/persistent t)
;;;; go into insert state when typing in minibuffer
(defvar oo-evil-state-before-minibuffer nil
  "Store the evil state before entering the minibuffer.")

(defhook! oo-preserve-prior-evil-state-h (minibuffer-setup-hook)
  "Save state before entering the minibuffer and enter insert state."
  (when (bound-and-true-p evil-mode)
    (setq oo-evil-state-before-minibuffer evil-state)
    (evil-insert-state)))

(defhook! oo-restore-prior-evil-state-h (minibuffer-exit-hook)
  "Restore state after minibuffer."
  (when (bound-and-true-p evil-mode)
    (when oo-evil-state-before-minibuffer
      (evil-change-state oo-evil-state-before-minibuffer))
    (setq oo-evil-state-before-minibuffer nil)))

(defun! oo-refresh-evil-cursor-h (_)
  (when (bound-and-true-p evil-mode)
    (evil-refresh-cursor)))

(add-hook 'enable-theme-functions #'oo-refresh-evil-cursor-h)
;;;; define eval operators
;; This is shamelessly copied from `evil-extra-operator'.
(evil-define-operator +evil-eval-operator (beg end)
  "Evil operator for evaluating code."
  :move-point nil
  (interactive "<r>")
  (eval-region beg end t))

;; This is also shamelessly copied with the difference that the format string is
;; "%S" instead of "%s".  Honestly, I think not having it that way was a bug.
(evil-define-operator +evil-eval-replace-operator (beg end)
  "Evil operator for replacing contents with result from eval."
  :move-point nil
  (interactive "<r>")
  (let* ((text (buffer-substring-no-properties beg end))
         (result (format "%S" (eval (read text)))))
    (delete-region beg end)
    (insert result)))

(evil-define-operator +evil-eval-print-operator (beg end)
  "Evil operator for printing the results of contents below."
  :move-point nil
  (interactive "<r>")
  (let* ((text (buffer-substring-no-properties beg end))
         (result (format "\n=> %S" (eval (read text)))))
    (goto-char end)
    (alet! (point)
      (insert result)
      (comment-region it (point)))))
;;;; prevent cursor color from changing with eldoc
;; For some reason the cursor color changes with eldoc.  Here I tell.  This also
;; fixes the cursor color change when expanding a tempel snippet.
(advice-add 'elisp-eldoc-funcall :around #'+elisp-eldoc-funcall@preserve-cursor-color)
(defun! +elisp-eldoc-funcall@preserve-cursor-color (orig &rest args)
  (set! bg (face-attribute 'cursor :background))
  (prog1 (apply orig args)
    (unless (equal bg (face-attribute 'cursor :background))
      (set-cursor-color bg))))
;;;; change cursor color and shape according to current evil state
;; Did not realize for the longest time that evil cursor can be a function that
;; changes the cursor.  With this in mind, the best way to set the cursor size
;; and shape dynamically is to set the corresponding cursor symbols to functions.
(defun +evil--cursor-color (state)
  "Return the cursor color for state as a string."
  (cond ((bound-and-true-p telephone-line-mode)
         (face-attribute (intern (format "telephone-line-evil-%s" state)) :background nil t))
        ((facep (intern (format "spaceline-evil-%s" state)))
         (face-attribute (intern (format "spaceline-evil-%s" state)) :background nil t))
        (t
         (face-attribute 'cursor :background nil t))))

(defun +evil-normal-state-cursor ()
  "Set cursor for normal state."
  (evil-set-cursor (list t (+evil--cursor-color 'normal))))

(defun +evil-insert-state-cursor ()
  "Set cursor for insert state."
  (evil-set-cursor (list '(bar . 2) (+evil--cursor-color 'insert))))

(defun +evil-visual-state-cursor ()
  "Set cursor for visual state."
  (evil-set-cursor (list t (+evil--cursor-color 'visual))))

(defun +evil-motion-state-cursor ()
  "Set cursor for motion state."
  (evil-set-cursor (list t (+evil--cursor-color 'motion))))

(defun +evil-replace-state-cursor ()
  "Set cursor for replace state."
  (evil-set-cursor (list t (+evil--cursor-color 'replace))))

(defun +evil-operator-state-cursor ()
  "Set cursor for operator state."
  (evil-set-cursor (list '(hbar . 9) (+evil--cursor-color 'operator))))

(defun +evil-emacs-state-cursor ()
  "Set cursor for emacs state."
  (evil-set-cursor (list t (+evil--cursor-color 'emacs))))

(opt! evil-normal-state-cursor   #'+evil-normal-state-cursor)
(opt! evil-insert-state-cursor   #'+evil-insert-state-cursor)
(opt! evil-visual-state-cursor   #'+evil-visual-state-cursor)
(opt! evil-motion-state-cursor   #'+evil-motion-state-cursor)
(opt! evil-replace-state-cursor  #'+evil-replace-state-cursor)
(opt! evil-operator-state-cursor #'+evil-operator-state-cursor)
(opt! evil-emacs-state-cursor    #'+evil-emacs-state-cursor)
;;;; make the visual selection face the same as the visual cursor color
;;;; insert state hook
(defun +evil-enter-insert-state-h ()
  "Enter insert state if `evil-mode' is enabled."
  (when (bound-and-true-p evil-mode)
    (evil-insert-state 1)))
;;;; cross-configuration
;;;;; org-capture
(hook! org-capture-mode-hook +evil-enter-insert-state-h)
;;;;; git-commit
;; Note that I cannot use `evil-set-initial-state' for this because
;; `git-commit-mode' is a minor-mode.
(hook! git-commit-mode-hook +evil-enter-insert-state-h)
;;;;; denote
(hook! denote-after-new-note-hook +evil-enter-insert-state-h)
;;;;; corfu
;; When using evil, neither `corfu-map' nor `tempel-map' bindings will work
;; because the maps are overridden by evil.  In order for them to work, we need
;; to boost give the maps greater precedence.
(defafter! oo-make-corfu-map-an-overriding-map (corfu)
  (evil-make-overriding-map corfu-map)
  (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
  (advice-add 'corfu--teardown :after 'evil-normalize-keymaps))
;;;;; tempel
(defafter! oo-make-tempel-map-an-overriding-map (tempel)
  (evil-make-overriding-map tempel-map))
;;;;; auto-insert
;;;;; magit
;; Note that I cannot use `evil-set-initial-state' for this because
;; `git-commit-mode' is a minor-mode.
(hook! git-commit-mode-hook +evil-enter-insert-state-hook)
;;; provide
(provide 'config-evil)
;;; config-evil.el ends here
