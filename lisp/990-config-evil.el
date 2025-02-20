;;; 990-config-evil.el --- evil configuration -*- lexical-binding: t; -*-
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
(require '050-base)
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
;;;; insert state hook
(defun oo--enter-evil-insert-state-maybe (&rest _)
  "Enter insert state if `evil-mode' is enabled."
  (when (bound-and-true-p evil-mode)
    (evil-insert-state 1)))
;;;; cross-configuration
;;;;; org-capture
(hook! org-capture-mode-hook oo--enter-evil-insert-state-maybe)
;;;;; git-commit
;; Note that I cannot use `evil-set-initial-state' for this because
;; `git-commit-mode' is a minor-mode.
(hook! git-commit-mode-hook oo--enter-evil-insert-state-maybe)
;;;;; denote
(hook! denote-after-new-note-hook oo--enter-evil-insert-state-maybe)
;;;;; corfu
;; When using evil, neither `corfu-map' nor `tempel-map' bindings will work
;; because the maps are overridden by evil.  In order for them to work, we need
;; to boost give the maps greater precedence.
(defun oo--corfu-normalize-evil-keymaps (&rest _)
  (evil-normalize-keymaps))

(defafter! oo-make-corfu-map-an-overriding-map (corfu)
  (evil-make-overriding-map corfu-map)
  (advice-add 'corfu--setup :after #'oo--corfu-normalize-evil-keymaps)
  (advice-add 'corfu--teardown :after #'oo--corfu-normalize-evil-keymaps))
;;;;; tempel
(defafter! oo-make-tempel-map-an-overriding-map (tempel)
  (evil-make-overriding-map tempel-map))

(advice-add 'tempel-insert :after #'oo--enter-evil-insert-state-maybe)

(defun oo--enter-evil-insert-state-maybe (&rest _)
  (when (bound-and-true-p evil-mode)
    (evil-insert-state 1)))
;;;;; auto-insert
;;;;; magit
;; Note that I cannot use `evil-set-initial-state' for this because
;; `git-commit-mode' is a minor-mode.
(hook! git-commit-mode-hook +evil-enter-insert-state-hook)
;;;; miscellaneous
(defun oo-dwim-escape ()
  "Exits out of whatever is happening after escape."
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
;;; provide
(provide '990-config-evil)
;;; 990-config-evil.el ends here
