;;; init-evil.el --- initialize evil -*- lexical-binding: t; -*-
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
;; Initialize evil.
;;
;;; Code:
;;;; requirements
(require 'base)
;;;; main
(defhook! oo-load-evil-h (after-init-hook :depth 10)
  (require 'evil nil t))

(hook! emacs-startup-hook evil-mode)

;; Must be set before evil is loaded.
(setq evil-want-keybinding nil)

;; To ensure that =oo-override-mode-map= takes priority over evil states, we need
;; to make it an intercept map for all evil states.  In evil, intercept maps are
;; maps that take priority (intercept) evil bindings when they have a different
;; binding for the same key (this is opposed to =overriding-maps=, which completely
;; override an evil keymap).
;; By default =evil= displays the current state in the echo area.  I think some
;; indicator for the current state is necessary but I don't want to do it via
;; echoing.  Instead I plan to do it primarily via cursor colors; and possibly the
;; modeline as well.
(opt! evil-echo-state nil)
(opt! evil-move-cursor-back nil)
(opt! evil-move-beyond-eol nil)
(opt! evil-search-wrap nil)

;; Disable starting any mode in motion state.

;; (opt! evil-normal-state-modes (append evil-emacs-state-modes
;; 									  evil-motion-state-modes
;; 									  evil-normal-state-modes))
;; (opt! evil-emacs-state-modes nil)
;; (opt! evil-motion-state-modes nil)

(opt! savehist-additional-variables (cl-adjoin 'evil-markers-alist savehist-additional-variables))
;;;; bindings
(declare-function minibuffer-keyboard-quit "delsel")
(declare-function evil-normal-state "evil")

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
(provide 'init-evil)
;;; init-evil.el ends here
