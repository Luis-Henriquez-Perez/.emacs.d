;;; 130-init-evil.el --- initialize evil -*- lexical-binding: t; -*-
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
(require! "^0[01]")
;;;; settings
;; Must be set before evil is loaded.  This, therefore, cannot be deferred with
;; `opt!'.  If this is not set evil with add opinionated bindings to certain
;; programs like dired which will override my own.
(defvar evil-want-keybinding)
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
(opt! evil-normal-state-modes (append evil-emacs-state-modes
									  evil-motion-state-modes
									  evil-normal-state-modes))
(opt! evil-emacs-state-modes nil)
(opt! evil-motion-state-modes nil)
;;;; main
(defhook! load-evil (after-init-hook :depth 10)
  (require 'evil nil t))

(add-hook 'emacs-startup-hook #'evil-mode)
;; (opt! savehist-additional-variables (cl-adjoin 'evil-markers-alist savehist-additional-variables))
;;;; bindings
(declare-function minibuffer-keyboard-quit "delsel")
(declare-function evil-normal-state "evil")
;;;; faces
(defface +evil-state-face
  '((t (:weight bold)))
  "Meta-face used for property inheritance on all evil state faces.")

(defface +evil-emacs-state-face
  '((t (:inherit +evil-state-face :background "#483d8b")))
  "Face for the Emacs state tag in evil indicator.")

(setf (alist-get '+evil-emacs-state-face oo-custom-faces-alist) 'font-lock-builtin-face)

(defface +evil-insert-state-face
  '((t (:inherit +evil-state-face :background "#228b22")))
  "Face for the insert state tag in evil indicator.")

(setf (alist-get '+evil-insert-state-face oo-custom-faces-alist) 'font-lock-type-face)

(defface +evil-motion-state-face
  '((t (:inherit +evil-state-face :background "#a0522d")))
  "Face for the motion state tag in evil indicator.")

(setf (alist-get '+evil-motion-state-face oo-custom-faces-alist) 'font-lock-variable-name-face)

(defface +evil-normal-state-face
  '((t (:inherit +evil-state-face :background "purple")))
  "Face for the normal state tag in evil indicator.")

(setf (alist-get '+evil-normal-state-face oo-custom-faces-alist) 'font-lock-keyword-face)

(defface +evil-operator-state-face
  '((t (:inherit +evil-state-face :background "#0000ff")))
  "Face for the operator state tag in evil indicator.")

(setf (alist-get '+evil-operator-state-face oo-custom-faces-alist) 'font-lock-function-name-face)

(defface +evil-visual-state-face
  '((t (:inherit +evil-state-face :background "#8b2252")))
  "Face for the visual state tag in evil indicator.")

(setf (alist-get '+evil-visual-state-face oo-custom-faces-alist) 'font-lock-string-face)

(defface +evil-replace-state-face
  '((t (:inherit +evil-state-face :background "#008b8b")))
  "Face for the replace state tag in evil indicator.")

(setf (alist-get '+evil-replace-state-face oo-custom-faces-alist) 'font-lock-constant-face)
;;;; change cursor color and shape according to current evil state
;; Did not realize for the longest time that evil cursor can be a function that
;; changes the cursor.  With this in mind, the best way to set the cursor size
;; and shape dynamically is to set the corresponding cursor symbols to functions.
(declare-function evil-set-cursor "evil")
(defvar evil-state)
(defun +evil-state-face ()
  "Return the cursor color for state as a string."
  ;; (when (null evil-state)
  ;;   (message "evil-state is nil"))
  (intern (format "+evil-%s-state-face" evil-state)))

(defun +evil-state-background ()
  (aand! (+evil-state-face) (face-attribute it :background)))

(defun +evil-default-cursor ()
  "Set cursor for normal state."
  (evil-set-cursor (list t (+evil-state-background))))

(defun +evil-insert-state-cursor ()
  "Set cursor for insert state."
  (evil-set-cursor (list '(bar . 2) (+evil-state-background))))

(defun +evil-operator-state-cursor ()
  "Set cursor for operator state."
  (evil-set-cursor (list '(hbar . 9) (+evil-state-background))))

(defalias '+evil-normal-state-cursor '+evil-default-cursor)
(defalias '+evil-motion-state-cursor '+evil-default-cursor)
(defalias '+evil-replace-state-cursor '+evil-default-cursor)
(defalias '+evil-emacs-state-cursor '+evil-default-cursor)
(defalias '+evil-visual-state-cursor '+evil-default-cursor)
;;;; cursor colors
(opt! evil-default-cursor   #'+evil-default-cursor)
(opt! evil-normal-state-cursor   #'+evil-normal-state-cursor)
(opt! evil-insert-state-cursor   #'+evil-insert-state-cursor)
(opt! evil-visual-state-cursor   #'+evil-visual-state-cursor)
(opt! evil-motion-state-cursor   #'+evil-motion-state-cursor)
(opt! evil-replace-state-cursor  #'+evil-replace-state-cursor)
(opt! evil-operator-state-cursor #'+evil-operator-state-cursor)
(opt! evil-emacs-state-cursor    #'+evil-emacs-state-cursor)
;;; provide
(provide '130-init-evil)
;;; 130-init-evil.el ends here
