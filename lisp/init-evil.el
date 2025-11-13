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
(require! "^0[01]")
;;;; settings
;; Must be set before evil is loaded.  This, therefore, cannot be deferred with
;; `opt!'.  If this is not set evil with add opinionated bindings to certain
;; programs like dired which will override my own.
(defvar evil-want-keybinding)
(setq evil-want-keybinding nil)

(defun oo-load-evil-h ()
  "Require `evil'."
  (require 'evil nil t))

(add-hook 'after-init-hook #'oo-load-evil-h -90)

(add-hook 'emacs-startup-hook #'evil-mode)

;; To ensure that =oo-override-mode-map= takes priority over evil states, we need
;; to make it an intercept map for all evil states.  In evil, intercept maps are
;; maps that take priority (intercept) evil bindings when they have a different
;; binding for the same key (this is opposed to =overriding-maps=, which completely
;; override an evil keymap).
(defvar override-global-map)
(declare-function evil-make-intercept-map "evil")
(defun oo-make-intercept-map-h ()
  "Register `oo-override-map' as an intercept map."
  (require 'bind-key)
  (evil-make-intercept-map override-global-map 'all t))

(add-hook 'evil-mode-hook #'oo-make-intercept-map-h)

(oo-require-after-load 'evil '990-config-evil)
;;; provide
(provide 'init-evil)
;;; init-evil.el ends here
