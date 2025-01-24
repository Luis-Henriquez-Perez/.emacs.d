;;; init-which-key.el --- initialize which-key -*- lexical-binding: t; -*-
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
;; Initialize which-key.
;;
;;; Code:
(require 'base)

(hook! emacs-startup-hook which-key-mode)

(opt! which-key-sort-uppercase-first nil)
(opt! which-key-max-display-columns nil)
(opt! which-key-add-column-padding 1)
(opt! which-key-min-display-lines 1)
(opt! which-key-side-window-slot -10)
(opt! which-key-sort-order #'which-key-prefix-then-key-order)
(opt! which-key-popup-type 'side-window)
(opt! which-key-idle-delay 0.8)
;; (opt! line-spacing 3 :hook which-key-init-buffer-hook :local t)
(opt! which-key-show-transient-maps t)
(opt! which-key-allow-evil-operators t)
(opt! which-key-show-operator-state-maps t)
(opt! which-key-show-prefix 'top)
;;; provide
(provide 'init-which-key)
;;; init-which-key.el ends here
