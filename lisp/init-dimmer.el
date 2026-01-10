;;; init-dimmer.el --- Initialize dimmer -*- lexical-binding: t; -*-
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
;; Initialize dimmer.
;;
;; Dimmer is a package that dimms windows that are not the active window to help
;; you know at a glance which window you are on.  You might think this is
;; unnecessary and I did too until I started getting confused about which window
;; I as in--even when I only had two open.
;;
;;; Code:
(require 'init-core)

;; TODO exclude if it is a which-key buffer.
;; TODO also exclude if it is the only buffer except for the minibuffer.
(o-opt dimmer-fraction 0.5)

;; TODO make this happen only after I open another window.  So I will need
;; something like an open window hook.
;; (add-hook 'o-first-input-hook #'dimmer-mode)
;;; provide
(provide 'init-dimmer)
;;; init-dimmer.el ends here
