;;; init-pkg-which-key.el --- initialize which-key -*- lexical-binding: t; -*-
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
(require 'init-core)

(autoload 'which-key-mode "which-key" nil nil 'function)
(add-hook 'after-init-hook #'which-key-mode)

(o-opt which-key-sort-uppercase-first nil)
(o-opt which-key-max-display-columns nil)
(o-opt which-key-add-column-padding 1)
(o-opt which-key-min-display-lines 1)
(o-opt which-key-side-window-slot -10)
(o-opt which-key-sort-order #'which-key-prefix-then-key-order)
(o-opt which-key-popup-type 'side-window)
(o-opt which-key-idle-delay 1.2)
;; (o-opt line-spacing 3 :hook which-key-init-buffer-hook :local t)
(o-opt which-key-show-transient-maps t)
(o-opt which-key-allow-evil-operators t)
(o-opt which-key-show-operator-state-maps t)
(o-opt which-key-show-prefix 'top)
;;; provide
(provide 'init-pkg-which-key)
;;; init-pkg-which-key.el ends here
