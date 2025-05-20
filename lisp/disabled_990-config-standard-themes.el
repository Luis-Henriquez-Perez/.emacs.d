;;; 990-config-standard-themes.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
(require '050-base)
(require 'standard-themes)

(set-face! standard oo-mode-line-segment-1
           :background (standard-themes-with-colors blue-warmer)
           :foreground (standard-themes-with-colors bg-main))

(set-face! standard oo-mode-line-segment-2
           :background (standard-themes-with-colors red-cooler)
           :foreground (standard-themes-with-colors bg-main))

(set-face! standard oo-mode-line-segment-3
           :background (standard-themes-with-colors yellow-cooler)
           :foreground (standard-themes-with-colors bg-main))

(set-face! standard oo-mode-line-segment-4
           :background (standard-themes-with-colors cyan-cooler)
           :foreground (standard-themes-with-colors bg-main))

(set-face! standard +evil-normal-state-face
           :background (standard-themes-with-colors magenta-cooler)
           :foreground (standard-themes-with-colors bg-main))

(set-face! standard +evil-insert-state-face
           :background (standard-themes-with-colors green)
           :foreground (standard-themes-with-colors bg-main))

(set-face! standard +evil-visual-state-face
           :background (standard-themes-with-colors magenta)
           :foreground (standard-themes-with-colors bg-main))

(set-face! standard +evil-replace-state-face
           :background (standard-themes-with-colors red)
           :foreground (standard-themes-with-colors bg-main))

(set-face! standard +evil-motion-state-face
           :background (standard-themes-with-colors yellow)
           :foreground (standard-themes-with-colors bg-main))

(set-face! standard +evil-operator-state-face
           :background (standard-themes-with-colors yellow)
           :foreground (standard-themes-with-colors bg-main))

(set-face! standard +evil-emacs-state-face
           :background (standard-themes-with-colors blue)
           :foreground (standard-themes-with-colors bg-main))
;;; provide
(provide '990-config-standard-themes)
;;; 990-config-standard-themes.el ends here
