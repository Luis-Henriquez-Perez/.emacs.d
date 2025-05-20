;;; 990-config-modus-themes.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
(require 'modus-themes)

(set-face! modus oo-mode-line-segment-1
           :background (modus-themes-with-colors rainbow-1)
           :foreground (modus-themes-with-colors bg-main))

(set-face! modus oo-mode-line-segment-2
           :background (modus-themes-with-colors rainbow-2)
           :foreground (modus-themes-with-colors bg-main))

(set-face! modus oo-mode-line-segment-3
           :background (modus-themes-with-colors rainbow-3)
           :foreground (modus-themes-with-colors bg-main))

(set-face! modus oo-mode-line-segment-4
           :background (modus-themes-with-colors rainbow-4)
           :foreground (modus-themes-with-colors bg-main))

(set-face! modus +evil-normal-state-face
           :background (modus-themes-with-colors magenta-cooler)
           :foreground (modus-themes-with-colors bg-main))

(set-face! modus +evil-insert-state-face
           :background (modus-themes-with-colors green)
           :foreground (modus-themes-with-colors bg-main))

(set-face! modus +evil-visual-state-face
           :background (modus-themes-with-colors magenta)
           :foreground (modus-themes-with-colors bg-main))

(set-face! modus +evil-replace-state-face
           :background (modus-themes-with-colors red)
           :foreground (modus-themes-with-colors bg-main))

(set-face! modus +evil-motion-state-face
           :background (modus-themes-with-colors yellow)
           :foreground (modus-themes-with-colors bg-main))

(set-face! modus +evil-operator-state-face
           :background (modus-themes-with-colors yellow)
           :foreground (modus-themes-with-colors bg-main))

(set-face! modus +evil-emacs-state-face
           :background (modus-themes-with-colors blue)
           :foreground (modus-themes-with-colors bg-main))
;;; provide
(provide '990-config-modus-themes)
;;; 990-config-modus-themes.el ends here
