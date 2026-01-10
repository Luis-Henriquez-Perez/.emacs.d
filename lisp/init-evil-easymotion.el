;;; init-evil-easymotion.el --- initialize evil-easymotion -*- lexical-binding: t; -*-
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
;; Initialize evil-easymotion.
;;
;;; Code:
(require 'init-core)

(defvar evil-set-command-properties)

(declare-function o-evilem-motion-beginning-of-word "lib-evil-easymotion")
(declare-function o-evilem-motion-beginning-of-WORD "lib-evil-easymotion")
(declare-function o-evilem-motion-end-of-word       "lib-evil-easymotion")
(declare-function o-evilem-motion-end-of-WORD       "lib-evil-easymotion")
(declare-function o-evilem-motion-char              "lib-evil-easymotion")
(declare-function o-evilem-motion-beginning-of-line "lib-evil-easymotion")

(autoload #'o-evilem-motion-beginning-of-word "lib-evil-easymotion" nil t 'function)
(autoload #'o-evilem-motion-beginning-of-WORD "lib-evil-easymotion" nil t 'function)
(autoload #'o-evilem-motion-end-of-word       "lib-evil-easymotion" nil t 'function)
(autoload #'o-evilem-motion-end-of-WORD       "lib-evil-easymotion" nil t 'function)
(autoload #'o-evilem-motion-char              "lib-evil-easymotion" nil t 'function)
(autoload #'o-evilem-motion-beginning-of-line "lib-evil-easymotion" nil t 'function)

(o-require-after-load 'evil-easymotion 'config-evil-easymotion)
;;; provide
(provide 'init-evil-easymotion)
;;; init-evil-easymotion.el ends here
