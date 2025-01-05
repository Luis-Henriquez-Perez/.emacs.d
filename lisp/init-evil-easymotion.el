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
(require 'base)

(opt! evilem-style 'at)
(opt! evilem-keys (eval-when-compile (string-to-list "jfkdlsaurieowncpqmxzb")))

(autoload #'+evilem-motion-beginning-of-word "config-evil-easymotion" nil t 'function)
(autoload #'+evilem-motion-beginning-of-WORD "config-evil-easymotion" nil t 'function)
(autoload #'+evilem-motion-end-of-word       "config-evil-easymotion" nil t 'function)
(autoload #'+evilem-motion-end-of-WORD       "config-evil-easymotion" nil t 'function)
(autoload #'+evilem-motion-char              "config-evil-easymotion" nil t 'function)
(autoload #'+evilem-motion-beginning-of-line "config-evil-easymotion" nil t 'function)

;; (bind! (n v) "w" #'+evilem-motion-beginning-of-word)
;; (bind! (n v) "W" #'+evilem-motion-beginning-of-WORD)
;; (bind! (n v) "e" #'+evilem-motion-end-of-word)
;; (bind! (n v) "E" #'+evilem-motion-end-of-WORD)
;; (bind! (n v o) "f" #'+evilem-motion-char)
;; (bind! (n v o) "H" #'+evilem-motion-beginning-of-line)
;;; provide
(provide 'init-evil-easymotion)
;;; init-evil-easymotion.el ends here
