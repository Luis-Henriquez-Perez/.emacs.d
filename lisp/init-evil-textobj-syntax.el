;;; init-evil-textobj-syntax.el --- Initialize evil-textobj-syntax -*- lexical-binding: t; -*-
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
;; Initialize evil-textobj-syntax.
;;
;;; Code:
(autoload #'evil-i-syntax "evil-textobj-syntax" nil t 'function)
(autoload #'evil-a-syntax "evil-textobj-syntax" nil t 'function)
(bind! evil-outer-text-objects-map "h" #'evil-a-syntax)
(bind! evil-inner-text-objects-map "h" #'evil-i-syntax)
;;; provide
(provide 'init-evil-textobj-syntax)
;;; init-evil-textobj-syntax.el ends here
