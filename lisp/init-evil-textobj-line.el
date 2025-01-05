;;; init-evil-textobj-line.el --- initialize evil-textobj-line -*- lexical-binding: t; -*-
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
;; Initialize evil-textobj-line.
;;
;;; Code:
(require 'base)

(autoload #'evil-inner-line                    "evil-textobj-line"                 nil t 'function)
(autoload #'evil-a-line                        "evil-textobj-line"                 nil t 'function)
(bind! evil-inner-text-objects-map "l" #'evil-inner-line)
(bind! evil-outer-text-objects-map "l" #'evil-a-line)
;;; provide
(provide 'init-evil-textobj-line)
;;; init-evil-textobj-line.el ends here
