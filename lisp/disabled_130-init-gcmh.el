;;; 130-init-gcmh.el --- initialize gcmh -*- lexical-binding: t; -*-
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
;; Initialize gcmh.
;;
;;; Code:
(require! "^0[01]")

(add-hook 'emacs-startup-hook #'gcmh-mode 91)

(opt! gcmh-idle-delay 'auto)
(opt! gcmh-high-cons-threshold (* 8 1024 1024))
(opt! gcmh-low-cons-threshold (* 4 1024 1024))
;;; provide
(provide '130-init-gcmh)
;;; 130-init-gcmh.el ends here
