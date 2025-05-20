;;; 130-init-activities.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; There are many, many workspace packages available for Emacs such as
;; `perpective', `persp-mode', `eyebrowse', `desktop.el', and the list goes on.
;; I choose to use this one because it uses bookmarks to generate the buffers.
;; And because there is extensive bookmark support in Emacs.
;;
;;; Code:
(opt! activities-kill-buffers t)
(opt! activities-bookmark-store t)

;; Maybe I should not or do not need to load this at startup.
(hook! emacs-startup-hook activities-mode)
(hook! emacs-startup-hook activities-tabs-mode)
;;; provide
(provide '130-init-activities)
;;; 130-init-activities.el ends here
