;;; init-restart-emacs.el --- Initialize restart-emacs -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Initialize `restart-emacs'.
;;
;;; Code:
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
;; Initialize `restart-emacs'.
;;
;;; Code:
(require 'base)
;;;; bindings
(bind! oo-app-map "E" #'restart-emacs-start-new-emacs)
(bind! oo-quit-map "R" #'restart-emacs)
(bind! oo-quit-map "E" #'restart-emacs-start-new-emacs)
(bind! oo-quit-map "r" #'restart-emacs)
(bind! oo-quit-map "q" #'save-buffers-kill-emacs)
;;; provide
(provide 'init-restart-emacs)
;;; init-restart-emacs.el ends here
