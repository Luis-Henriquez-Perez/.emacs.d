;;; init-ace-window.el --- ace-window configuration -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Free Software Foundation, Inc.
;;
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
;; This is my configuration for ace window.
;;
;;; Code:
;;;; ace-window
;;;;; swap
(opt! aw-swap-invert t)
;;;;; set the keys used by ace-window
;; The character z conflicts.
(opt! aw-keys (eval-when-compile (string-to-list "jfkdlsaurieowncpqmxb")))
;;;;; keybindings
(bind! oo-window-map "s" #'ace-swap-window)
(bind! oo-window-map "w" #'ace-window)
(bind! oo-window-map "j" #'ace-window)
(bind! oo-window-map "o" #'ace-window)
;;; provide
(provide 'init-ace-window)
;;; init-ace-window.el ends here
