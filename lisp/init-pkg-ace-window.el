;;; init-pkg-ace-window.el --- ace-window configuration -*- lexical-binding: t; -*-
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
(require 'init-core)

(o-package-declare 'ace-window)

(o-opt aw-swap-invert t)
;; The character z conflicts.
(o-opt aw-keys (eval-when-compile (string-to-list "jfkdlsaurieowncpqmxb")))
;;; provide
(provide 'init-pkg-ace-window)
;;; init-pkg-ace-window.el ends here
