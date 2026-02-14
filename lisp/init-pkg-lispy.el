;;; init-pkg-lispy.el --- Initialize lispy -*- lexical-binding: t; -*-
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
;; Initialize lispy.
;;
;;; Code:
(o-declare-package 'lispy)

;; do not delete unmached delimiters with.
;; right now the only problem is.
(o-opt lispy-safe-delete t)
(o-opt lispy-safe-copy t)
(o-opt lispy-safe-paste t)

(autoload 'lispy-delete "lispy" nil 'interactive 'function)
(autoload 'lispy-comment "lispy" nil 'interactive 'function)
(autoload 'lispy-space "lispy" nil 'interactive 'function)
;;; provide
(provide 'init-pkg-lispy)
;;; init-pkg-lispy.el ends here
