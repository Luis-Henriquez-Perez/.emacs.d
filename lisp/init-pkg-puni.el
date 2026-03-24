;;; init-pkg-puni.el --- Initialize puni -*- lexical-binding: t; -*-
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
;; Puni is a library for commands that operate on sexps.  It is lightweight and
;; has no external dependencies.  The main reason I use it is because it provies
;;
;;; Code:
(o-package-declare 'puni)

(autoload 'puni-soft-delete "puni" nil nil 'function)
(autoload 'puni-bounds-of-sexp-around-point "puni" nil nil 'function)
(autoload 'puni-bounds-of-list-around-point "puni" nil nil 'function)
(autoload 'puni--wrap-region "puni" nil nil 'function)
(autoload 'puni-delete-region "puni" nil nil 'function)
(autoload 'puni-soft-delete "puni" nil nil 'function)
;;; provide
(provide 'init-pkg-puni)
;;; init-pkg-puni.el ends here
