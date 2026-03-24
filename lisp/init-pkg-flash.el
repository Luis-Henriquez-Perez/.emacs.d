;;; init-pkg-flash.el --- Initialize flash -*- lexical-binding: t; -*-
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
;; Initialize flash.
;;
;; flash is similar to avy except you can keep typing in characters to narrow
;; the search.  Also it looks much nicer with the rainbow display.
;;
;;; Code:
(o-package-declare 'flash)

(autoload 'flash-jump "flash" nil 'interactive 'function)
(autoload 'flash-jump-continue "flash" nil 'interactive 'function)

(o-opt flash-labels "asdfjkl;ghqwertyuiopzxcvbnm")
(o-opt flash-label-uppercase nil)     ; double available labels (a-z + A-Z)
(o-opt flash-multi-window t)
(o-opt flash-autojump nil)            ; auto-jump when single match
(o-opt flash-backdrop nil)          ; no dimming
(o-opt flash-rainbow t)             ; colorful labels
(o-opt flash-rainbow-shade 2)       ; 1-9: pastel to dark
(o-opt flash-highlight-matches t)
(o-opt flash-label-position 'overlay)
(o-opt flash-char-jump-labels t)    ; labels on f/t/F/T matches
(o-opt flash-nohlsearch t)          ; clear highlight after jump
(o-opt flash-search-history t)
;;; provide
(provide 'init-pkg-flash)
;;; init-pkg-flash.el ends here
