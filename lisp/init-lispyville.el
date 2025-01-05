;;; init-lispyville.el --- initialize lispyville -*- lexical-binding: t; -*-
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
;; Initialize lispyville.
;;
;;; Code:
(require 'base)

;; Do not bind any keys by default.
(hook! emacs-lisp-mode-hook lispyville-mode)
(opt! lispyville-key-theme nil)

(bind! i lispyville-mode-map "SPC" #'lispy-space)
(bind! i lispyville-mode-map ";" #'lispy-comment)

(bind! evil-outer-text-objects-map "c" #'lispyville-outer-comment)
(bind! evil-inner-text-objects-map "c" #'lispyville-inner-comment)

(bind! (n v) "g c" #'lispyville-comment-or-uncomment)
(bind! (n v) "g l" #'lispyville-comment-and-clone-dwim)
;;; provide
(provide 'init-lispyville)
;;; init-lispyville.el ends here
