;;; init-vertico.el --- initialize vertico -*- lexical-binding: t; -*-
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
;; Initialize vertico.
;;
;;; Code:
(require 'base)

(hook! on-first-input-hook vertico-mode)

(opt! vertico-count-format '("%-6s " . "%2$s"))
(opt! vertico-count 15)

(bind! i vertico-map "TAB" #'vertico-next)
(bind! i vertico-map "C-k" #'vertico-previous)
(bind! i vertico-map "C-j" #'vertico-next)
(bind! i vertico-map ";" #'vertico-quick-exit)
(bind! i vertico-map "C-;" #'vertico-quick-exit)
(bind! i vertico-map [backtab] #'vertico-previous)
(bind! i vertico-map "C-o" #'embark-act)
;;; provide
(provide 'init-vertico)
;;; init-vertico.el ends here
