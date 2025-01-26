;;; config-smartparens.el --- smartparens configuration -*- lexical-binding: t; -*-
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
;; This is my configuration for smartparens.
;;
;;; Code:
(require 'smartparens)

(opt! sp-highlight-wrap-tag-overlay nil)

(opt! sp-highlight-pair-overlay nil)

(opt! sp-highlight-wrap-overlay nil)

(opt! sp-show-pair-delay 0.2)

(sp-local-pair sp-lisp-modes "'" nil :actions nil)

(sp-local-pair sp-lisp-modes "`" "'" :when '(sp-in-string-p sp-in-comment-p))

(sp-local-pair 'minibuffer-mode "'" nil :actions nil)
(sp-local-pair 'minibuffer-mode "`" nil :actions nil)

(require 'smartparens-config)
;;; provide
(provide 'config-smartparens)
;;; config-smartparens.el ends here
