;;; +abbrev-python-mode-abbrevs.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; TODO: add commentary
;;
;;; Code:
(put '+abbrev-insert-ifmain 'no-self-insert t)

(defun +abbrev-insert-ifmain ()
  (require 'tempel)
  (tempel-insert 'ifmain)
  (when (bound-and-true-p evil-mode)
    (evil-normalize-keymaps))
  t)

(defun +abbrev-enable-python-abbrev-p ()
  (derived-mode-p 'python-mode))

(define-abbrev global-abbrev-table "mmain" "" '+abbrev-insert-ifmain :enable-function '+abbrev-enable-python-abbrev-p)

(define-abbrev global-abbrev-table "isoup" "from bs4 import BeautifulSoup" nil :enable-function '+abbrev-enable-python-abbrev-p)
