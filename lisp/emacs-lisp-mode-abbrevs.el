;;; emacs-lisp-mode-abbrevs.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
(require 'abbrev)

(autoload! oo-expand-elisp-defun "990-snippets")
(autoload! oo-expand-elisp-defhook "990-snippets")

(define-abbrev-table 'emacs-lisp-mode-abbrev-table
  '(("msgv" "" 'oo-expand-elisp-message-var :enable-function oo-in-elisp-code-p)
    ("msg" "" 'oo-expand-elisp-message :enable-function oo-in-elisp-code-p)
    ("dhk" "" oo-expand-elisp-defhook :enable-function oo-in-elisp-code-p)
    ("elasped" "elapsed" nil)
    ("aih" "after-init-hook" nil)
    ("esh" "emacs-startup-hook" nil)
    ("dvar" "" oo-expand-elisp-defvar :enable-function oo-in-elisp-code-p)
    ("funn" "" oo-expand-elisp-defun :enable-function oo-in-elisp-code-p)
    ("dfn" "" oo-expand-elisp-defun :enable-function oo-in-elisp-code-p)))
;;; provide
(provide 'emacs-lisp-mode-abbrevs)
;;; emacs-lisp-mode-abbrevs.el ends here
