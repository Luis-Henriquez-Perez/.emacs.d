;; emacs-lisp-mode-abbrev-table.el --- Define emacs-lisp-mode-abbrev-table -*- lexical-binding: t; -*-
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
;; Define emacs-lisp-mode=abbrev-table.
;;
;;; Code:
(require 'abbrev)

(declare-function #'o-tempel-expand-elisp-defun       "init-after-tempel")
(declare-function #'o-tempel-expand-elisp-defhook     "init-after-tempel")
(declare-function #'o-tempel-expand-elisp-defvar      "init-after-tempel")
(declare-function #'o-tempel-expand-elisp-message     "init-after-tempel")
(declare-function #'o-tempel-expand-elisp-message-var "init-after-tempel")

(define-abbrev emacs-lisp-mode-abbrev-table "aih" "after-init-hook")
(define-abbrev emacs-lisp-mode-abbrev-table "esh" "emacs-startup-hook")
(define-abbrev emacs-lisp-mode-abbrev-table "dfn"  "" #'o-tempel-expand-elisp-defun :enable-function #'o-abbrev-in-elisp-code-p)
(define-abbrev emacs-lisp-mode-abbrev-table "dhk"  "" #'o-tempel-expand-elisp-defhook :enable-function #'o-abbrev-in-elisp-code-p)
(define-abbrev emacs-lisp-mode-abbrev-table "dvar" "" #'o-tempel-expand-elisp-defvar :enable-function #'o-abbrev-in-elisp-code-p)
(define-abbrev emacs-lisp-mode-abbrev-table "funn" "" #'o-tempel-expand-elisp-defun :enable-function #'o-abbrev-in-elisp-code-p)
(define-abbrev emacs-lisp-mode-abbrev-table "msg"  "" #'o-tempel-expand-elisp-message :enable-function #'o-abbrev-in-elisp-code-p)
(define-abbrev emacs-lisp-mode-abbrev-table "msgv" "" #'o-tempel-expand-elisp-message-var :enable-function #'o-abbrev-in-elisp-code-p)
;;; provide
(provide 'emacs-lisp-mode-abbrev-table)
;;; emacs-lisp-mode-abbrev-table.el ends here
