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

(declare-function #'o-tempel-elisp-expand-defun "init-after-tempel")
(declare-function #'o-tempel-elisp-expand-defhook "init-after-tempel")
(declare-function #'o-tempel-elisp-expand-defvar "init-after-tempel")
(declare-function #'o-tempel-elisp-expand-message "init-after-tempel")
(declare-function #'o-tempel-elisp-expand-message-var "init-after-tempel")

;; (defun o-elisp-abbrev (abbrev def hook)
;;   "Shorthand for defining an elisp abbrev."
;;   (define-abbrev emacs-lisp-mode-abbrev-table abbrev def hook :enable-function ))

(define-abbrev emacs-lisp-mode-abbrev-table "aih" "after-init-hook")
(define-abbrev emacs-lisp-mode-abbrev-table "esh" "emacs-startup-hook")
(define-abbrev emacs-lisp-mode-abbrev-table "dfn" "" #'o-tempel-elisp-expand-defun :enable-function #'o-abbrev-in-elisp-code-p)
(define-abbrev emacs-lisp-mode-abbrev-table "dhk" "" #'o-tempel-elisp-expand-defhook :enable-function #'o-abbrev-in-elisp-code-p)
(define-abbrev emacs-lisp-mode-abbrev-table "dvar" "" #'o-tempel-elisp-expand-defvar :enable-function #'o-abbrev-in-elisp-code-p)
(define-abbrev emacs-lisp-mode-abbrev-table "funn" "" #'o-tempel-elisp-expand-defun :enable-function #'o-abbrev-in-elisp-code-p)
(define-abbrev emacs-lisp-mode-abbrev-table "msg" "" #'o-tempel-elisp-expand-message :enable-function #'o-abbrev-in-elisp-code-p)
(define-abbrev emacs-lisp-mode-abbrev-table "msgv" "" #'o-tempel-elisp-expand-message-var :enable-function #'o-abbrev-in-elisp-code-p)
;;; provide
(provide 'emacs-lisp-mode-abbrev-table)
;;; emacs-lisp-mode-abbrev-table.el ends here
