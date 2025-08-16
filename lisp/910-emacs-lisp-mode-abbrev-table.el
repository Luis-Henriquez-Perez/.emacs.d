;;; 910-emacs-lisp-mode-abbrev-table.el --- TODO: add commentary -*- lexical-binding: t; -*-
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

(autoload 'oo-expand-elisp-defun "990-snippets" nil nil 'function)
(autoload 'oo-expand-elisp-defhook "990-snippets" nil nil 'function)
(autoload 'oo-expand-elisp-defvar "990-snippets" nil nil 'function)
(autoload 'oo-expand-elisp-message "990-snippets" nil nil 'function)
(autoload 'oo-expand-elisp-message-var "990-snippets" nil nil 'function)

(define-abbrev-table 'emacs-lisp-mode-abbrev-table
  '(("aih" "after-init-hook")
    ("dfn" "" oo-expand-elisp-defun :enable-function oo-in-elisp-code-p)
    ("dhk" "" oo-expand-elisp-defhook :enable-function oo-in-elisp-code-p)
    ("dvar" "" oo-expand-elisp-defvar :enable-function oo-in-elisp-code-p)
    ("elasped" "elapsed")
    ("esh" "emacs-startup-hook")
    ("funn" "" oo-expand-elisp-defun :enable-function oo-in-elisp-code-p)
    ("msg" "" oo-expand-elisp-message :enable-function oo-in-elisp-code-p)
    ("msgv" "" oo-expand-elisp-message-var :enable-function oo-in-elisp-code-p)))
;;; provide
(provide '910-emacs-lisp-mode-abbrev-table)
;;; 910-emacs-lisp-mode-abbrev-table.el ends here
;;; 910-emacs-lisp-mode-abbrev-table.el --- TODO: add commentary -*- lexical-binding: t; -*-
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

(autoload 'oo-expand-elisp-defun "990-snippets" nil nil 'function)
(autoload 'oo-expand-elisp-defhook "990-snippets" nil nil 'function)
(autoload 'oo-expand-elisp-defvar "990-snippets" nil nil 'function)
(autoload 'oo-expand-elisp-message "990-snippets" nil nil 'function)
(autoload 'oo-expand-elisp-message-var "990-snippets" nil nil 'function)

(define-abbrev-table 'emacs-lisp-mode-abbrev-table
  '(("aih" "after-init-hook")
    ("dfn" "" oo-expand-elisp-defun :enable-function oo-in-elisp-code-p)
    ("dhk" "" oo-expand-elisp-defhook :enable-function oo-in-elisp-code-p)
    ("dvar" "" oo-expand-elisp-defvar :enable-function oo-in-elisp-code-p)
    ("elasped" "elapsed")
    ("esh" "emacs-startup-hook")
    ("funn" "" oo-expand-elisp-defun :enable-function oo-in-elisp-code-p)
    ("msg" "" oo-expand-elisp-message :enable-function oo-in-elisp-code-p)
    ("msgv" "" oo-expand-elisp-message-var :enable-function oo-in-elisp-code-p)))
;;; provide
(provide '910-emacs-lisp-mode-abbrev-table)
;;; 910-emacs-lisp-mode-abbrev-table.el ends here
