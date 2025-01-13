;;; oo-emacs-lisp-mode-abbrevs.el --- abbrevs for emacs-lisp-mode -*- lexical-binding: t; -*-
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
;; These are abbrevs for emacs-lisp-mode.
;;
;;; Code:
;;;; requirements
(require 'abbrev)
;;;; emacs-lisp
(defun oo--use-emacs-lisp-mode-abbrevs-p ()
  "Return non-nil when emacs-lisp-mode abbrevs should expand.
This is when `emacs-lisp-mode' is enabled and point is not in a string or
comment."
  (and (derived-mode-p 'emacs-lisp-mode)
       (not (oo-in-string-or-comment-p))))
;;; provide
(provide 'oo-emacs-lisp-mode-abbrevs)
;;; oo-emacs-lisp-mode-abbrevs.el ends here
