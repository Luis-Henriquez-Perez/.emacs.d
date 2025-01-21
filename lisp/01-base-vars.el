;;; 01-base-vars.el --- core variables -*- lexical-binding: t; -*-
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
;;; Code:
(defconst oo-lisp-dir (expand-file-name "lisp/" user-emacs-directory)
  "Directory where handcrafted configuration files go.")

(defconst oo-local-dir (expand-file-name ".local/" user-emacs-directory)
  "Directory where auto-generated files go.")

(defconst oo-etc-dir (expand-file-name "etc/" oo-local-dir)
  "Directory where auto-generated configuration files go.")

(defconst oo-var-dir (expand-file-name "var/" oo-local-dir)
  "Directory where persistent data files go.")

(defvar oo-errors nil
  "An alist of errors that occur in advices or hooks during Emacs startup.
Each element looks like (HOOK-OR-ADVICE . ERROR).  HOOK-OR-ADVICE is a hook or
an advice symbol that raised an error.  ERROR is the error object generated by
HOOK-OR-ADVICE.")

(defvar oo-debug-p (or (getenv "DEBUG") init-file-debug)
  "When non-nil print debug messages.
The --debug-init flag and setting the DEBUG envar will enable this at startup.")
;;; provide
(provide '01-base-vars)
;;; 01-base-vars.el ends here
