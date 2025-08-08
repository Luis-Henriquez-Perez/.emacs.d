;;; 051-base-vars.el --- core variables -*- lexical-binding: t; -*-
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
  "Directory that stores subdirectories.")

;; (defvaralias 'oo-config-dir 'oo-etc-dir)

(defconst oo-config-dir (expand-file-name "etc/" oo-local-dir)
  "Directory where package configuration files go.")

;; (defvaralias 'oo-cache-dir 'oo-var-dir)

(defconst oo-cache-dir (expand-file-name "var/" oo-local-dir)
  "Directory where persistent data files go.")

(defvar oo-debug-p (or (getenv "DEBUG") init-file-debug)
  "When non-nil print debug messages.
The --debug-init flag and setting the DEBUG envar will enable this at startup.")

(defvar oo-init-data nil
  "A record of data concerning loaded files.

This is an alist where each element is of the form (feature start-time end-time
error).  Feature is an init feature, start and end time, and error is any
error.  If an error occurs start and end are nil.  Conversely, if start and end
are non-nil, then error is nil.")

(defvar oo-custom-faces-alist nil
  "An alist that update the background of faces based on existing faces.

Each element is of the form (custom-face . built-in-face).  Whenever the theme
is changed CUSTOM-FACE updates its background and foreground similar to
built-in-face.  See `oo--enable-theme-functions--set-state-faces-from-theme-h'.")

;; I need to process the `command-line-args' for font here so that I can set the
;; font before the frame is loaded.
(defvar oo-initial-theme (aremf! command-line-args
                           (and (string-match "^--theme=\\(.+\\)" it)
                                (intern (match-string 1 it))))
  "Initial theme.")

(defvar oo-initial-font (aremf! command-line-args
                          (and (string-match "^--font=\\(.+\\)" it)
                               (match-string 1 it)))
  "Initial font.")

(when oo-initial-font
  (push `(font . ,oo-initial-font) default-frame-alist))
;;; provide
(provide '051-base-vars)
;;; 051-base-vars.el ends here
