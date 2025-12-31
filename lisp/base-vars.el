;;; base-vars.el --- core variables -*- lexical-binding: t; -*-
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
(defconst o-lisp-dir (expand-file-name "lisp/" user-emacs-directory)
  "Directory where handcrafted configuration files go.")

(defconst o-local-dir (expand-file-name ".local/" user-emacs-directory)
  "Directory that stores subdirectories.")

(defconst o-etc-dir (expand-file-name "etc/" o-local-dir)
  "Directory where package configuration files go.")

(defvaralias 'o-data-dir 'o-etc-dir)

(defconst o-var-dir (expand-file-name "var/" o-local-dir)
  "Directory where persistent data files go.")

(defvaralias 'o-cache-dir 'o-var-dir)

(defvar o-custom-faces-alist nil
  "An alist that maps custom faces to built-in faces.

Each element is of the form (CUSTOM-FACE . BUILT-IN-FACE).  Whenever the theme
is changed CUSTOM-FACE updates its background and foreground similar to
BUILT-IN-FACE.  See `o-hook--set-faces-from-theme'.")

;; I need to process the `command-line-args' for font here so that I can set the
;; font before the frame is loaded.
(defvar o-init-theme nil
  "The initial theme to be applied at startup.
If nil, no theme is applied on startup.")

(defvar o-init-font nil
  "The initial font to be applied at startup.
If nil, no font is applied at startup.")

;; Since it is inexpensive I set this to non-nil by default.
(defvar o-init-profile-p t
  "Non-nil if Emacs configuration should be profiled at startup.")

;; I set this to non-nil so that I can get a functional Emacs instance and can
;; debug errors.  If I want to see the specific error I can restart Emacs with
;; this enabled.
(defvar o-init-noerrors-p t
  "Non-nil if errors in init files should be ignored at startup.")

(defvar o-init-data nil
  "Initialization data.
This includes the time that features took to load.")

(defvar o-local-var-alist nil
  "An alist of for setting local variables.
Each element is of the form (HOOK . VARALIST).
HOOK is a mode-hook.  And VARALIST is the alist of local variables and values
that need to be mapped when HOOK's mode is enabled. an alist mapping symbols to their
corresponding values, each element looks like (VAR . VAL).")
;;;; LEADERS
(defconst o-key-leader-normal "SPC"
  "The evil leader prefix key.")

(defconst o-key-localleader-normal "SPC m"
  "The localleader prefix key for major-mode specific commands.")

(defconst o-key-localleader-normal-alt ","
  "A shorter alternative `o-localleader-key'.")

(defconst o-key-leader-insert "M-SPC"
  "The leader prefix key used for Insert state.")

(defconst o-key-localleader-insert "M-SPC m"
  "The localleader prefix key for major-mode specific commands.")

(defconst o-key-localleader-insert-alt "M-,"
  "A short non-normal `o-localleader-key'.")

(defconst o-key-leader-emacs "C-c l"
  "The leader prefix key used for Emacs states.")

(defconst o-key-leader-emacs-alt "C-c SPC")

(defconst o-key-localleader-emacs "C-c l m"
  "The localleader prefix key for major-mode specific commands.")
;;; provide
(provide 'base-vars)
;;; base-vars.el ends here
