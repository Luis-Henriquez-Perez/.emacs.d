;;; 001-base-vars.el --- core variables -*- lexical-binding: t; -*-
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
(eval-when-compile (require '000-anaphoric-macros))

(defconst oo-lisp-dir (expand-file-name "lisp/" user-emacs-directory)
  "Directory where handcrafted configuration files go.")

(defconst oo-local-dir (expand-file-name ".local/" user-emacs-directory)
  "Directory that stores subdirectories.")

(defconst oo-etc-dir (expand-file-name "etc/" oo-local-dir)
  "Directory where package configuration files go.")

(defvaralias 'oo-data-dir 'oo-etc-dir)

(defconst oo-var-dir (expand-file-name "var/" oo-local-dir)
  "Directory where persistent data files go.")

(defvaralias 'oo-cache-dir 'oo-var-dir)

(defvar oo-custom-faces-alist nil
  "An alist that update the background of faces based on existing faces.

Each element is of the form (custom-face . built-in-face).  Whenever the theme
is changed CUSTOM-FACE updates its background and foreground similar to
built-in-face.  See `oo--enable-theme-functions--set-state-faces-from-theme-h'.")

(defvar oo-after-init-features nil
  "Features that should be required just after initializing Emacs.")

(defun oo-require-after-init-features-h ()
  (each! oo-after-init-features
    (require it)))

(add-hook 'after-init-hook #'oo-require-after-init-features-h 0)

;; I need to process the `command-line-args' for font here so that I can set the
;; font before the frame is loaded.
(defvar oo-init-theme (aremf! command-line-args
                        (and (string-match "^--theme=\\(.+\\)" it)
                             (intern (match-string 1 it))))
  "Initial theme.
This is the theme set at startup.  If nil, no theme is set.")

(defvar oo-init-font (aremf! command-line-args
                       (and (string-match "^--font=\\(.+\\)" it)
                            (match-string 1 it)))
  "Initial font.
This is the font set at startup.  If nil, no font is set.")

(defvar oo-init-profile-p (aremf! command-line-args
                            (and (string-match "^--profile" it)
                                 t))
  "Non-nil if files loaded at startup should be profiled.")

(defvar oo-init-noerrors-p (aremf! command-line-args
                             (and (string-match "^--noerrors" it)
                                  t))
  "Non-nil if errors in init files should be ignored at startup.")

(defvar oo-after-init-hook-ran-p nil
  "Non-nil if `after-init-hook' ran.")

(defun oo-after-init-hook-ran-h ()
  "Record that `after-init-hook' ran."
  (setq oo-after-init-hook-ran-p t))

(add-hook 'after-init-hook #'oo-after-init-hook-ran-h)

(defvar oo-emacs-startup-hook-ran-p nil
  "Non-nil if `emacs-startup-hook' ran.")

(defun oo-emacs-startup-hook-ran-h ()
  "Record that `emacs-startup-hook' ran."
  (setq oo-emacs-startup-hook-ran-p t))

(add-hook 'emacs-startup-hook #'oo-emacs-startup-hook-ran-h)

(defvar oo-init-data nil
  "Initialization data.
This includes the time that files took to load.")
;; This list could contain (feature timetoload).
;;; provide
(provide '001-base-vars)
;;; 001-base-vars.el ends here
