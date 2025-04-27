;;; 000-base-vars.el --- core variables -*- lexical-binding: t; -*-
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

(defvar oo-debug-p (or (getenv "DEBUG") init-file-debug)
  "When non-nil print debug messages.
The --debug-init flag and setting the DEBUG envar will enable this at startup.")

(defvar oo-init-data nil
  "A record of data concerning loaded files.

This is an alist where each element is of the form (feature start-time end-time
error).  Feature is an init feature, start and end time, and error is any
error.  If an error occurs start and end are nil.  Conversely, if start and end
are non-nil, then error is nil.")

(defvar oo-after-init-hook-time nil
  "Time elapsed by `after-init-hook'.")

(defvar oo-emacs-startup-hook-time nil
  "Time elasped by `emacs-startup-hook'.")

;; This is very basic font setting based on available faces.  I have seen much
;; more complex font setups like in minemacs (which probably got its from doom)
;; but for now this will do.
(defvar oo-default-fonts (list (font-spec :family "JetBrainsMono Nerd Font"
                                          :weight 'regular
                                          :slant 'normal
                                          :width 'normal
                                          :size 18)
                               (font-spec :family "CaskaydiaCove Nerd Font Mono"
                                          :weight 'regular
                                          :slant 'normal
                                          :width 'normal
                                          :size 18)
                               (font-spec :family "Mononoki Nerd Font"
                                          :weight 'regular
                                          :slant 'normal
                                          :width 'normal
                                          :size 18)
                               (font-spec :family "RecMonoDuotone Nerd Font"
                                          :weight 'regular
                                          :slant 'normal
                                          :width 'normal
                                          :size 18))
  "List of fonts to check.")

(defvar oo-custom-faces-alist nil
  "An alist that update the background of faces based on existing faces.

Each element is of the form (custom-face . built-in-face).  Whenever the theme
is changed CUSTOM-FACE updates its background and foreground similar to
built-in-face.  See `oo--enable-theme-functions--set-state-faces-from-theme-h'.")
;;; provide
(provide '000-base-vars)
;;; 000-base-vars.el ends here
