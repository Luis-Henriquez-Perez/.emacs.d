;;; early-init.el --- initial setup -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Luis Henriquez <luis@luishp.xyz>
;;
;; Author: Luis Henriquez <luis@luishp.xyz>
;; Maintainer: Luis Henriquez <luis@luishp.xyz>
;; Version: 0.1
;;
;; Created: 02 Feb 2024
;;
;; URL: https://github.com/Luis-Henriquez-Perez/dotfiles
;;
;; License: GPLv3
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
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;; Code:

;; https://medium.com/@danielorihuelarodriguez/optimize-emacs-start-up-time-ae314201e04f
;; https://news.ycombinator.com/item?id=39127859
;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.8)

(advice-add #'x-apply-session-resources :override #'ignore)

(set-register :mode-line-format mode-line-format)
(setq-default mode-line-format nil)

(setq package-enable-at-startup nil)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)

;; Prevent unwanted runtime compilation for gccemacs (native-comp) users;
;; packages are compiled ahead-of-time when they are installed and site files
;; are compiled when gccemacs is installed.
(defvar native-comp-jit-compilation)
(setq native-comp-jit-compilation nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(left-fringe  . 0) default-frame-alist)
(push '(right-fringe . 0) default-frame-alist)

(defvar oo-startup-theme nil
  "Theme to enable during startup.")

;; Function to load the theme
(defun oo-command-line-switch--theme ()
  "Load a theme passed as --theme THEME on the command line."
  ;; 'switch' is something like "--theme=modus-operandi"
  (message "option -> %S" argi)
  (message "match -> %S" (string-match "^--theme=\\([^[:space:]]+\\)" argi))
  (when (string-match "^--theme=\\([^[:space:]]+\\)" argi)
    (message "In when condition...%S" (match-string 1))
    (setq oo-startup-theme (intern (match-string 1)))
    (message "Set oo-startup-theme->%S" oo-startup-theme)
    (setq command-line-args-left (cdr command-line-args-left))
    t))

(push #'oo-command-line-switch--theme command-line-functions)


(provide 'early-init)
;;; early-init.el ends here
