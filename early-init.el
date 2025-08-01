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

(defvar oo-initial-theme nil
  "Theme to enable during startup.")

;; Just process it here.  Do not wait for command-line switches.
(dolist (argi command-line-args)
  (when (string-match "^--theme=\\([^[:space:]]+\\)" argi)
    (setq oo-initial-theme (intern (match-string 1 argi)))))

;; I need to process the `command-line-args' for font here so that I can set the
;; font before the frame is loaded.
(defvar oo-initial-font nil
  "Initial font.")

(dolist (argi command-line-args)
  (when (string-match "^--font=\\([^[:space:]]+\\)" argi)
    (setq oo-initial-font (intern (match-string 1 argi)))))

(when oo-initial-font
  (push `(font . ,oo-initial-font) default-frame-alist))

(provide 'early-init)
;;; early-init.el ends here
