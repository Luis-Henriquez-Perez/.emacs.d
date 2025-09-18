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

;; Credits to irreal for sharing that keywords can be used as registers in his
;; blog post (https://irreal.org/blog/?p=12386).
(set-register :file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(advice-add 'x-apply-session-resources :override #'ignore)

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

(push (expand-file-name "lisp/" user-emacs-directory) load-path)

(require '001-base-vars)
(require '002-base-log)
(eval-when-compile (require '003-base-loader))

(require! "^0[15]")

;; Silence byte-compilation warnings.  The compiler cannot tell that I define
;; these variables in the previous `require!' macro.
(defvar oo-cache-dir)
(defvar oo-initial-font)
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache (expand-file-name "eln-cache/" oo-cache-dir)))

(when oo-init-font
  (push `(font . ,oo-init-font) default-frame-alist))

;; Adding advice triggers the creation of the "eln-cache" directory.  To avoid
;; creating it prematurely advices should go after `startup-redirect-eln-cache'.

;; They made the process of disabling this more difficult.
(advice-add #'display-startup-echo-area-message :around #'ignore)

;; The built-in package `woman' overwrites the existing variable
;; `woman-topic-history' by aliasing it to `Man-topic-history' and emacs tells
;; you this by popping up a *Warnings* buffer whenever woman.el is loaded.  This
;; whole thing is probably some bug.  So I stop this whole thing from happening.
(defun oo--suppress-woman-warning (orig-fn &rest args)
  (pcase args
    (`(woman-topic-history Man-topic-history . ,_)
     (advice-remove 'defvaralias #'oo--suppress-woman-warning))
    (_
     (apply orig-fn args))))

(advice-add 'defvaralias :around #'oo--suppress-woman-warning)

;; Essentially, I am telling all Emacs functions that prompt the user for a =yes=
;; or =no= to instead allow me to type =y= or =p=.  [[helpfn:yes-or-no-p][yes-or-no-p]] is defined in c
;; source code.
(advice-add #'yes-or-no-p :override #'y-or-n-p)

(advice-add 'custom-save-all :override #'ignore)
;;; provide early-init
(provide 'early-init)
;;; early-init.el ends here
