;;; init.el --- My emacs configuration -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Free Software Foundation, Inc.
;;
;; Author: Luis Henriquez-Perez <luis@luishp.xyz>
;; Maintainer: Luis Henriquez-Perez <luis@luishp.xyz>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
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
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This is my personal emacs configuration.  Please refer to the
;; README for information on how to run and modify them.
;;
;;; Code:
;;;; disable garbage collection until after startup
;; https://medium.com/@danielorihuelarodriguez/optimize-emacs-start-up-time-ae314201e04f
;; https://news.ycombinator.com/item?id=39127859
;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.8)
;;;; don't search for whenever a package is loaded
;; Credits to irreal for sharing that keywords can be used as registers in his
;; blog post (https://irreal.org/blog/?p=12386).
(set-register :file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
;;;; prevent flashing of unstyled modeline
;; Don't render the modeline on startup.  For one thing, the startup looks
;; better without flashing stuff on the screen.  Additionally, the more that's
;; saved on rendering, the faster the startup.
(set-register :mode-line-format mode-line-format)
(setq-default mode-line-format nil)
;;;; set load-path
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
;;;; require! - help me load files robustly
(defmacro require! (feature)
  "Require FEATURE, reporting errors and logging the time it takes to load."
  `(condition-case err
       (let ((start-time))
         (require ,feature)
         (message "Loaded %s in %.2fms"
                  ',feature
                  (* 1000 (float-time (time-subtract (current-time) start-time)))))
     (error
      (message "Error loading %s: %s" ',feature (error-message-string err)))))
;;;; load requirements
(require 'init-no-littering)
;; Built-in packages
(require 'init-abbrev)
(require 'init-auto-insert)
(require 'init-custom)
(require 'init-dired)
(require 'init-emacs-lock)
(require 'init-files)
(require 'init-frame)
(require 'init-hideshow)
(require 'init-loaddefs)
(require 'init-minibuffer)
(require 'init-paren)
(require 'init-project)
(require 'init-saveplace)
(require 'init-simple)
(require 'init-startup)
(require 'init-vc-hooks)
(require 'init-window)
;; External packages
(require 'init-ace-window)
(require 'init-aggressive-indent)
(require 'init-avy)
(require 'init-buffer-terminator)
(require 'init-burly)
(require 'init-captain)
(require 'init-consult)
(require 'init-corfu)
(require 'init-corfu-history)
(require 'init-corfu-quick)
(require 'init-dashboard)
(require 'init-dirvish)
(require 'init-easy-escape)
(require 'init-emmet)
(require 'init-emms)
(require 'init-eshell)
(require 'init-evil)
(require 'init-vertico-multiform)
(require 'init-evil-cleverparens)
(require 'init-evil-collection)
(require 'init-evil-easymotion)
(require 'init-evil-exchange)
(require 'init-evil-fringe-mark)
(require 'init-evil-goggles)
(require 'init-evil-surround)
(require 'init-evil-textobj-anyblock)
(require 'init-evil-textobj-line)
(require 'init-evil-textobj-syntax)
(require 'init-eww)
(require 'init-expreg)
(require 'init-fill-adapt)
(require 'init-grugru)
(require 'init-helm)
(require 'init-helpful)
(require 'init-highlight-quoted)
(require 'init-htmlize)
(require 'init-hungry-delete)
(require 'init-hy-mode)
(require 'init-lispyville)
(require 'init-macroexpand)
(require 'init-magit)
(require 'init-marginalia)
(require 'init-modus-themes)
(require 'init-mu4e)
(require 'init-notmuch)
(require 'init-orderless)
(require 'init-org)
(require 'init-org-appear)
(require 'init-outli)
(require 'init-outline)
(require 'init-pomodoro)
(require 'init-rainbow-delimiters)
(require 'init-re-builder)
(require 'init-recentf)
(require 'init-restart-emacs)
(require 'init-savehist)
(require 'init-smartparens)
(require 'init-super-save)
(require 'init-tab-bar)
(require 'init-tempel)
(require 'init-vertico)
(require 'init-vertico-buffer)
(require 'init-vertico-quick)
(require 'init-w3m)
(require 'init-wdired)
(require 'init-which-key)
(require 'init-window)
(require 'oo-keybindings)
(require 'oo-autoloads)
(require 'oo-init)
;;; provide init
(provide 'init)
;;; init.el ends here
