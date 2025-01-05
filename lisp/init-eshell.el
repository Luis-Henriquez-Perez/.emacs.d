;;; init-eshell.el --- initialize eshell -*- lexical-binding: t; -*-
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
;; Initialize eshell.
;;
;;; Code:
(require 'base)

(hook! eshell-mode-hook abbrev-mode)
(hook! eshell-mode-hook smartparens-mode)
(hook! eshell-mode-hook eat-eshell-mode)
(hook! eshell-mode-hook eshell-syntax-highlighting-mode)
;; Do not let me kill the eshell buffer, at least not easily.
(hook! eshell-mode-hook emacs-lock-mode)

(autoload #'eshell-z "eshell-z" nil t 'function)
(autoload #'eshell-up "eshell-up" nil t 'function)

(oo-popup-at-bottom "\\*eshell")

(autoload 'epe-theme-lambda "eshell-prompt-extras")

(opt! eshell-banner-message "")
(opt! eshell-highlight-prompt nil)
(opt! eshell-prompt-function 'epe-theme-lambda)
(opt! eshell-hist-ignoredups t)
(opt! eshell-prefer-lisp-functions t)
;; Represent buffers as #<buffer-name>
(opt! eshell-buffer-shorthand t)
;; boost eshell history-size
;; Increase the history size from 128 to 1000.
(opt! eshell-history-size 1000)
;; Eshell prints various messages about loading modules.  These messages
;; originate from the function [[][eshell-unload-all-modules]].  I would rather
;; not see these messages.
(advice-add #'eshell-unload-all-modules :around #'oo-funcall-quietly)
;; At first I thought the culprit was this function, but I was wrong.  The
;; printing comes from =eshell-mode=.  In any case, however, I silence it as
;; well.
(advice-add #'eshell-mode :around #'oo-funcall-quietly)

(bind! oo-app-map "e" #'eshell)
;;; provide
(provide 'init-eshell)
;;; init-eshell.el ends here
