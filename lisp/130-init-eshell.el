;;; 130-init-eshell.el --- initialize eshell -*- lexical-binding: t; -*-
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
(require '050-base)

(autoload! eshell-z "eshell-z")
(autoload! eshell-up "eshell-up")
(autoload! epe-theme-pipeline "eshell-prompt-extras")

(hook! eshell-mode-hook abbrev-mode)
(hook! eshell-mode-hook smartparens-mode)
(hook! eshell-mode-hook eat-eshell-mode)
(hook! eshell-mode-hook eshell-syntax-highlighting-mode)
;; Do not let me kill the eshell buffer, at least not easily.
(hook! eshell-mode-hook emacs-lock-mode)

(oo-popup-at-bottom "\\*eshell")

(opt! eshell-banner-message "")
(opt! eshell-highlight-prompt nil)
(autoload! oo-eshell-prompt "990-config-eshell")
;; For now outsource to epe, but later ill
(opt! eshell-prompt-function 'epe-theme-pipeline)
;; This is obsolete as of Emacs 30.1.
(opt! eshell-prompt-regexp "^[^λ]+λ ")
(opt! eshell-hist-ignoredups t)
;; Prefer external commands over lisp functions.
(opt! eshell-prefer-lisp-functions nil)
;; Represent buffers as #<buffer-name>
(opt! eshell-buffer-shorthand t)
;; boost eshell history-size
;; Increase the history size from 128 to 1000.
(opt! eshell-history-size 1000)
;; Prefer system functions over built-ins.
(opt! eshell-prefer-lisp-functions nil)
;; By "highlight" eshell does not just mean coloring the font with the
;; `eshell-prompt' face.  It also makes the prompt read-only.  Strangely, the
;; prompt is not read-only by default.  Furthermore, there is no way to override
;; the text properties `eshell-emit-prompt' adds to the prompt without advising
;; the it.
(opt! eshell-highlight-prompt t)
(opt! eshell-hist-ignoredups t)
;; boost eshell history-size
;; Increase the history size from 128 to 1000.
(opt! eshell-history-size 1000)
;; Stop eshell from printing messages.
(declare-function eshell-unload-all-modules "eshell")
(advice-add #'eshell-unload-all-modules :around #'oo-funcall-quietly)

(advice-add #'eshell-mode :around #'oo-funcall-quietly)

(nmap eshell-mode-map "J" #'eshell-previous-prompt)
(nmap eshell-mode-map "K" #'eshell-next-prompt)
;;; provide
(provide '130-init-eshell)
;;; 130-init-eshell.el ends here
