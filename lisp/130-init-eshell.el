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
(require! "^0[01]")

(setq-hook! eshell-mode-hook 'completion-at-point-functions '(cape-dabbrev pcomplete-completions-at-point t))

(each! '(esh-arg esh-util esh-proc esh-io esh-cmd em-dirs em-hist em-prompt em-term em-ls em-glob em-basic em-script em-cmpl em-smart)
  (push it oo-idle-features))

(autoload 'eshell-z "eshell-z" nil t 'function)
(autoload 'eshell-up "eshell-up" nil t 'function)

(add-hook 'eshell-mode-hook #'abbrev-mode)
(add-hook 'eshell-mode-hook #'smartparens-mode)
(add-hook 'eshell-mode-hook #'eat-eshell-mode)
(add-hook 'eshell-mode-hook #'eshell-syntax-highlighting-mode)
;; Do not let me kill the eshell buffer, at least not easily.
;; (add-hook 'eshell-mode-hook #'emacs-lock-mode)

(oo-popup-at-bottom "\\*eshell")

(declare-function eshell-unload-all-modules "eshell")
(advice-add #'eshell-unload-all-modules :around #'oo-call-quietly-a)

(advice-add #'eshell-mode :around #'oo-call-quietly-a)
;;; provide
(provide '130-init-eshell)
;;; 130-init-eshell.el ends here
