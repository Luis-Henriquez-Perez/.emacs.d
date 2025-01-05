;;; init-abbrev.el --- initialize abbrev-mode -*- lexical-binding: t; -*-
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
;; Initialize abbrev-mode.
;;
;;; Code:
(require 'base)
;;;; hooks
(hook! prog-mode-hook abbrev-mode)
(hook! text-mode-hook abbrev-mode)
;;;; mode hook
;; There is an idea of loading the abbrevs just before they are needed--as in
;; right as your typing--but doing that would actually cause a noticable delay
;; when typing a character.
(defhook! oo-load-plain-text-abbrevs-h (abbrev-mode-hook)
  (when (derived-mode-p 'text-mode)
    (require 'oo-plain-text-abbrevs))
  (remove-hook 'abbrev-mode-hook #'oo-load-plain-text-abbrevs-h))

(defhook! oo-load-emacs-lisp-mode-abbrevs-h (emacs-lisp-mode-hook)
  (when (derived-mode-p 'emacs-lisp-mode)
    (require 'oo-emacs-lisp-mode-abbrevs))
  (remove-hook 'emacs-lisp-mode-hook #'oo-load-emacs-lisp-mode-abbrevs-h))

(defhook! oo-load-eshell-mode-abbrevs-h (eshell-mode-hook)
  (when (derived-mode-p 'eshell-mode)
    (require 'oo-eshell-mode-abbrevs))
  (remove-hook 'eshell-mode-hook #'oo-load-eshell-mode-abbrevs-h))
;;;; bindings
(bind! "C-c k" #'unexpand-abbrev)
;;;; do not save abbrevs to a file
(advice-add 'read-abbrev-file :around #'ignore)
(advice-add 'write-abbrev-file :around #'ignore)
(advice-add 'abbrev--possibly-save :around #'ignore)
(advice-add 'quietly-read-abbrev-file :around #'ignore)
;;;; setup advices
(autoload 'oo--pulse-expansion "config-abbrev" nil nil 'function)
(autoload 'oo--add-period-maybe "config-abbrev" nil nil 'function)
(autoload 'oo--ensure-self-insert "config-abbrev" nil nil 'function)

(advice-add 'abbrev--default-expand :around #'oo--pulse-expansion)
(advice-add 'abbrev--default-expand :around #'oo--add-period-maybe)
(advice-add 'abbrev--default-expand :around #'oo--ensure-self-insert)
;;; provide
(provide 'init-abbrev)
;;; init-abbrev.el ends here
