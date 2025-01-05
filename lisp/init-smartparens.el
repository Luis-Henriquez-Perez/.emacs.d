;;; init-smartparens.el --- initialize smartparens -*- lexical-binding: t; -*-
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
;; Initialize smartparens.
;;
;;; Code:
(require 'base)

(hook! text-mode-hook turn-on-show-smartparens-mode)
(hook! text-mode-hook smartparens-mode)
(hook! prog-mode-hook smartparens-mode)
(hook! prog-mode-hook turn-on-show-smartparens-mode)

;; This allows me to have parens completion when I invoke the command `eval-expression'.
(defhook! oo-enable-smartparens-if-in-minibuffer-h (minibuffer-setup-hook)
  "Enable `smartparens-mode' in the minibuffer."
  (when (memq this-command '(eval-expression evil-ex))
    (require 'smartparens)
    (smartparens-strict-mode 1)))

(opt! sp-highlight-wrap-tag-overlay nil)
(opt! sp-highlight-pair-overlay nil)
(opt! sp-highlight-wrap-overlay nil)
(opt! sp-show-pair-delay 0.2)

(bind! oo-toggle-map "s" #'smartparens-mode)
;;; provide
(provide 'init-smartparens)
;;; init-smartparens.el ends here
