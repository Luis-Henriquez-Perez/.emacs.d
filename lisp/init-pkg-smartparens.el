;;; init-pkg-smartparens.el --- initialize smartparens -*- lexical-binding: t; -*-
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
(require 'init-core)

(o-declare-package 'smartparens)

(declare-function smartparens-strict-mode "smartparens")

(autoload 'smartparens-strict-mode "smartparens" nil nil 'function)
(autoload 'smartparens-mode "smartparens" nil nil 'function)
(autoload 'turn-on-show-smartparens-mode "smartparens" nil nil 'function)

;; (add-hook 'text-mode-hook #'turn-on-show-smartparens-mode)
(add-hook 'text-mode-hook #'smartparens-mode)
(add-hook 'prog-mode-hook #'smartparens-mode)
;; (add-hook 'prog-mode-hook #'turn-on-show-smartparens-mode)
(add-hook 'eshell-mode-hook #'smartparens-mode)

;; This allows me to have parens completion when I invoke the command `eval-expression'.
(defun smartparens|enable-in-minibuffer ()
  "Enable `smartparens-mode' in the minibuffer."
  (when (memq this-command '(eval-expression evil-ex))
    (require 'smartparens)
    (smartparens-strict-mode 1)))

(add-hook 'minibuffer-setup-hook #'smartparens|enable-in-minibuffer)

(o-opt sp-highlight-wrap-tag-overlay nil)
(o-opt sp-highlight-pair-overlay nil)
(o-opt sp-highlight-wrap-overlay nil)
(o-opt sp-show-pair-delay 0.2)

(o-require-after-load 'smartparens 'init-after-smartparens)
;;; provide
(provide 'init-pkg-smartparens)
;;; init-pkg-smartparens.el ends here
