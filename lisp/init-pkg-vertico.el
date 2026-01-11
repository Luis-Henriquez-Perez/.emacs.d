;;; init-pkg-vertico.el --- initialize vertico -*- lexical-binding: t; -*-
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
;; Initialize vertico.
;;
;;; Code:
(require 'init-core)

(add-hook 'vertico-mode-hook #'vertico-buffer-mode)

(o-opt vertico-buffer-display-action
      '(display-buffer-in-direction
        (direction . below)
        (window-height . ,(+ 3 vertico-count))))

(o-popup-at-bottom "\\*Vertico")

(add-hook 'o-first-input-hook #'vertico-mode)
;; TODO: make conditional based on whether icons are available.
(add-hook 'vertico-mode-hook #'nerd-icons-completion-mode)

(add-hook 'vertico-mode-hook #'vertico-multiform-mode)

;; (o-pushing vertico-multiform-commands '(Info-menu (vertico-sort-function . nil)))
(o-opt vertico-multiform-commands
      '((Info-menu (vertico-sort-function . nil))
        ;; (execute-extended-command (vertico-sort-function . vertico-sort-history-alpha))
        ;; (t (vertico-sort-function . vertico-sort-history-length-alpha))
        ))

(o-opt vertico-quick1 "asdfgh")
(o-opt vertico-quick2 "jkluionm")

(o-opt vertico-count-format nil)
(o-opt vertico-count 15)
;;; provide
(provide 'init-pkg-vertico)
;;; init-pkg-vertico.el ends here
