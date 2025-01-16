;;; init-yasnippet.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; TODO: add commentary
;;
;;; Code:
:hook (prog-mode-hook . yas-minor-mode-on)
:setq
(yas-snippet-dirs . (list (concat VOID:DATA-DIR "snippets/")))
(yas-verbosity . (if void:debug-p 3 0))
(yas-indent-line . 'auto)
(yas-prompt-functions . '(yas-completing-prompt yas-ido-prompt))
(yas-use-menu . nil)
(yas-triggers-in-field . t)
:defer-config
(--each yas-snippet-dirs (mkdir it t))
(delq #'yas-dropdown-prompt yas-prompt-functions)
(after! smartparens
        ;; tell smartparens overlays not to interfere with yasnippet keybinds
        (advice-add #'yas-expand :before #'sp-remove-active-pair-overlay))
;;; provide
(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
