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
(require! init-package)
(require! init-no-littering)
(require! 127-hooks)
(require! init-macrostep)
(require! init-proceed)
(require! init-goto-address)
(require! init-ws-butler)
(require! init-dabbrev)
(require! init-abbrev)
(require! init-ace-window)
(require! init-aggressive-indent)
(require! init-auto-insert)
(require! init-avy)
(require! init-burly)
(require! init-cape)
(require! init-captain)
(require! init-completion-preview)
(require! init-consult)
(require! init-corfu)
(require! init-dashboard)
(require! init-denote)
(require! init-dimmer)
(require! init-dired)
(require! init-dirvish)
(require! init-easy-escape)
(require! init-elfeed)
(require! init-emmet)
(require! init-emms)
(require! init-escr)
(require! init-eshell)
(require! init-evil-collection)
(require! init-evil-easymotion)
(require! init-evil-fringe-mark)
(require! init-evil-goggles)
(require! init-evil-nerd-commenter)
(require! init-evil-surround)
(require! init-evil-textobj-anyblock)
(require! init-evil-textobj-line)
(require! init-evil-textobj-syntax)
(require! init-evil)
(require! init-eww)
(require! init-fill-adapt)
(require! init-gnus)
(require! init-grugru)
(require! init-helm)
(require! init-helpful)
(require! init-highlight-quoted)
(require! init-htmlize)
(require! init-hungry-delete)
(require! init-hy-mode)
(require! init-icomplete)
(require! init-lispyville)
(require! init-magit)
(require! init-marginalia)
(require! init-mu4e)
(require! init-notmuch)
(require! init-orderless)
(require! init-org-appear)
(require! init-org-fancy-priorities)
(require! init-org-pretty-tags)
(require! init-org-superstar)
(require! init-org)
(require! init-outli)
(require! init-outline)
(require! init-pomodoro)
(require! init-rainbow-delimiters)
(require! init-re-builder)
(require! init-recentf)
(require! init-restart-emacs)
(require! init-savehist)
(require! init-saveplace)
(require! init-smartparens)
(require! init-super-save)
(require! init-tempel)
(require! init-vertico)
(require! init-w3m)
(require! init-wdired)
(require! init-which-key)
(require! init-yeetube)
(require! init-zone)
(require! 160-keybindings)
;;; provide init
(provide 'init)
;;; init.el ends here
