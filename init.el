;;; init.el --- My emacs configuration -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Free Software Foundation, Inc.
;;
;; Author: Luis Henriquez Perez <luis@luishp.xyz>
;; Maintainer: Luis Henriquez Perez <luis@luishp.xyz>
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
;; Load the initialization files for packages.
;;
;;; Code:
;;;; LOAD INITIAL FILES
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(let (file-name-handler-alist) (require 'init-core))
;;;; SETUP
(o-require init-pkg-elpaca)
(o-require init-pkg-no-littering)
;;;; BUILT-IN
(o-require init-pkg-abbrev)
(o-require init-pkg-auto-insert)
(o-require init-pkg-completion-preview)
(o-require init-pkg-dabbrev)
(o-require init-pkg-dired)
(o-require init-pkg-eshell)
(o-require init-pkg-eww)
(o-require init-pkg-gnus)
(o-require init-pkg-goto-address)
(o-require init-pkg-icomplete)
(o-require init-pkg-outline)
(o-require init-pkg-proceed)
(o-require init-pkg-re-builder)
(o-require init-pkg-recentf)
(o-require init-pkg-savehist)
(o-require init-pkg-saveplace)
(o-require init-pkg-wdired)
(o-require init-pkg-which-key)
(o-require init-pkg-zone)
;;;; MEEP
(o-require init-pkg-bray)
(o-require init-pkg-meep)
(o-require init-pkg-visible-mark)
;;;; ORG
(o-require init-pkg-org)
(o-require init-pkg-org-appear)
(o-require init-pkg-org-fancy-priorities)
(o-require init-pkg-org-pretty-tags)
(o-require init-pkg-org-superstar)
;;;; EDITING
(o-require init-pkg-aggressive-indent)
(o-require init-pkg-avy)
(o-require init-pkg-captain)
(o-require init-pkg-easy-escape)
(o-require init-pkg-fill-adapt)
(o-require init-pkg-grugru)
(o-require init-pkg-helpful)
(o-require init-pkg-highlight-quoted)
(o-require init-pkg-orderless)
(o-require init-pkg-rainbow-delimiters)
(o-require init-pkg-smartparens)
(o-require init-pkg-super-save)
(o-require init-pkg-ws-butler)
(o-require init-pkg-emmet-mode)
;;;; UNCATEGORIZED
(o-require init-repeat-fu)
(o-require init-pkg-vertico)
(o-require init-pkg-dimmer)
(o-require init-pkg-denote)
(o-require init-pkg-notmuch)
(o-require init-pkg-ace-window)
(o-require init-pkg-burly)
(o-require init-pkg-consult)
(o-require init-pkg-dashboard)
(o-require init-pkg-dirvish)
(o-require init-pkg-macrostep)
(o-require init-pkg-outli)
(o-require init-pkg-elfeed)
(o-require init-pkg-htmlize)
(o-require init-pkg-w3m)
(o-require init-pkg-yeetube)
(o-require init-pkg-emms)
(o-require init-pkg-lispy)
;;;; DISABLED
;; (o-require init-pkg-cape)
;; (o-require init-pkg-corfu)
;; (o-require init-pkg-hungry-delete)
;; (o-require init-pkg-hy-mode)
;; (o-require init-pkg-magit)
;; (o-require init-pkg-marginalia)
;; (o-require init-pkg-mu4e)
;; (o-require init-pkg-pomodoro)
;; (o-require init-pkg-restart-emacs)
;; (o-require init-pkg-tempel)
;;;; KEYBINDINGS
(o-require my-keybindings)
;;; provide init
(provide 'init)
;;; init.el ends here
