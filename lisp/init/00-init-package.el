;;; 00-init-package.el --- Initialize package -*- lexical-binding: t; -*-
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
;; Initialize package.
;; Install all of my packages.
;;
;;; Code:
;; If this variable is not set beforehand, `package-gnupghome-dir' will not be
;; set to the right place.
(defvar package-user-dir)
(setq package-user-dir (locate-user-emacs-file "packages/"))

(require 'cl-lib)
(require 'package)
(require 'package-vc)
(require 'base-lib)
(eval-when-compile (require 'base-macros))

;; This was taken from prot's recommendation.
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

(add-to-list 'package-archives '("gnu-elpa"       . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("gnu-elpa-devel" . "https://elpa.gnu.org/devel/"))
(add-to-list 'package-archives '("melpa"          . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("nongnu"         . "https://elpa.nongnu.org/nongnu/"))

(setq package-archive-priorities '(("melpa" . 10) ("gnu-elpa" . 9) ("nongnu" . 8)))

'(ace-jump-helm-line
  ace-link
  ace-window
  aggressive-indent
  all-the-icons
  all-the-icons-nerd-fonts
  annalist
  beacon
  buffer-terminator
  burly
  cape
  caps-lock
  captain
  consult
  corfu
  cyberpunk-theme
  dashboard
  denote
  dimmer
  dirvish
  doct
  easy-escape
  eat
  edit-indirect
  elfeed
  elmacro
  embark
  embark-consult
  emmet-mode
  emms
  eros
  eshell-prompt-extras
  eshell-syntax-highlighting
  eshell-up
  eshell-z
  esup
  evenok
  evil
  evil-cleverparens
  evil-collection
  evil-exchange
  evil-fringe-mark
  evil-goggles
  evil-iedit-state
  evil-lisp-state
  evil-nerd-commenter
  evil-surround
  evil-textobj-column
  evil-textobj-entire
  evil-textobj-line
  evil-textobj-syntax
  evil-visual-mark-mode
  expreg
  exwm
  f
  fancy-narrow
  fennel-mode
  filladapt
  fit-text-scale
  git-gutter
  goto-chg
  green-screen-theme
  grugru
  gruvbox-theme
  habamax-theme
  helm
  helm-system-packages
  helpful
  hemera-theme
  hide-mode-line
  highlight-indent-guides
  highlight-quoted
  htmlize
  ctable
  humanoid-themes
  hungry-delete
  hy-mode
  hydra
  immaterial-theme
  leuven-theme
  doom-themes
  lispy
  lispyville
  loopy
  lorem-ipsum
  lua-mode
  macrostep
  magit
  marginalia
  markdown-mode
  material-theme
  minimal-theme
  modus-themes
  monkeytype
  monokai-theme
  nerd-icons
  no-littering
  noccur
  notmuch
  one-themes
  orderless
  org-appear
  org-bookmark-heading
  org-fancy-priorities
  org-ml
  org-pretty-tags
  org-superstar
  org-tidy
  orglink
  password-store
  pcre2el
  php-mode
  polymode
  pomodoro
  rainbow-delimiters
  rainbow-mode
  redacted
  restart-emacs
  s
  sdcv
  sly
  smartparens
  solarized-theme
  spacemacs-theme
  ssh-agency
  standard-themes
  stripe-buffer
  sudo-edit
  super-save
  tango-plus-theme
  telephone-line
  tempel
  tempel-collection
  textsize
  transpose-frame
  transwin
  ts
  vc-auto-commit
  vertico
  w3m
  web-mode
  wgrep
  which-key
  ws-butler
  xr
  yeetube
  zoutline)

(emacs-wallpaper :url "https://github.com/Luis-Henriquez-Perez/emacs-wallpaper" :branch "mine")
(escr :url "https://github.com/Luis-Henriquez-Perez/escr")
(evil-easymotion :url "https://github.com/Luis-Henriquez-Perez/evil-easymotion" :branch "master")
(on :url "https://github.com/ajgrf/on.el")
(outli :url "https://github.com/jdtsmith/outli")
(spaceline :url "https://github.com/Luis-Henriquez-Perez/spaceline" :branch "add-evil-operator-state-face")
(zone-matrix :url "https://github.com/ober/zone-matrix" :branch "master")

;; The function `package-install-selected-packages' does not activate the
;; packages which causes a problem fo rme.
(unless (bound-and-true-p package--initialized)
  (time-elapsed! (package-initialize)))

(unless package-archive-contents
  (message "Refreshing contents...")
  (time-elapsed! (package-refresh-contents)))

;; (remove-hook 'kill-emacs-hook #'emms-history-save)
;; Manage garbage collection myself.  U shouldn't just disable garbage
;; collection altogether for this becausee ur emacs could crash if it has too
;; much uncollected garbage.
(time-elapsed!
 (let ((refreshed-contents-p nil)
       (gc-cons-threshold most-positive-fixnum))
   (dolist (package package-selected-packages)
     (cond ((assq package package-archive-contents)
            (unless (package-installed-p package)
              (unless refreshed-contents-p
                (package-refresh-contents)
                (setq refreshed-contents-p (not refreshed-contents-p)))
              (message "package is not installed %s package" package)
              (with-demoted-errors "%S" (package-install package 'dont-select))
              (if (package-installed-p package)
                  (garbage-collect)
                (message "Failed to install package `%s'" package))))
           (t
            (message "Package %s is not available." package))))))

(time-elapsed! (package-vc-install-selected-packages))
;;; provide
(provide '00-init-package)
;;; 00-init-package.el ends here
