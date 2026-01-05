;;; my-keybindings.el --- Initialize keybindings -*- lexical-binding: t; -*-
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
;; Initialize keybindings.
;;
;;; Code:
(require 'base-lib)
(require 'bind-key)
;;;; miscellaneous
(declare-function minibuffer-keyboard-quit "delsel")
(declare-function evil-normal-state "evil")
;;;; leader bindings
;;;;; window
(o-defvar-keymap o-window-map
  "D" #'delete-other-windows
  "M" #'maximize-window
  "S" #'burly-bookmark-windows
  "b" #'balance-windows
  "d" #'delete-window
  "h" #'split-window-vertically
  "j" #'ace-window
  "k" #'display-buffer
  "o" #'ace-window
  "s" #'ace-swap-window
  "t" #'transpose-frame
  "u" #'winner-undo
  "v" #'split-window-horizontally
  "w" #'ace-window)
;;;;; git
(o-defvar-keymap o-git-map
  "B" #'magit-branch
  "b" #'vc-switch-branch
  "c" #'magit-commit
  "g" #'magit-status
  ;; "j" #'o-dwim-vc-action
  "l" #'vc-switch-branch
  ;; "n" #'o-dwim-vc-action
  "p" #'magit-push
  "r" #'vc-register
  "s" #'magit-status)
;;;;; org
(o-defvar-keymap o-org-map
  :prefix 'o-org-map
  "t" #'org-capture|todo
  "j" #'org-capture|todo
  "a" #'org-archive-subtree
  "l" #'org-clock-in-last
  "i" #'org-clock-in
  "k" #'org-clock-in
  "o" #'org-clock-out
  "s" #'org-add-note
  "n" #'org-add-note
  "p" #'org-capture|plain)
;;;;; app
(o-defvar-keymap o-screenshot-map
  "r" #'escr-region-screenshot
  "f" #'escr-frame-screenshot
  "w" #'escr-window-screenshot)

(o-defvar-keymap o-app-map
  "E" #'restart-emacs-start-new-emacs
  "d" #'dired-jump
  "j" #'org-capture|todo
  "n" #'notmuch
  "e" #'eshell
  "f" #'elfeed
  "s" '("screenshot" . o-screenshot-map))
;;;;; toggle
(o-defvar-keymap o-toggle-map
  "c" #'blink-cursor-mode
  "g" #'grugru
  "s" #'smartparens-mode
  "r" #'o-load-random-theme
  "t" #'load-theme
  "h" #'whitespace-mode
  "W" #'whitespace-mode
  "w" #'widen
  "l" #'display-line-numbers-mode
  "u" #'toggle-truncate-lines
  "n" #'o-dwim-narrow
  "e" #'eval-expression
  "f" #'o-set-font-face
  "d" #'toggle-debug-on-error
  "S" #'profiler-start
  "P" #'profiler-stop)
;;;;; buffer
(o-defvar-keymap o-buffer-map
  "x" #'kill-current-buffer
  "b" #'switch-to-buffer
  "j" #'next-buffer
  "k" #'previous-buffer)
;;;;; help
(o-defvar-keymap o-help-map
  "m" #'describe-mode
  "l" #'describe-function
  "f" #'describe-function
  "j" #'describe-variable
  "v" #'describe-variable
  "h" #'describe-variable
  "c" #'describe-char
  "C" #'describe-char
  "k" #'describe-key
  "a" #'describe-face
  "F" #'describe-face)
;;;;; find
(o-defvar-keymap o-find-map
  "t" #'tab-switch
  ";" #'save-buffer
  "o" #'find-file
  "E" #'o-open-emacs-config
  "I" #'o-open-emacs-init-file
  "L" #'o-open-emacs-lisp-dir
  "G" #'rgrep
  "p" #'consult-yank-pop
  "k" #'consult-bookmark
  "l" #'consult-line
  "h" #'consult-outline
  "g" #'consult-grep
  "z" #'ace-link
  "b" #'burly-open-bookmark
  "i" #'imenu
  ;; "j" #'o-dwim-vc-action
  "n" #'o-new-buffer
  "f" #'switch-to-buffer
  "a" #'find-library
  "d" #'pop-to-buffer)
;;;;; quit
(o-defvar-keymap o-quit-map
  "R" #'restart-emacs
  "E" #'restart-emacs-start-new-emacs
  "r" #'restart-emacs
  "k" #'oo/kill-emacs-no-errors
  "Q" #'oo/kill-emacs-no-hook
  "q" #'save-buffers-kill-emacs)
;;;;; music
(o-defvar-keymap o-music-map
  "e" #'o-emms-playlist-mode-go
  "l" #'emms-toggle-repeat-track
  "g" #'emms-playlist-mode-go
  "f" #'emms-play-file
  "p" #'emms-pause
  "P" #'emms-stop
  "r" #'emms-random
  "R" #'emms-toggle-repeat-playlist
  "v" #'emms-volume-lower
  "V" #'emms-volume-raise
  "s" #'emms-seek-to)
;;;;; package
(o-defvar-keymap o-package-map
  "l" #'list-packages
  "i" #'package-install
  "d" #'package-install)
;;;;; quick map
(o-defvar-keymap o-quick-map
  "j" #'org-capture|todo
  "a" #'org-archive-subtree
  "g" #'grugru
  "i" #'tempel-insert
  "l" #'tempel-insert)
;;;;; leader map
(o-defvar-keymap o-leader-map
  "SPC" #'execute-extended-command
  ";" #'+org-agenda-day-view
  "a" '("app" . o-app-map)
  "b" '("buffer" . o-buffer-map)
  "e" '("music" . o-music-map)
  "f" '("find" . o-find-map)
  "g" '("git" . o-git-map)
  "h" '("help" . o-help-map)
  "j" '("quick" . o-quick-map)
  "l" #'consult-buffer
  "y" #'o-load-random-theme
  "s" #'o-load-random-theme
  "d" #'transwin-toggle
  "k" #'evil-keypad-start
  "p" '("package" . o-package-map)
  "t" '("toggle" . o-toggle-map)
  "w" '("window" . o-window-map)
  "q" '("quit" . o-quit-map))

(add-hook 'emacs-startup-hook #'override-global-mode)
;;;; UNCATEGORIZED
(declare-function which-key-add-keymap-based-replacements "which-key")

(o-defafter o-after--register-localleader-with-which-key (which-key)
  (which-key-add-keymap-based-replacements o-leader-map "m" "localleader"))

;; (keymap-set evil-motion-state-map "o" #'evil-forward-WORD-begin)
;;; provide
(provide 'my-keybindings)
;;; my-keybindings.el ends here
