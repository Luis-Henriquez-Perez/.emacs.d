;;; 160-keybindings.el --- Initialize keybindings -*- lexical-binding: t; -*-
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
(require! "^0")
(require 'bind-key)
;;;; miscellaneous
(declare-function minibuffer-keyboard-quit "delsel")
(declare-function evil-normal-state "evil")
;;;; leader bindings
;;;;; window
(defvar-keymap! oo-window-map
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
(defvar-keymap! oo-git-map
  "B" #'magit-branch
  "b" #'vc-switch-branch
  "c" #'magit-commit
  "g" #'magit-status
  ;; "j" #'oo-dwim-vc-action
  "l" #'vc-switch-branch
  ;; "n" #'oo-dwim-vc-action
  "p" #'magit-push
  "r" #'vc-register
  "s" #'magit-status)
;;;;; org
(defvar-keymap! oo-org-map
  :prefix 'oo-org-map
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
(defvar-keymap! oo-screenshot-map
  "r" #'escr-region-screenshot
  "f" #'escr-frame-screenshot
  "w" #'escr-window-screenshot)

(defvar-keymap! oo-app-map
  "E" #'restart-emacs-start-new-emacs
  "d" #'dired-jump
  "j" #'org-capture|todo
  "n" #'notmuch
  "e" #'eshell
  "f" #'elfeed
  "s" '("screenshot" . oo-screenshot-map))
;;;;; toggle
(defvar-keymap! oo-toggle-map
  "c" #'blink-cursor-mode
  "g" #'grugru
  "s" #'smartparens-mode
  "r" #'oo-load-random-theme
  "t" #'load-theme
  "h" #'whitespace-mode
  "W" #'whitespace-mode
  "w" #'widen
  "l" #'display-line-numbers-mode
  "u" #'toggle-truncate-lines
  "n" #'oo-dwim-narrow
  "e" #'eval-expression
  "f" #'oo-set-font-face
  "d" #'toggle-debug-on-error
  "S" #'profiler-start
  "P" #'profiler-stop)
;;;;; buffer
(defvar-keymap! oo-buffer-map
  "x" #'kill-current-buffer
  "b" #'switch-to-buffer
  "j" #'next-buffer
  "k" #'previous-buffer)
;;;;; help
(defvar-keymap! oo-help-map
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
(defvar-keymap! oo-find-map
  "t" #'tab-switch
  ";" #'save-buffer
  "o" #'find-file
  "E" #'oo-open-emacs-config
  "I" #'oo-open-emacs-init-file
  "L" #'oo-open-emacs-lisp-dir
  "G" #'rgrep
  "p" #'consult-yank-pop
  "k" #'consult-bookmark
  "l" #'consult-line
  "h" #'consult-outline
  "g" #'consult-grep
  "z" #'ace-link
  "b" #'burly-open-bookmark
  "i" #'imenu
  ;; "j" #'oo-dwim-vc-action
  "n" #'oo-new-buffer
  "f" #'switch-to-buffer
  "a" #'find-library
  "d" #'pop-to-buffer)
;;;;; quit
(defvar-keymap! oo-quit-map
  "R" #'restart-emacs
  "E" #'restart-emacs-start-new-emacs
  "r" #'restart-emacs
  "k" #'oo/kill-emacs-no-errors
  "Q" #'oo/kill-emacs-no-hook
  "q" #'save-buffers-kill-emacs)
;;;;; music
(defvar-keymap! oo-music-map
  "e" #'oo-emms-playlist-mode-go
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
(defvar-keymap! oo-package-map
  "l" #'list-packages
  "i" #'package-install
  "d" #'package-install)
;;;;; quick map
(defvar-keymap! oo-quick-map
  "j" #'org-capture|todo
  "a" #'org-archive-subtree
  "g" #'grugru
  "i" #'tempel-insert
  "l" #'tempel-insert)
;;;;; leader map
(defvar-keymap! oo-leader-map
  "SPC" #'execute-extended-command
  ";" #'+org-agenda-day-view
  "a" '("app" . oo-app-map)
  "b" '("buffer" . oo-buffer-map)
  "e" '("music" . oo-music-map)
  "f" '("find" . oo-find-map)
  "g" '("git" . oo-git-map)
  "h" '("help" . oo-help-map)
  "j" '("quick" . oo-quick-map)
  "l" #'consult-buffer
  "y" #'oo-load-random-theme
  "s" #'oo-load-random-theme
  "d" #'transwin-toggle
  "k" #'evil-keypad-start
  "p" '("package" . oo-package-map)
  "t" '("toggle" . oo-toggle-map)
  "w" '("window" . oo-window-map)
  "q" '("quit" . oo-quit-map))

(add-hook 'emacs-startup-hook #'override-global-mode)
;;;; UNCATEGORIZED
(declare-function which-key-add-keymap-based-replacements "which-key")

(defafter! oo-register-localleader-with-which-key (which-key)
  (which-key-add-keymap-based-replacements oo-leader-map "m" "localleader"))

;; (keymap-set evil-motion-state-map "o" #'evil-forward-WORD-begin)
;;; provide
(provide '160-keybindings)
;;; 160-keybindings.el ends here
