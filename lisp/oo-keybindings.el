;;; oo-keybindings.el --- Initialize keybindings -*- lexical-binding: t; -*-
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
(require 'base)
(eval-when-compile (require 'base-macros-bind))
(require 'bind-key)
(hook! after-init-hook override-global-mode :depth -100)
;;;; keybinding leaders
;; This file provides leaders keys for evil and non-evil states and it binds
;; these leader keys.

;; These leaders are specifically for evil mode states (not including insert and
;;                                                          Emacs).  I choose the space (=SPC=) key for evil leaders because it is one of if
;; not the easiest key to press because of its central placement on the keyboard
;; and its sheer size--at least on the [[https://en.wikipedia.org/wiki/QWERTY][qwerty]] keyboard that I use.  The choice
;; of =SPC m= for the major mode specific keys is simply for the pnemonic =m= which
;; stands for "major mode".  The short major mode prefix key =,= is for cases when I
;; want to shorten a key binding.  Although obviously not as easy to remember as
;; =m=, it provides me with one shorter keypress in certain situations.
(defconst oo-normal-leader-key "SPC"
  "The evil leader prefix key.")

(defconst oo-normal-localleader-key "SPC l"
  "The localleader prefix key for major-mode specific commands.")

(defconst oo-normal-localleader-short-key ","
  "A shorter alternative `oo-localleader-key'.")
;; These leaders are for evil insert and emacs states as well as vanilla
;; Emacs.  Note that evil Emacs state is different from vanilla Emacs.  One of the
;; goals with these bindings is to set up keybindings in the case that I disable
;; evil mode or in the case that I want to use my bindings in insert or Emacs
;; state--or even vanilla Emacs.  The choice behind the bindings is the same as
;; [[id:][before]], except I just prepended the =Meta= (a.k.a. the =Alt= key) to everything.
(defconst oo-insert-leader-key "M-SPC"
  "The leader prefix key used for Insert state.")

(defconst oo-insert-localleader-key "M-SPC l"
  "The localleader prefix key for major-mode specific commands.")

(defconst oo-insert-localleader-short-key "M-,"
  "A short non-normal `oo-localleader-key'.")

(defconst oo-emacs-leader-key "C-c l"
  "The leader prefix key used for Emacs states.")

(defconst oo-emacs-alt-leader-key "C-c SPC")

(defconst oo-emacs-localleader-key "C-c l l"
  "The localleader prefix key for major-mode specific commands.")
;;;; base bindings
(bind! i "A-x" #'execute-extended-command)
(bind! i "M-x" #'execute-extended-command)
(bind! i "C-c h" #'grugru)
(bind! (n i) "C-c k" #'unexpand-abbrev)
(bind! i [escape] #'oo-dwim-escape)

(bind! (n v) "w" #'+evilem-motion-beginning-of-word)
(bind! (n v) "e" #'+evilem-motion-end-of-word)
(bind! (n v) "W" #'+evilem-motion-beginning-of-WORD)
(bind! (n v) "E" #'+evilem-motion-end-of-WORD)
(bind! (n v o) "f" #'+evilem-motion-char)
(bind! (n v o) "H" #'+evilem-motion-beginning-of-line)
(bind! (n v o) "H" #'+evilem-motion-beginning-of-line)
(bind! v "V" #'expreg-contract)
(bind! v "v" #'expreg-expand)

(bind! n "+" #'text-scale-increase)
(bind! n "-" #'text-scale-decrease)
(bind! n "H" #'evil-first-non-blank)
(bind! n "L" #'evil-last-non-blank)
(bind! n "J" #'evil-scroll-page-down)
(bind! n "K" #'evil-scroll-page-up)

(bind! i lispyville-mode-map "SPC" #'lispy-space)
(bind! i lispyville-mode-map ";" #'lispy-comment)

(bind! (n v) "g u" #'evil-upcase)
(bind! (n v) "g U" #'evil-downcase)
(bind! (n v) "g t" #'evil-goto-first-line)
(bind! (n v) "g b" #'evil-goto-line)
(bind! (n v) "g h" #'+evil-eval-operator)
(bind! (n v) "g r" #'+evil-eval-replace-operator)
(bind! (n v) "g l" #'+evil-eval-print-operator)
(bind! (n v) "g p" #'+evil-eval-print-operator)
(bind! (n v) "g x" #'evil-exchange)
(bind! (n v) "g X" #'evil-exchange-cancel)
(bind! (n v) "g a" #'evil-exchange)
(bind! (n v) "g A" #'evil-exchange-cancel)
(bind! (n v) "g c" #'lispyville-comment-or-uncomment)
(bind! (n v) "g l" #'lispyville-comment-and-clone-dwim)
;;;; text objects
(bind! evil-outer-text-objects-map "c" #'lispyville-outer-comment)
(bind! evil-inner-text-objects-map "c" #'lispyville-inner-comment)
(bind! evil-outer-text-objects-map "h" #'evil-a-syntax)
(bind! evil-inner-text-objects-map "h" #'evil-i-syntax)
(bind! evil-inner-text-objects-map "l" #'evil-inner-line)
(bind! evil-outer-text-objects-map "l" #'evil-a-line)
(bind! evil-inner-text-objects-map "f" #'evil-cp-inner-form)
(bind! evil-outer-text-objects-map "f" #'evil-cp-a-form)
(bind! evil-inner-text-objects-map "b" #'evil-textobj-anyblock-inner-block)
(bind! evil-outer-text-objects-map "b" #'evil-textobj-anyblock-a-block)
;;;; main bindings
;;;;; leader prefix
(bind! i override-global-map oo-insert-leader-key #'oo-leader-prefix-command)
(bind! override-global-map oo-emacs-leader-key  #'oo-leader-prefix-command)
(bind! override-global-map oo-emacs-alt-leader-key  #'oo-leader-prefix-command)
(bind! (n m v) override-global-map oo-normal-leader-key #'oo-leader-prefix-command)
(bind! (n m v) override-global-map ";" #'execute-extended-command)

(defvar oo-leader-map (make-sparse-keymap))
(define-prefix-command 'oo-leader-prefix-command 'oo-leader-map)

(defvar-keymap )

(bind! oo-leader-map ";" #'+org-agenda-day-view)
(bind! oo-leader-map oo-normal-leader-key #'execute-extended-command)
(bind! oo-leader-map "b" #'oo-buffer-prefix-command :wk "buffer")
(bind! oo-leader-map "g" #'oo-git-prefix-command :wk "git")
(bind! oo-leader-map "l" #'oo-git-prefix-command :wk "git")
(bind! oo-leader-map "w" #'oo-window-prefix-command :wk "window")
(bind! oo-leader-map "a" #'oo-app-prefix-command :wk "app")
(bind! oo-leader-map "p" #'oo-package-prefix-command :wk "package")
(bind! oo-leader-map "f" #'oo-find-prefix-command :wk "find")
(bind! oo-leader-map "h" #'oo-help-prefix-command :wk "help")
(bind! oo-leader-map "e" #'oo-emms-prefix-command :wk "emms")
(bind! oo-leader-map "t" #'oo-toggle-prefix-command :wk "toggle")
(bind! oo-leader-map "q" #'oo-quit-prefix-command :wk "quit")
;;;;; window
(defvar oo-window-map (make-sparse-keymap))
(define-prefix-command 'oo-window-prefix-command 'oo-window-map)

(bind! oo-window-map "v" #'split-window-horizontally)
(bind! oo-window-map "h" #'split-window-vertically)
(bind! oo-window-map "b" #'balance-windows)
(bind! oo-window-map "M" #'maximize-window)
(bind! oo-window-map "d" #'delete-window)
(bind! oo-window-map "D" #'delete-other-windows)
(bind! oo-window-map "k" #'display-buffer)
(bind! oo-window-map "u" #'winner-undo)
(bind! oo-window-map "t" #'transpose-frame)
(bind! oo-window-map "s" #'ace-swap-window)
(bind! oo-window-map "w" #'ace-window)
(bind! oo-window-map "j" #'ace-window)
(bind! oo-window-map "o" #'ace-window)
(bind! oo-window-map "S" #'burly-bookmark-windows)
(bind! oo-window-map "b" #'burly-bookmark-windows)
;;;;; git
(defvar oo-git-map (make-sparse-keymap))
(define-prefix-command 'oo-git-prefix-command 'oo-git-map)

(bind! oo-git-map "b" #'vc-switch-branch)
(bind! oo-git-map "l" #'vc-switch-branch)
(bind! oo-git-map "s" #'magit-status)
(bind! oo-git-map "g" #'magit-status)
(bind! oo-git-map "p" #'magit-push)
(bind! oo-git-map "c" #'magit-commit)
(bind! oo-git-map "B" #'magit-branch)
(bind! oo-git-map "n" #'oo-dwim-vc-action)
;;;;; org
(defvar oo-org-map (make-sparse-keymap))
(define-prefix-command 'oo-org-prefix-command 'oo-org-map)

(bind! oo-leader-map "o" #'oo-org-prefix-command :wk "org")
(bind! oo-leader-map "j" #'oo-org-prefix-command :wk "org")

(bind! oo-org-map "t" #'+org-capture-todo)
(bind! oo-org-map "j" #'+org-capture-todo)
(bind! oo-org-map "a" #'org-archive-subtree)
(bind! oo-org-map "l" #'org-clock-in-last)
(bind! oo-org-map "i" #'org-clock-in)
(bind! oo-org-map "k" #'org-clock-in)
(bind! oo-org-map "o" #'org-clock-out)
(bind! oo-org-map "s" #'org-add-note)
(bind! oo-org-map "n" #'org-add-note)
(bind! oo-org-map "p" #'+org-capture-plain)
;;;;; app
(defvar oo-app-map (make-sparse-keymap))
(define-prefix-command 'oo-app-prefix-command 'oo-app-map)

(bind! oo-app-map "j" #'+org-capture-todo)
(bind! oo-app-map "n" #'notmuch)
(bind! oo-app-map "e" #'eshell)
;; (bind! oo-app-map "s" nil :wk "screenshot")
(bind! oo-app-map "s r" #'escr-region-screenshot)
(bind! oo-app-map "s f" #'escr-frame-screenshot)
(bind! oo-app-map "s w" #'escr-window-screenshot)
;;;;; toggle
(define-prefix-command 'oo-toggle-prefix-command 'oo-toggle-map)

(defvar-keymap oo-toggle-map
  :doc "Keymap that contains bindings for things that should be toggled."
  "c" #'blink-cursor-mode
  "g" #'grugru
  "s" #'smartparens-mode
  "t" #'load-theme
  "w" #'whitespace-mode
  "l" #'display-line-numbers-mode
  "u" #'toggle-truncate-lines
  "n" #'oo-dwim-narrow
  "i" #'iedit-mode
  "e" #'eval-expression
  "f" #'oo-set-font-face
  "r" #'read-only-mode
  "d" #'toggle-debug-on-error
  "P" #'profiler-stop)
;;;;; buffer
(defvar oo-buffer-map (make-sparse-keymap))
(define-prefix-command 'oo-buffer-prefix-command 'oo-buffer-map)

(bind! oo-buffer-map "x" #'kill-current-buffer)
(bind! oo-buffer-map "b" #'switch-to-buffer)
(bind! oo-buffer-map "j" #'next-buffer)
(bind! oo-buffer-map "k" #'previous-buffer)
(bind! oo-buffer-map "b" #'switch-to-buffer)
;;;;; help
(defvar oo-help-map (make-sparse-keymap))
(define-prefix-command 'oo-help-prefix-command 'oo-help-map)

(defvar-keymap oo-help-map
  "m" #'describe-mode
  "l" #'describe-function
  "f" #'describe-function
  "j" #'describe-variable
  "v" #'describe-variable
  "h" #'describe-variable
  "C" #'describe-char
  "k" #'describe-key)
;;;;; find
(defvar oo-find-map (make-sparse-keymap))
(define-prefix-command 'oo-find-prefix-command 'oo-find-map)

(bind! oo-find-map ";" #'save-buffer)
(bind! oo-find-map "o" #'find-file)
(bind! oo-find-map "E" #'oo-open-emacs-config)
(bind! oo-find-map "I" #'oo-open-emacs-init-file)
(bind! oo-find-map "L" #'oo-open-emacs-lisp-dir)
(bind! oo-find-map "G" #'rgrep)
(bind! oo-find-map "p" #'consult-yank-pop)
(bind! oo-find-map "k" #'consult-bookmark)
(bind! oo-find-map "b" #'consult-bookmark)
(bind! oo-find-map "l" #'consult-line)
(bind! oo-find-map "h" #'consult-outline)
(bind! oo-find-map "g" #'consult-grep)
(bind! oo-find-map "z" #'ace-link)
(bind! oo-find-map "b" #'burly-open-bookmark)
(bind! oo-find-map ";" #'save-buffer)
(bind! oo-find-map "i" #'imenu)
(bind! oo-find-map "j" #'oo-dwim-narrow)
(bind! oo-find-map "n" #'oo-new-buffer)
(bind! oo-find-map "o" #'find-file)
(bind! oo-find-map "f" #'switch-to-buffer)
(bind! oo-find-map "d" #'display-buffer)
(bind! oo-find-map "a" #'find-library)
(bind! oo-find-map "f" #'switch-to-buffer)
(bind! oo-find-map "d" #'pop-to-buffer)
;;;;; quit
(defvar oo-quit-map (make-sparse-keymap))
(define-prefix-command 'oo-quit-prefix-command 'oo-quit-map)

(bind! oo-quit-map "R" #'restart-emacs)
(bind! oo-quit-map "E" #'restart-emacs-start-new-emacs)
(bind! oo-quit-map "r" #'restart-emacs)
(bind! oo-quit-map "q" #'save-buffers-kill-emacs)
;;;;; workspace
(bind! oo-workspace-map "t" #'tab-select)
(bind! oo-workspace-map "n" #'tab-new)
(bind! oo-workspace-map "j" #'tab-next)
(bind! oo-workspace-map "k" #'tab-previous)
;;;;; emms
(defvar oo-emms-map (make-sparse-keymap))
(define-prefix-command 'oo-emms-prefix-command 'oo-emms-map)
(bind! oo-emms-map "p" #'emms-pause)
(bind! oo-emms-map "P" #'emms-stop)
(bind! oo-emms-map "r" #'emms-toggle-repeat-track)
(bind! oo-emms-map "R" #'emms-toggle-repeat-playlist)
(bind! oo-emms-map "v" #'emms-volume-lower)
(bind! oo-emms-map "V" #'emms-volume-raise)
(bind! oo-emms-map "s" #'emms-seek-to)
;;;;; package
(defvar oo-package-map (make-sparse-keymap))
(define-prefix-command 'oo-package-prefix-command 'oo-package-map)

(bind! oo-package-map "i" #'package-install)
(bind! oo-package-map "d" #'package-install)
;;;; helm
(bind! i helm-map "TAB" #'helm-next-line)
(bind! i helm-map [backtab] #'helm-previous-line)
(bind! i helm-map "C-j" #'helm-next-line)
(bind! i helm-map "C-k" #'helm-previous-line)
(bind! i helm-map "C-a" #'helm-select-action)
(bind! i helm-map "C-m" #'helm-toggle-visible-mark-forward)
(bind! i helm-map "RET" #'+helm-select-nth-action)
(bind! i helm-map "S-TAB" #'helm-mark-current-line)
(bind! i helm-map "C-;" #'ace-jump-helm-line)
;;;; corfu
(bind! i corfu-map "<tab>"   #'corfu-next)
(bind! i corfu-map [backtab] #'corfu-previous)
(bind! i corfu-map "S-TAB"   #'corfu-previous)
(bind! i corfu-map "C-;"     #'corfu-quick-complete)
(bind! i corfu-map "C-j"     #'corfu-next)
(bind! i corfu-map "C-k"     #'corfu-previous)
(bind! i corfu-map "C-p"     #'corfu-previous)
(bind! i corfu-map ";"       #'corfu-quick-complete)
(bind! i corfu-map "SPC"     #'corfu-insert)
;;;; vertico
(bind! i vertico-map "C-n" #'vertico-scroll-up)
(bind! i vertico-map "C-p" #'vertico-scroll-down)
(bind! i vertico-map "TAB" #'vertico-next)
(bind! i vertico-map "C-k" #'vertico-previous)
(bind! i vertico-map "C-j" #'vertico-next)
(bind! i vertico-map ";" #'vertico-quick-exit)
(bind! i vertico-map "C-;" #'vertico-quick-exit)
(bind! i vertico-map [backtab] #'vertico-previous)
(bind! i vertico-map "C-o" #'embark-act)
;;;; uncategorized
(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements oo-leader-map "l" "localleader"))

(bind! eww-mode-map n "R" #'eww-reload)
(bind! oo-find-map "t" #'tab-switch)
(bind! oo-app-map "E" #'restart-emacs-start-new-emacs)
(bind! oo-quick-map "j" #'grugru)
(bind! oo-buffer-map "x" #'kill-current-buffer)
(bind! n org-mode-map "T" #'org-todo)
(bind! n org-mode-map "t" #'+org-choose-tags)
(bind! oo-emms-map "f" #'emms-play-file)
(bind! oo-quick-map "i" #'tempel-insert)
(bind! oo-quick-map "l" #'tempel-insert)
(bind! evil-motion-state-map "o" #'evil-forward-WORD-begin)

(bind! oo-app-map "d" #'dired-jump)
(bind! (n m) dired-mode-map "h" #'dired-up-directory)
(bind! (n m) dired-mode-map "l" #'dired-find-file)
(bind! (n m) dired-mode-map "RET" #'dired-find-file)
(bind! i tempel-map "C-j" #'tempel-next)
(bind! i tempel-map "C-k" #'tempel-previous)
(bind! i tempel-map "TAB" #'tempel-next)
(bind! i tempel-map [backtab] #'tempel-previous)
(bind! "" #'lorem-ipsum-insert-sentences)
(bind! "" #'lorem-ipsum-insert-sentences)
(bind! "" #'lorem-ipsum-insert-sentences)
;;;; macrostep
(declare-function macrostep-expand "macrostep")
(declare-function macrostep-collapse-all "macrostep")
(declare-function macrostep-collapse "macrostep")

(oo-localleader-bind emacs-lisp-mode-map "me" #'macrostep-expand)
(oo-localleader-bind emacs-lisp-mode-map "mc" #'macrostep-collapse)
(oo-localleader-bind emacs-lisp-mode-map "mC" #'macrostep-collapse-all)
;;;; info
(bind! n Info-mode-map "H" #'Info-last)
(bind! n Info-mode-map "L" #'Info-next)
;;; provide
(provide 'oo-keybindings)
;;; oo-keybindings.el ends here
