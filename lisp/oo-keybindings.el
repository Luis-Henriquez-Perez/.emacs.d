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
;;;; requirements
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
(oo-bind i "A-x" #'execute-extended-command)
(oo-bind i "M-x" #'execute-extended-command)
(oo-bind i "C-c h" #'grugru)
(oo-bind i "C-c k" #'unexpand-abbrev)
(oo-bind i [escape] #'oo-dwim-escape)

(oo-bind (n v) "w" #'+evilem-motion-beginning-of-word)
(oo-bind (n v) "e" #'+evilem-motion-end-of-word)
(oo-bind (n v) "W" #'+evilem-motion-beginning-of-WORD)
(oo-bind (n v) "E" #'+evilem-motion-end-of-WORD)
(oo-bind (n v o) "f" #'+evilem-motion-char)
(oo-bind (n v o) "H" #'+evilem-motion-beginning-of-line)
(oo-bind (n v o) "H" #'+evilem-motion-beginning-of-line)
(oo-bind v "V" #'expreg-contract)
(oo-bind v "v" #'expreg-expand)

(oo-bind n "+" #'text-scale-increase)
(oo-bind n "-" #'text-scale-decrease)
(oo-bind n "H" #'evil-first-non-blank)
(oo-bind n "L" #'evil-last-non-blank)
(oo-bind n "J" #'evil-scroll-page-down)
(oo-bind n "K" #'evil-scroll-page-up)

(oo-bind i lispyville-mode-map "SPC" #'lispy-space)
(oo-bind i lispyville-mode-map ";" #'lispy-comment)

(oo-bind (n v) "g u" #'evil-upcase)
(oo-bind (n v) "g U" #'evil-downcase)
(oo-bind (n v) "g t" #'evil-goto-first-line)
(oo-bind (n v) "g b" #'evil-goto-line)
(oo-bind (n v) "g h" #'+evil-eval-operator)
(oo-bind (n v) "g r" #'+evil-eval-replace-operator)
(oo-bind (n v) "g l" #'+evil-eval-print-operator)
(oo-bind (n v) "g p" #'+evil-eval-print-operator)
(oo-bind (n v) "g x" #'evil-exchange)
(oo-bind (n v) "g X" #'evil-exchange-cancel)
(oo-bind (n v) "g a" #'evil-exchange)
(oo-bind (n v) "g A" #'evil-exchange-cancel)
(oo-bind (n v) "g c" #'lispyville-comment-or-uncomment)
(oo-bind (n v) "g l" #'lispyville-comment-and-clone-dwim)
;;;; text objects
(oo-bind evil-outer-text-objects-map "c" #'lispyville-outer-comment)
(oo-bind evil-inner-text-objects-map "c" #'lispyville-inner-comment)
(oo-bind evil-outer-text-objects-map "h" #'evil-a-syntax)
(oo-bind evil-inner-text-objects-map "h" #'evil-i-syntax)
(oo-bind evil-inner-text-objects-map "l" #'evil-inner-line)
(oo-bind evil-outer-text-objects-map "l" #'evil-a-line)
(oo-bind evil-inner-text-objects-map "f" #'evil-cp-inner-form)
(oo-bind evil-outer-text-objects-map "f" #'evil-cp-a-form)
(oo-bind evil-inner-text-objects-map "b" #'evil-textobj-anyblock-inner-block)
(oo-bind evil-outer-text-objects-map "b" #'evil-textobj-anyblock-a-block)
;;;; main bindings
;;;;; leader prefix
(oo-bind i override-global-map oo-insert-leader-key #'oo-leader-prefix-command)
(oo-bind override-global-map oo-emacs-leader-key  #'oo-leader-prefix-command)
(oo-bind override-global-map oo-emacs-alt-leader-key  #'oo-leader-prefix-command)
(oo-bind (n m v) override-global-map oo-normal-leader-key #'oo-leader-prefix-command)
(oo-bind (n m v) override-global-map ";" #'execute-extended-command)

(defvar oo-leader-map (make-sparse-keymap))
(define-prefix-command 'oo-leader-prefix-command 'oo-leader-map)

(oo-bind oo-leader-map ";" #'+org-agenda-day-view)
(oo-bind oo-leader-map oo-normal-leader-key #'execute-extended-command)
(oo-bind oo-leader-map "b" #'oo-buffer-prefix-command :wk "buffer")
(oo-bind oo-leader-map "g" #'oo-git-prefix-command :wk "git")
(oo-bind oo-leader-map "l" #'oo-git-prefix-command :wk "git")
(oo-bind oo-leader-map "w" #'oo-window-prefix-command :wk "window")
(oo-bind oo-leader-map "a" #'oo-app-prefix-command :wk "app")
(oo-bind oo-leader-map "p" #'oo-package-prefix-command :wk "package")
(oo-bind oo-leader-map "f" #'oo-find-prefix-command :wk "find")
(oo-bind oo-leader-map "h" #'oo-help-prefix-command :wk "help")
(oo-bind oo-leader-map "e" #'oo-emms-prefix-command :wk "emms")
(oo-bind oo-leader-map "t" #'oo-toggle-prefix-command :wk "toggle")
(oo-bind oo-leader-map "q" #'oo-quit-prefix-command :wk "quit")
;;;;; window
(defvar oo-window-map (make-sparse-keymap))
(define-prefix-command 'oo-window-prefix-command 'oo-window-map)

(oo-bind oo-window-map "v" #'split-window-horizontally)
(oo-bind oo-window-map "h" #'split-window-vertically)
(oo-bind oo-window-map "b" #'balance-windows)
(oo-bind oo-window-map "M" #'maximize-window)
(oo-bind oo-window-map "d" #'delete-window)
(oo-bind oo-window-map "D" #'delete-other-windows)
(oo-bind oo-window-map "k" #'display-buffer)
(oo-bind oo-window-map "u" #'winner-undo)
(oo-bind oo-window-map "t" #'transpose-frame)
(oo-bind oo-window-map "s" #'ace-swap-window)
(oo-bind oo-window-map "w" #'ace-window)
(oo-bind oo-window-map "j" #'ace-window)
(oo-bind oo-window-map "o" #'ace-window)
(oo-bind oo-window-map "S" #'burly-bookmark-windows)
(oo-bind oo-window-map "b" #'burly-bookmark-windows)
;;;;; git
(defvar oo-git-map (make-sparse-keymap))
(define-prefix-command 'oo-git-prefix-command 'oo-git-map)

(oo-bind oo-git-map "b" #'vc-switch-branch)
(oo-bind oo-git-map "l" #'vc-switch-branch)
(oo-bind oo-git-map "s" #'magit-status)
(oo-bind oo-git-map "g" #'magit-status)
(oo-bind oo-git-map "p" #'magit-push)
(oo-bind oo-git-map "c" #'magit-commit)
(oo-bind oo-git-map "B" #'magit-branch)
(oo-bind oo-git-map "n" #'oo-dwim-vc-action)
;;;;; app
(defvar oo-app-map (make-sparse-keymap))
(define-prefix-command 'oo-app-prefix-command 'oo-app-map)

(oo-bind oo-app-map "n" #'notmuch)
(oo-bind oo-app-map "e" #'eshell)
(oo-bind oo-app-map "c" #'org-capture)
(oo-bind oo-app-map "a c" #'org-capture)
(oo-bind oo-app-map "a a" #'+org-capture-plain)
(oo-bind oo-app-map "a p" #'+org-capture-plain)
(oo-bind oo-app-map "a s" #'+org-capture-todo)
(oo-bind oo-app-map "a j" #'+org-capture-todo)
(oo-bind oo-app-map "a t" #'+org-capture-todo)
(oo-bind oo-app-map "a k" #'+org-capture-bug)
(oo-bind oo-app-map "a l" #'+org-capture-open)
(oo-bind oo-app-map "a o" #'+org-capture-open)
(oo-bind oo-app-map "a ;" #'+org-capture-question)
(oo-bind oo-app-map "a q" #'+org-capture-question)
;;;;; toggle
(defvar oo-toggle-map (make-sparse-keymap)
  "Keymap that contains bindings for things that should be toggled.")
(define-prefix-command 'oo-toggle-prefix-command 'oo-toggle-map)

(oo-bind oo-toggle-map "c" #'blink-cursor-mode)
(oo-bind oo-toggle-map "g" #'grugru)
(oo-bind oo-toggle-map "s" #'smartparens-mode)
(oo-bind oo-toggle-map "r" #'oo-load-random-theme)
(oo-bind oo-toggle-map "t" #'load-theme)
(oo-bind oo-toggle-map "w" #'whitespace-mode)
(oo-bind oo-toggle-map "l" #'display-line-numbers-mode)
(oo-bind oo-toggle-map "u" #'toggle-truncate-lines)
(oo-bind oo-toggle-map "n" #'oo-dwim-narrow)
(oo-bind oo-toggle-map "i" #'iedit-mode)
(oo-bind oo-toggle-map "e" #'eval-expression)
(oo-bind oo-toggle-map "f" #'oo-set-font-face)
(oo-bind oo-toggle-map "r" #'read-only-mode)
(oo-bind oo-toggle-map "d" #'toggle-debug-on-error)
(oo-bind oo-toggle-map "P" #'profiler-stop)
;;;;; buffer
(defvar oo-buffer-map (make-sparse-keymap))
(define-prefix-command 'oo-buffer-prefix-command 'oo-buffer-map)

(oo-bind oo-buffer-map "x" #'kill-current-buffer)
(oo-bind oo-buffer-map "b" #'switch-to-buffer)
(oo-bind oo-buffer-map "j" #'next-buffer)
(oo-bind oo-buffer-map "k" #'previous-buffer)
(oo-bind oo-buffer-map "b" #'switch-to-buffer)
;;;;; help
(defvar oo-help-map (make-sparse-keymap))
(define-prefix-command 'oo-help-prefix-command 'oo-help-map)

(oo-bind oo-help-map "m" #'describe-mode)
(oo-bind oo-help-map "l" #'describe-function)
(oo-bind oo-help-map "f" #'describe-function)
(oo-bind oo-help-map "j" #'describe-variable)
(oo-bind oo-help-map "v" #'describe-variable)
(oo-bind oo-help-map "h" #'describe-variable)
(oo-bind oo-help-map "C" #'describe-char)
(oo-bind oo-help-map "k" #'describe-key)
;;;;; find
(defvar oo-find-map (make-sparse-keymap))
(define-prefix-command 'oo-find-prefix-command 'oo-find-map)

(oo-bind oo-find-map ";" #'save-buffer)
(oo-bind oo-find-map "o" #'find-file)
(oo-bind oo-find-map "E" #'oo-open-emacs-config)
(oo-bind oo-find-map "I" #'oo-open-emacs-init-file)
(oo-bind oo-find-map "L" #'oo-open-emacs-lisp-dir)
(oo-bind oo-find-map "G" #'rgrep)
(oo-bind oo-find-map "p" #'consult-yank-pop)
(oo-bind oo-find-map "k" #'consult-bookmark)
(oo-bind oo-find-map "b" #'consult-bookmark)
(oo-bind oo-find-map "l" #'consult-line)
(oo-bind oo-find-map "h" #'consult-outline)
(oo-bind oo-find-map "g" #'consult-grep)
(oo-bind oo-find-map "z" #'ace-link)
(oo-bind oo-find-map "b" #'burly-open-bookmark)
(oo-bind oo-find-map ";" #'save-buffer)
(oo-bind oo-find-map "i" #'imenu)
(oo-bind oo-find-map "j" #'oo-dwim-narrow)
(oo-bind oo-find-map "n" #'oo-new-buffer)
(oo-bind oo-find-map "o" #'find-file)
(oo-bind oo-find-map "f" #'switch-to-buffer)
(oo-bind oo-find-map "d" #'display-buffer)
(oo-bind oo-find-map "a" #'find-library)
(oo-bind oo-find-map "f" #'switch-to-buffer)
(oo-bind oo-find-map "d" #'pop-to-buffer)
;;;;; quit
(defvar oo-quit-map (make-sparse-keymap))
(define-prefix-command 'oo-quit-prefix-command 'oo-quit-map)

(oo-bind oo-quit-map "R" #'restart-emacs)
(oo-bind oo-quit-map "E" #'restart-emacs-start-new-emacs)
(oo-bind oo-quit-map "r" #'restart-emacs)
(oo-bind oo-quit-map "q" #'save-buffers-kill-emacs)
;;;;; workspace
(oo-bind oo-workspace-map "t" #'tab-select)
(oo-bind oo-workspace-map "n" #'tab-new)
(oo-bind oo-workspace-map "j" #'tab-next)
(oo-bind oo-workspace-map "k" #'tab-previous)
;;;;; emms
(defvar oo-emms-map (make-sparse-keymap))
(define-prefix-command 'oo-emms-prefix-command 'oo-emms-map)
(oo-bind oo-emms-map "p" #'emms-pause)
(oo-bind oo-emms-map "P" #'emms-stop)
(oo-bind oo-emms-map "r" #'emms-toggle-repeat-track)
(oo-bind oo-emms-map "R" #'emms-toggle-repeat-playlist)
(oo-bind oo-emms-map "v" #'emms-volume-lower)
(oo-bind oo-emms-map "V" #'emms-volume-raise)
(oo-bind oo-emms-map "s" #'emms-seek-to)
;;;;; package
(defvar oo-package-map (make-sparse-keymap))
(define-prefix-command 'oo/package-prefix-command 'oo-package-map)

(oo-bind oo-package-map "i" #'package-install)
(oo-bind oo-package-map "d" #'package-install)
;;;; helm
(oo-bind i helm-map "TAB" #'helm-next-line)
(oo-bind i helm-map [backtab] #'helm-previous-line)
(oo-bind i helm-map "C-j" #'helm-next-line)
(oo-bind i helm-map "C-k" #'helm-previous-line)
(oo-bind i helm-map "C-a" #'helm-select-action)
(oo-bind i helm-map "C-m" #'helm-toggle-visible-mark-forward)
(oo-bind i helm-map "RET" #'+helm-select-nth-action)
(oo-bind i helm-map "S-TAB" #'helm-mark-current-line)
(oo-bind i helm-map "C-;" #'ace-jump-helm-line)
;;;; corfu
(oo-bind i corfu-map "<tab>"   #'corfu-next)
(oo-bind i corfu-map [backtab] #'corfu-previous)
(oo-bind i corfu-map "S-TAB"   #'corfu-previous)
(oo-bind i corfu-map "C-;"     #'corfu-quick-complete)
(oo-bind i corfu-map "C-j"     #'corfu-next)
(oo-bind i corfu-map "C-k"     #'corfu-previous)
(oo-bind i corfu-map "C-p"     #'corfu-previous)
(oo-bind i corfu-map ";"       #'corfu-quick-complete)
(oo-bind i corfu-map "SPC"     #'corfu-insert)
;;;; vertico
(oo-bind i vertico-map "TAB" #'vertico-next)
(oo-bind i vertico-map "C-k" #'vertico-previous)
(oo-bind i vertico-map "C-j" #'vertico-next)
(oo-bind i vertico-map ";" #'vertico-quick-exit)
(oo-bind i vertico-map "C-;" #'vertico-quick-exit)
(oo-bind i vertico-map [backtab] #'vertico-previous)
(oo-bind i vertico-map "C-o" #'embark-act)
;;;; uncategorized
(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements oo-leader-map "l" "localleader"))

(oo-bind eww-mode-map n "R" #'eww-reload)
(oo-bind oo-find-map "t" #'tab-switch)
(oo-bind oo-app-map "E" #'restart-emacs-start-new-emacs)
(oo-bind oo-quick-map "j" #'grugru)
(oo-bind oo-buffer-map "x" #'kill-current-buffer)
(oo-bind n org-mode-map "T" #'org-todo)
(oo-bind n org-mode-map "t" #'+org-choose-tags)
(oo-bind oo-emms-map "f" #'emms-play-file)
(oo-bind oo-quick-map "i" #'tempel-insert)
(oo-bind oo-quick-map "l" #'tempel-insert)
(oo-bind evil-motion-state-map "o" #'evil-forward-WORD-begin)

(oo-bind oo-app-map "d" #'dired-jump)
(oo-bind (n m) dired-mode-map "h" #'dired-up-directory)
(oo-bind (n m) dired-mode-map "l" #'dired-find-file)
(oo-bind (n m) dired-mode-map "RET" #'dired-find-file)
(oo-bind i tempel-map "C-j" #'tempel-next)
(oo-bind i tempel-map "C-k" #'tempel-previous)
(oo-bind i tempel-map "TAB" #'tempel-next)
(oo-bind i tempel-map [backtab] #'tempel-previous)
(oo-bind "" #'lorem-ipsum-insert-sentences)
(oo-bind "" #'lorem-ipsum-insert-sentences)
(oo-bind "" #'lorem-ipsum-insert-sentences)
;;;; macrostep
(declare-function macrostep-expand "macrostep")
(declare-function macrostep-collapse-all "macrostep")
(declare-function macrostep-collapse "macrostep")

(oo-localleader-bind emacs-lisp-mode-map "me" #'macrostep-expand)
(oo-localleader-bind emacs-lisp-mode-map "mc" #'macrostep-collapse)
(oo-localleader-bind emacs-lisp-mode-map "mC" #'macrostep-collapse-all)
;;;; information
;; (define-key )
(oo-bind n Info-mode-map "H" #'Info-last)
(oo-bind n Info-mode-map "L" #'Info-next)
;; (oo-bind Info-mode-map "H" #'Info-backward-node)
;; (oo-bind Info-mode-map "H" #'Info-backward-node)
;;; provide
(provide 'oo-keybindings)
;;; oo-keybindings.el ends here
