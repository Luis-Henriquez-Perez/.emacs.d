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
(require '050-base)
(require 'bind-key)
(eval-when-compile (require '036-keybinding-macros))
(eval-when-compile (require '035-base-macros))
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

(defconst oo-normal-localleader-key "SPC m"
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

(defconst oo-insert-localleader-key "M-SPC m"
  "The localleader prefix key for major-mode specific commands.")

(defconst oo-insert-localleader-short-key "M-,"
  "A short non-normal `oo-localleader-key'.")

(defconst oo-emacs-leader-key "C-c l"
  "The leader prefix key used for Emacs states.")

(defconst oo-emacs-alt-leader-key "C-c SPC")

(defconst oo-emacs-localleader-key "C-c l m"
  "The localleader prefix key for major-mode specific commands.")
;;;; miscellaneous
(defun oo-dwim-escape ()
  "Exits out of whatever is happening after escape."
  (interactive)
  (when (bound-and-true-p evil-mode)
    (evil-normal-state 1))
  (cond ((minibuffer-window-active-p (minibuffer-window))
		 (if (or defining-kbd-macro executing-kbd-macro)
			 (minibuffer-keyboard-quit)
           (abort-recursive-edit)))
		((or defining-kbd-macro executing-kbd-macro) nil)
        (t
         (when (and (not buffer-read-only)
                    (buffer-file-name)
                    (buffer-modified-p))
           (save-buffer))
		 (keyboard-quit))))
;;;; base bindings
(nmap "+" #'text-scale-increase)
(nmap "-" #'text-scale-decrease)
(nmap "H" #'evil-first-non-blank)
(nmap "L" #'evil-last-non-blank)
(nmap "J" #'evil-scroll-page-down)
(nmap "K" #'evil-scroll-page-up)

(imap "A-x" #'execute-extended-command)
(imap "M-x" #'execute-extended-command)
(imap "C-c h" #'grugru)
(imap "C-c k" #'unexpand-abbrev)
(imap [escape] #'oo-dwim-escape)
(imap "TAB" #'completion-at-point)
(imap lispyville-mode-map "SPC" #'lispy-space)
(imap lispyville-mode-map ";" #'lispy-comment)

(vmap "V" #'expreg-contract)
(vmap "v" #'expreg-expand)

(nvmap ";" #'execute-extended-command)
;; (nvmap "w" #'+evilem-motion-beginning-of-word)
;; (nvmap "e" #'+evilem-motion-end-of-word)
;; (nvmap "W" #'+evilem-motion-beginning-of-WORD)
;; (nvmap "E" #'+evilem-motion-end-of-WORD)
;; (nvmap "f" #'+evilem-motion-char)
;; (nvmap "H" #'+evilem-motion-beginning-of-line)
;; (nvmap "H" #'+evilem-motion-beginning-of-line)
(nvmap "g b" #'+evil-eval-print-operator)
(nvmap "g p" #'+evil-eval-print-operator)
(nvmap "g c" #'evilnc-comment-operator)
(nvmap "g h" #'+evil-eval-operator)
(nvmap "g l" #'+evil-eval-replace-operator)
(nvmap "g r" #'+evil-eval-replace-operator)
(nvmap "g s" #'evil-exchange)
(nvmap "g S" #'evil-exchange-cancel)
(nvmap "g x" #'evil-exchange)
(nvmap "g X" #'evil-exchange-cancel)
;;;; text objects
;; (iotmap "c" #'evilnc-inner-comment #'evilnc-outer-comment)
;; TODO: In "lispy" modes use lispyville-outer-comment instead.
;; (iotmap "c" #'lispyville-inner-comment #'lispyville-outer-comment)
(iotmap "h" #'evil-i-syntax #'evil-a-syntax)
(iotmap "l" #'evil-inner-line #'evil-a-line)
(iotmap "f" #'evil-cp-inner-form #'evil-cp-a-form)
(iotmap "b" #'evil-textobj-anyblock-inner-block #'evil-textobj-anyblock-a-block)
;;;; leader bindings
;;;;; window
(defkeymap! oo-window-map
  "v" #'split-window-horizontally
  "h" #'split-window-vertically
  "b" #'balance-windows
  "M" #'maximize-window
  "d" #'delete-window
  "D" #'delete-other-windows
  "k" #'display-buffer
  "u" #'winner-undo
  "t" #'transpose-frame
  "s" #'ace-swap-window
  "w" #'ace-window
  "j" #'ace-window
  "o" #'ace-window
  "S" #'burly-bookmark-windows)
;;;;; git
(defkeymap! oo-git-map
  "p" #'magit-push
  "c" #'magit-commit
  "B" #'magit-branch
  "n" #'oo-dwim-vc-action
  "b" #'vc-switch-branch
  "l" #'vc-switch-branch
  "s" #'magit-status
  "g" #'magit-status)
;;;;; org
(defkeymap! oo-org-map
  :prefix 'oo-org-map
  "t" #'+org-capture-todo
  "j" #'+org-capture-todo
  "a" #'org-archive-subtree
  "l" #'org-clock-in-last
  "i" #'org-clock-in
  "k" #'org-clock-in
  "o" #'org-clock-out
  "s" #'org-add-note
  "n" #'org-add-note
  "p" #'+org-capture-plain)
;;;;; app
(defkeymap! oo-app-map
  "E" #'restart-emacs-start-new-emacs
  "d" #'dired-jump
  "j" #'+org-capture-todo
  "n" #'notmuch
  "e" #'eshell
  "f" #'elfeed
  "s r" #'escr-region-screenshot
  "s f" #'escr-frame-screenshot
  "s w" #'escr-window-screenshot)
;;;;; toggle
(defkeymap! oo-toggle-map
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
  "P" #'profiler-stop)
;;;;; buffer
(defkeymap! oo-buffer-map
  "x" #'kill-current-buffer
  "b" #'switch-to-buffer
  "j" #'next-buffer
  "k" #'previous-buffer)
;;;;; help
(defkeymap! oo-help-map
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
(defkeymap! oo-find-map
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
  "j" #'oo-dwim-narrow
  "n" #'oo-new-buffer
  "f" #'switch-to-buffer
  "a" #'find-library
  "d" #'pop-to-buffer)
;;;;; quit
(defkeymap! oo-quit-map
  "R" #'restart-emacs
  "E" #'restart-emacs-start-new-emacs
  "r" #'restart-emacs
  "q" #'save-buffers-kill-emacs)
;;;;; music
(defkeymap! oo-music-map
  "f" #'emms-play-file
  "p" #'emms-pause
  "P" #'emms-stop
  "r" #'emms-toggle-repeat-track
  "R" #'emms-toggle-repeat-playlist
  "v" #'emms-volume-lower
  "V" #'emms-volume-raise
  "s" #'emms-seek-to)
;;;;; package
(defkeymap! oo-package-map
  "i" #'package-install
  "d" #'package-install)
;;;;; quick map
(defkeymap! oo-quick-map
  "j" #'+org-capture-todo
  "g" #'grugru
  "i" #'tempel-insert
  "l" #'tempel-insert)
;;;;; leader map
(defkeymap! oo-leader-map
  "SPC" #'execute-extended-command
  ";" #'+org-agenda-day-view
  "j" '("quick" . oo-quick-map)
  "w" '("window" . oo-window-map)
  "b" '("buffer" . oo-buffer-map)
  "g" '("git" . oo-git-map)
  "l" '("git" . oo-git-map)
  "a" '("app" . oo-app-map)
  "p" '("package" . oo-package-map)
  "f" '("find" . oo-find-map)
  "h" '("help" . oo-help-map)
  "e" '("music" . oo-music-map)
  "t" '("toggle" . oo-toggle-map)
  "q" '("quit" . oo-quit-map))

(oo-add-hook 'emacs-startup-hook #'override-global-mode)

(nmap override-global-map oo-normal-leader-key #'oo-leader-map)
(imap override-global-map oo-insert-leader-key #'oo-leader-map)
(emap override-global-map oo-emacs-leader-key #'oo-leader-map)
(emap override-global-map oo-emacs-alt-leader-key #'oo-leader-map)
;;;; helm
(imap helm-map "TAB" #'helm-next-line)
(imap helm-map [backtab] #'helm-previous-line)
(imap helm-map "C-j" #'helm-next-line)
(imap helm-map "C-k" #'helm-previous-line)
(imap helm-map "C-a" #'helm-select-action)
(imap helm-map "C-m" #'helm-toggle-visible-mark-forward)
(imap helm-map "RET" #'+helm-select-nth-action)
(imap helm-map "S-TAB" #'helm-mark-current-line)
(imap helm-map "C-;" #'ace-jump-helm-line)
;;;; corfu
(imap corfu-map "<tab>"   #'corfu-next)
(imap corfu-map [backtab] #'corfu-previous)
(imap corfu-map "S-TAB"   #'corfu-previous)
(imap corfu-map "C-;"     #'corfu-quick-complete)
(imap corfu-map "C-j"     #'corfu-next)
(imap corfu-map "C-k"     #'corfu-previous)
(imap corfu-map "C-p"     #'corfu-previous)
(imap corfu-map ";"       #'corfu-quick-complete)
(imap corfu-map "SPC"     #'corfu-insert)
;;;; vertico
(imap vertico-map "C-n" #'vertico-scroll-up)
(imap vertico-map "C-p" #'vertico-scroll-down)
(imap vertico-map "TAB" #'vertico-next)
(imap vertico-map "C-k" #'vertico-previous)
(imap vertico-map "C-j" #'vertico-next)
(imap vertico-map ";" #'vertico-quick-exit)
(imap vertico-map "C-;" #'vertico-quick-exit)
(imap vertico-map [backtab] #'vertico-previous)
(imap vertico-map "C-o" #'embark-act)
;;;; uncategorized
(declare-function which-key-add-keymap-based-replacements "which-key")
(afterfeature! which-key
  (which-key-add-keymap-based-replacements oo-leader-map "m" "localleader"))

(nmap eww-mode-map "R" #'eww-reload)

(nmap org-mode-map "T" #'org-todo)
(nmap org-mode-map "t" #'+org-choose-tags)

;; (keymap-set evil-motion-state-map "o" #'evil-forward-WORD-begin)
(nmap dired-mode-map "h" #'dired-up-directory)
(nmap dired-mode-map "l" #'dired-find-file)
(nmap dired-mode-map "RET" #'dired-find-file)

(imap tempel-map "C-l" #'tempel-abort)
(imap tempel-map "C-j" #'tempel-next)
(imap tempel-map "C-k" #'tempel-previous)
(imap tempel-map "TAB" #'tempel-next)
(imap tempel-map [backtab] #'tempel-previous)
;;;; macrostep
(declare-function macrostep-expand "macrostep")
(declare-function macrostep-collapse-all "macrostep")
(declare-function macrostep-collapse "macrostep")

(llmap emacs-lisp-mode-map "m" '("macrostep" . oo-macrostep-map))
(llmap emacs-lisp-mode-map "e" #'macrostep-expand)
(llmap emacs-lisp-mode-map "c" #'macrostep-collapse)
(llmap emacs-lisp-mode-map "C" #'macrostep-collapse-all)
(llmap emacs-lisp-mode-map "a" #'macrostep-collapse-all)

(defkeymap! oo-macrostep-map
  "e" #'macrostep-expand
  "c" #'macrostep-collapse
  "C" #'macrostep-collapse-all
  "a" #'macrostep-collapse-all)
;;;; info
(nmap Info-mode-map "H" #'Info-last)
(nmap Info-mode-map "L" #'Info-next)
;;; provide
(provide '160-keybindings)
;;; 160-keybindings.el ends here
