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
(eval-when-compile (require '037-keybinding-macros))
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
(declare-function minibuffer-keyboard-quit "delsel")
(declare-function evil-normal-state "evil")
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
(nmap [escape] #'oo-dwim-escape)

(imap "A-x" #'execute-extended-command)
(imap "M-x" #'execute-extended-command)
(imap "C-c h" #'grugru)
(nimap "C-c k" #'unexpand-abbrev)
(imap [escape] #'oo-dwim-escape)
(imap "TAB" #'completion-at-point)
(imap lispyville-mode-map "SPC" #'lispy-space)
(imap lispyville-mode-map ";" #'lispy-comment)

(vmap "V" #'expreg-contract)
(vmap "v" #'expreg-expand)

;; Ensure that ";" is always available as `execute-extended-command'.  Modes
;; like dired bind it themselves and would otherwise override it.
(nvmap override-global-map ";" #'execute-extended-command)
;; The problem is I feel like the default evil motions are not that useful
;; beyond moving to one forward unit.  So I have made the controversial decision
;; to rebind.  TODO: ke
(nvmap "w" #'+evilem-motion-beginning-of-word)
(nvmap "e" #'+evilem-motion-end-of-word)
(nvmap "W" #'+evilem-motion-beginning-of-WORD)
(nvmap "E" #'+evilem-motion-end-of-WORD)
(nvmap "f" #'+evilem-motion-char)
(nvmap "H" #'+evilem-motion-beginning-of-line)

(nvmap "g b" #'+evil-eval-print-operator)
(nvmap "g p" #'+evil-eval-print-operator)
(nvmap "g c" #'evilnc-comment-operator)
(each! '(cider-repl-mode-map clojure-mode-map clojurec-mode-map clojurescript-mode-map clojurex-mode-map clojure-ts-mode-map clojurescript-ts-mode-map clojurec-ts-mode-map common-lisp-mode-map emacs-lisp-mode-map eshell-mode-map fennel-mode-map fennel-repl-mode-map geiser-repl-mode-map gerbil-mode-map inf-clojure-mode-map inferior-emacs-lisp-mode-map inferior-lisp-mode-map inferior-scheme-mode-map lisp-interaction-mode-map lisp-mode-map monroe-mode-map racket-mode-map racket-repl-mode-map scheme-interaction-mode-map scheme-mode-map slime-repl-mode-map sly-mrepl-mode-map stumpwm-mode-map)
  (oo-bind-key it "g c" #'lispyville-comment-or-uncomment '(normal visual)))
(nvmap emacs-lisp-mode-map [remap evilnc-comment-operator] #'lispyville-comment-or-uncomment)
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
(iotmap "a" #'lispyville-inner-comment #'lispyville-outer-comment)
(iotmap "h" #'evil-i-syntax #'evil-a-syntax)
(iotmap "l" #'evil-inner-line #'evil-a-line)
(iotmap "f" #'evil-cp-inner-form #'evil-cp-a-form)
(iotmap "b" #'evil-textobj-anyblock-inner-block #'evil-textobj-anyblock-a-block)

;; (iotmap "a" #'evilnc-inner-commenter #'evilnc-outer-commenter)
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
  "j" #'oo-dwim-vc-action
  "l" #'vc-switch-branch
  "n" #'oo-dwim-vc-action
  "p" #'magit-push
  "r" #'vc-register
  "s" #'magit-status)
;;;;; org
(defvar-keymap! oo-org-map
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
(defvar-keymap! oo-screenshot-map
  "r" #'escr-region-screenshot
  "f" #'escr-frame-screenshot
  "w" #'escr-window-screenshot)

(defvar-keymap! oo-app-map
  "E" #'restart-emacs-start-new-emacs
  "d" #'dired-jump
  "j" #'+org-capture-todo
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
  "j" #'oo-dwim-vc-action
  "n" #'oo-new-buffer
  "f" #'switch-to-buffer
  "a" #'find-library
  "d" #'pop-to-buffer)
;;;;; quit
(defvar-keymap! oo-quit-map
  "R" #'restart-emacs
  "E" #'restart-emacs-start-new-emacs
  "r" #'restart-emacs
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
  "j" #'+org-capture-todo
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

(nmap override-global-map oo-normal-leader-key #'oo-leader-map)
(imap override-global-map oo-insert-leader-key #'oo-leader-map)
(emap override-global-map oo-emacs-leader-key #'oo-leader-map)
(emap override-global-map oo-emacs-alt-leader-key #'oo-leader-map)
;;;; uncategorized
(declare-function which-key-add-keymap-based-replacements "which-key")
(afterfeature! which-key
  (which-key-add-keymap-based-replacements oo-leader-map "m" "localleader"))

(nmap eww-mode-map "R" #'eww-reload)

;; (keymap-set evil-motion-state-map "o" #'evil-forward-WORD-begin)
;;;; macrostep
(declare-function macrostep-expand "macrostep")
(declare-function macrostep-collapse-all "macrostep")
(declare-function macrostep-collapse "macrostep")

(llmap emacs-lisp-mode-map "m" '("macrostep" . oo-macrostep-map))
(llmap emacs-lisp-mode-map "e" #'macrostep-expand)
(llmap emacs-lisp-mode-map "c" #'macrostep-collapse)
(llmap emacs-lisp-mode-map "C" #'macrostep-collapse-all)
(llmap emacs-lisp-mode-map "a" #'macrostep-collapse-all)

(llmap org-mode-map "a" #'org-archive-subtree)
(llmap org-mode-map "n" #'org-add-note)
(llmap org-mode-map "t" #'org-todo)

(defvar-keymap! oo-macrostep-map
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
