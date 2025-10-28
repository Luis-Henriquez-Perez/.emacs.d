;;; 130-init-evil.el --- initialize evil -*- lexical-binding: t; -*-
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
;; Initialize evil.
;;
;;; Code:
(require! "^0[01]")
;;;; settings
;; Must be set before evil is loaded.  This, therefore, cannot be deferred with
;; `opt!'.  If this is not set evil with add opinionated bindings to certain
;; programs like dired which will override my own.
(defvar evil-want-keybinding)
(setq evil-want-keybinding nil)

(defun oo-load-evil-h ()
  "Require `evil'."
  (require 'evil nil t))

(add-hook 'after-init-hook #'oo-load-evil-h -90)

(add-hook 'emacs-startup-hook #'evil-mode)

;; To ensure that =oo-override-mode-map= takes priority over evil states, we need
;; to make it an intercept map for all evil states.  In evil, intercept maps are
;; maps that take priority (intercept) evil bindings when they have a different
;; binding for the same key (this is opposed to =overriding-maps=, which completely
;; override an evil keymap).
(defvar override-global-map)
(declare-function evil-make-intercept-map "evil")
(defun oo-make-intercept-map-h ()
  "Register `oo-override-map' as an intercept map."
  (require 'bind-key)
  (evil-make-intercept-map override-global-map 'all t))

(add-hook 'evil-mode-hook #'oo-make-intercept-map-h)

(oo-require-after-load 'evil '990-config-evil)

(nmap! "+" #'text-scale-increase)
(nmap! "-" #'text-scale-decrease)
(nmap! "H" #'evil-first-non-blank)
(nmap! "L" #'evil-last-non-blank)
(nmap! "J" #'evil-scroll-page-down)
(nmap! "K" #'evil-scroll-page-up)
(nmap! [escape] #'oo-evil-dwim-escape)
(nmap! "ff" #'evil-find-char)
(nmap! "fj" #'oo-evilem-motion-char)

(imap! "A-x" #'execute-extended-command)
(imap! "M-x" #'execute-extended-command)
(imap! "C-c h" #'grugru)
(imap! [escape] #'oo-evil-dwim-escape)
(imap! "TAB" #'completion-preview-insert)

(nimap! "C-c j" #'abbrev/inverse-add)
(nimap! "C-c k" #'unexpand-abbrev)

(vmap! "V" #'expreg-contract)
(vmap! "v" #'expreg-expand)
;; Ensure that ";" is always available as `execute-extended-command'.  Modes
;; like dired bind it themselves and would otherwise override it.
(nvmap! override-global-map ";" #'execute-extended-command)
;; The problem is I feel like the default evil motions are not that useful
;; beyond moving to one forward unit.  So I have made the controversial decision
;; to rebind.

;; (nvmap! "w" #'oo-evilem-motion-beginning-of-word)
;; (nvmap! "e" #'oo-evilem-motion-end-of-word)
;; (nvmap! "W" #'oo-evilem-motion-beginning-of-WORD)
;; (nvmap! "E" #'oo-evilem-motion-end-of-WORD)
;; (nvmap! "f" #'oo-evilem-motion-char)
;; (nvmap! "H" #'oo-evilem-motion-beginning-of-line)

(nvmap! "g b" #'oo-evil-eval-print-operator)
(nvmap! "g p" #'oo-evil-eval-print-operator)
(nvmap! "g c" #'evilnc-comment-operator)
(each! '(cider-repl-mode-map clojure-mode-map clojurec-mode-map clojurescript-mode-map clojurex-mode-map clojure-ts-mode-map clojurescript-ts-mode-map clojurec-ts-mode-map common-lisp-mode-map emacs-lisp-mode-map eshell-mode-map fennel-mode-map fennel-repl-mode-map geiser-repl-mode-map gerbil-mode-map inf-clojure-mode-map inferior-emacs-lisp-mode-map inferior-lisp-mode-map inferior-scheme-mode-map lisp-interaction-mode-map lisp-mode-map monroe-mode-map racket-mode-map racket-repl-mode-map scheme-interaction-mode-map scheme-mode-map slime-repl-mode-map sly-mrepl-mode-map stumpwm-mode-map)
  (oo-bind-key it "g c" #'lispyville-comment-or-uncomment '(normal visual)))
(nvmap! emacs-lisp-mode-map [remap evilnc-comment-operator] #'lispyville-comment-or-uncomment)
(nvmap! "g e" #'oo-evil-eval-operator)
(nvmap! "g h" #'oo-evil-eval-operator)
(nvmap! "g l" #'oo-evil-eval-replace-operator)
(nvmap! "g r" #'oo-evil-eval-replace-operator)
(nvmap! "g s" #'evil-exchange)
(nvmap! "g S" #'evil-exchange-cancel)
(nvmap! "g x" #'evil-exchange)
(nvmap! "g X" #'evil-exchange-cancel)
;;;; TEXT-OBJECTS
(autoload 'evilnc-inner-comment "evil-nerd-commenter" nil nil 'function)
(autoload 'evilnc-outer-comment "evil-nerd-commenter" nil nil 'function)

(iotmap! "c" #'evilnc-inner-comment #'evilnc-outer-comment)
;; TODO: In "lispy" modes use lispyville-outer-comment instead.
(iotmap! "a" #'lispyville-inner-comment #'lispyville-outer-comment)
(iotmap! "h" #'evil-i-syntax #'evil-a-syntax)
(iotmap! "l" #'evil-inner-line #'evil-a-line)
(iotmap! "f" #'evil-cp-inner-form #'evil-cp-a-form)
;; (iotmap! "b" #'evil-textobj-anyblock-inner-block #'evil-textobj-anyblock-a-block)
(iotmap! "b" #'oo-evil-inner-buffer #'oo-evil-outer-buffer)
;;; provide
(provide '130-init-evil)
;;; 130-init-evil.el ends here
