;;; functions-2.el --- external package library -*- lexical-binding: t; -*-
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
;; Compared to `030-base-functions' this library has functions that on external
;; packages loaded in `base-requirements'.

;; This file contains functions and macros directly used for customizing Emacs
;; by which I mean for doing things like adding hooks, adding advices, and
;; setting variables for a particular feature.  Basically tools for configuring
;; packages and features.
;;
;;; Code:
(require 'macros-base)
(require 'macros-autolet)
(require 'macros-loop)
(require 'functions-call-after)

(defvar evil-state-properties)
(declare-function evil-define-key* "evil")

(defun o-call-quietly-a (fn &rest args)
  "Call FN with ARGS without producing any output."
  (o-quiet (apply fn args)))

;; https://stackoverflow.com/questions/1609oo17/elisp-conditionally-change-keybinding
(defvar o-alternate-commands (make-hash-table)
  "A hash-table mapping command symbols to a list of command symbols.")

(o-defun o-alternate-command-choose-fn (command)
  "Return an alternate command that should be called instead of COMMAND."
  (or (o-each (gethash command o-alternate-commands)
        (o-aand (funcall it) (o-return it)))
      command))

(defmacro o-alt (old new feature)
  `(progn (push (lambda (&rest _) (when (or (featurep ',feature) (require ',feature nil t)) ',new))
                (gethash ',old o-alternate-commands))
          (define-key global-map [remap ,old] '(menu-item "" ,old :filter o-alternate-command-choose-fn))))

;; The point of this function is to give me a uniform interface for binding keys
;; where I do not have to worry about whether the keymap is defined or whether
;; evil is loaded.  Furthermore by having a function I can apply a change from
;; one to all bindings.
(o-defun o-bind-key (keymap key def &optional states)
  "Bind KEY to DEF in KEYMAP.
KEYMAP is a keymap symbol."
  (o-set states (ensure-list states))
  (when (or (not states) (o-aremf states (and (equal it 'global))))
    (o-call-after-bound keymap `(lambda () (keymap-set ,keymap ,key ',def))))
  (when states
    (setq key (if (vectorp key) key (kbd key)))
    (o-set fn `(lambda () (evil-define-key* ',states ,keymap ,key ',def)))
    (o-call-after-load 'evil (apply-partially #'o-call-after-bound keymap fn)))
  nil)

(o-defun o-apply-local-vars (hook)
  "Apply local variables for hook."
  (o-set failmsg "Failed to set local variable %s: %S ->%S")
  (o-for ((symbol . value) (alist-get hook o-local-var-alist))
    (o-set bodyform `(setq-local ,symbol ,value))
    (o-set handlerbody `(o-log 'failure ,failmsg ',symbol (car err) (cdr err)))
    (o-pushing forms `(condition-case err ,bodyform (error ,handlerbody))))
  (eval (macroexp-progn (nreverse forms)) t))
;;; provide
(provide 'functions-2)
;;; functions-2.el ends here
