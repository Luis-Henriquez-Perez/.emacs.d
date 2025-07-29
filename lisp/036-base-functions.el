;;; 036-base-functions.el --- external package library -*- lexical-binding: t; -*-
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
(require '032-after-load-functions)
(eval-when-compile (require '035-base-macros))

(defvar evil-state-properties)
(declare-function evil-define-key* "evil")

(defun oo-call-quietly-a (fn &rest args)
  "Call FN with ARGS without producing any output."
  (quiet! (apply fn args)))

;; I don't yet know where to put this function.  So for now, here it goes.
(defun oo-popup-at-bottom (regexp)
  "Open buffers at bottom that match regexp."
  (alet! `(,regexp
           (display-buffer-at-bottom)
           (side bottom)
           (slot 1)
           (window-height 0.5)
           (window-parameters ((no-other-window t))))
    (push it display-buffer-alist)))

;; https://stackoverflow.com/questions/1609oo17/elisp-conditionally-change-keybinding
(defvar oo-alternate-commands (make-hash-table)
  "A hash-table mapping command symbols to a list of command symbols.")

(defun! oo-alternate-command-choose-fn (command)
  "Return an alternate command that should be called instead of COMMAND."
  (or (dolist (it (gethash command oo-alternate-commands))
        (aand! (funcall it) (break! it)))
      command))

(defmacro alt! (old new feature)
  `(progn (push (lambda (&rest _) (when (or (featurep ',feature) (require ',feature nil t)) ',new))
                (gethash ',old oo-alternate-commands))
          (define-key global-map [remap ,old] '(menu-item "" ,old :filter oo-alternate-command-choose-fn))))

;; The benefit of using a function as a wrapper to bind all my keys is I just
;; have to change this to change how I bind any keys.  I have full control
;; tdwiw.  Basically I was the option of binding keys programmatically if need
;; be.
;; Consider "global" a special state.
(defmacro pop-elt! (elt list-var)
  "Remove the first occurrence of ELT from LIST-VAR (a symbol) and return ELT.
Modifies LIST-VAR in place. Returns nil if ELT not found."
  (cl-with-gensyms (lst prev cur)
    `(let ((,lst ,list-var))
       (cond
        ((null ,lst) nil)
        ((equal (car ,lst) ,elt)
         (setq ,list-var (cdr ,lst))
         ,elt)
        (t
         (let ((,prev ,lst)
               (,cur (cdr ,lst)))
           (while (and ,cur (not (equal (car ,cur) ,elt)))
             (setq ,prev ,cur
                   ,cur (cdr ,cur)))
           (when ,cur
             (setcdr ,prev (cdr ,cur))
             ,elt)))))))

;; The point of this function is to give me a uniform interface for binding keys
;; where I do not have to worry about whether the keymap is defined or whether
;; evil is loaded.  Furthermore by having a function I can apply a change from
;; one to all bindings.
(defun! oo-bind-key (keymap key def &optional states)
  "Bind KEY to DEF in KEYMAP.
KEYMAP is a keymap symbol."
  (set! states (ensure-list states))
  (when (or (not states) (pop-elt! 'global states))
    (oo-call-after-bound keymap `(lambda () (keymap-set ,keymap ,key ',def))))
  (when states
    (setq key (if (vectorp key) key (kbd key)))
    (set! fn `(lambda () (evil-define-key* ',states ,keymap ,key ',def)))
    (oo-call-after-load 'evil (apply-partially #'oo-call-after-bound keymap fn)))
  nil)
;;; provide
(provide '036-base-functions)
;;; 036-base-functions.el ends here
