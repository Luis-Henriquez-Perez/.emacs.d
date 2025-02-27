;;; 032-after-load-functions.el --- Define functions for deferred loading -*- lexical-binding: t; -*-
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
;; Define functions for deferred loading
;;
;;; Code:
(require 'subr-x)
(require '001-init-log)
(require '030-base-functions)
(eval-when-compile (require '031-anaphoric-macros))
(eval-when-compile (require '031-autolet-macros))
(eval-when-compile (require '031-looping-macros))

;; This alist is meant to call certain functions whenever a file is loaded.  It
;; is meant for things could happen at any time.  Right now I use it for evil
;; state characters and knowing when a symbol is defined--specifically keymaps
;; which I use for keybindings and variable symbols used in `opt!'.
(defvar oo-after-load-hash-table (make-hash-table :size 100)
  "A hash table whose elements are (ITEM . FUNCTIONS).
ITEM is either a symbol or a character (an integer).  FUNCTIONS is a list of
functions.")

;; So remember with `oo-after-load-hash-table' that we push the elements in so if we
;; loop through it normally the first item processed is actually the last item
;; we entered into the list. I personally would expect the items to be processed
;; in the order I added them.

;; This is actually trickier than it seems.  Before I used to push the elements
;; I did not touch into a list and simply set the value of
;; `oo-after-load-hash-table' to that list.  But surprisingly, some
;; elements of the alist would disappear.  A long while later I realized why: a
;; side-effect of this looping is modifying the list.  By setting.  Instead, I
;; need to keep track of the elements I will remove.
(defun! oo-call-after-load-functions (&rest _)
  "Call functions in `oo-after-load-hash-table' that need to be called.
Also, update `oo-after-load-hash-table' to reflect functions called."
  (for! (reverse key (hash-table-keys oo-after-load-hash-table))
    (cond ((and (symbolp key) (boundp key))
           (for! (reverse fn (gethash key oo-after-load-hash-table))
             (funcall fn))
           (remhash key oo-after-load-hash-table))
          ((and (integerp key)
                (featurep 'evil)
                (set! state (oo--evil-char-to-state key)))
           (for! (reverse fn (gethash key oo-after-load-hash-table))
             (funcall fn state))
           (remhash key oo-after-load-hash-table)))))

(defun oo-call-after-bound (symbol fn)
  "Call FN after SYMBOL is bound.
Call FN immediately if SYMBOL is already bound.  Otherwise, register
SYMBOL and FN in `oo-after-load-hash-table'."
  (if (boundp symbol)
      (funcall fn)
    (push fn (gethash symbol oo-after-load-hash-table))))

(defvar evil-state-properties)
(defun oo--evil-char-to-state (char)
  "Return state whose first letter is CHAR."
  (cl-find-if (lambda (state) (= char (string-to-char (symbol-name state))))
              (mapcar #'car evil-state-properties)))

;; These functions and variables have to be outside.  If you provide a symbol
;; for a nonexistent state evil will actually create a keymap for it.  When you
;; do define the state that corresponds to the symbol then it will end up
;; working out and that keymap that was initially created will be used.  This
;; means I actually do not need this for evil state symbols.  The reason then
;; that I need this is for the state characters I use to abbrev evil states.
(defun oo-call-after-evil-state-char (char fn)
  "Call FN with state after state starting with CHAR is defined."
  (aif! (and (bound-and-true-p evil-mode) (oo--evil-char-to-state char))
      (funcall fn it)
    (push fn (gethash char oo-after-load-hash-table))))

(defun oo--call-after-load (expr fn)
  "Call FN after EXPR is met."
  (pcase expr
    ((pred null)
     (funcall fn nil))
    (`(:or . ,exprs)
     (dolist (expr exprs)
       (oo-call-after-load expr fn)))
    (`(:and . ,exprs)
     (oo--call-after-load exprs fn))
    ((or `(,(and feature (pred symbolp))) (and feature (pred symbolp)))
     (if (featurep feature)
         (funcall fn feature)
       (eval-after-load feature (apply-partially #'oo--call-after-load feature fn))))
    (`(,expr . ,exprs)
     (oo--call-after-load expr `(lambda (_) (oo--call-after-load ',exprs #',fn))))
    (_
     (error "invalid expression `%S'" expr))))

;; This macro is designed with the following goals in mind.
;; 1 - use one generic macro for most binding needs
;; 2 - log the variables I set and when they are being set
;; You'll get a warning when trying to bind a symbol that hasn't been defined yet.
;; So it's best to bind a package symbol only after the package has been loaded.
;; 3 - stop worrying about variables that haven't been bound
;; 4 - stop worrying about whether a variable is a custom variable or not
;; Some variables are custom variables.  Meaning they have some function that.
(defun oo-call-after-load (expr fn)
  "Call FN with ARGS after EXPR resolves.
EXPR can be a feature (symbol), a list of CONDITIONS, a list whose CAR is
either `:or' or `:and' and whose CDR is a list of EXPRS.  If CONDITION is a
feature, call FN with ARGS if feature has already been provided; otherwise,
behave similarly to `eval-after-load'.  If EXPR is a list of
EXPRS, call FN with ARGS only after all CONDITIONS have been met.  If
EXPR is a list whose CAR is `:and' behave the same way as (CDR CONDITION).
If EXPR is a list whose CAR is `:or', call FN with ARGS after any of
EXPRS in (CDR CONDITION) is met."
  (alet! (eval `(let ((first-call-p t))
                  (lambda (&optional feature)
                    (when first-call-p
                      (setq first-call-p nil)
                      (oo-log 'trace "AFTER-LOAD: %s -> %s" feature #',fn)
                      (condition-case err
                          (funcall #',fn)
                        (error
                         (if oo-debug-p
                             (signal (car err) (cdr err))
                           (oo-log 'error "`%s` : %s -> %s"
                                   ',fn
                                   (car err)
                                   (cdr err))))))))
               t)
    (oo--call-after-load expr it)))
;;; provide
(provide '032-after-load-functions)
;;; 032-after-load-functions.el ends here
