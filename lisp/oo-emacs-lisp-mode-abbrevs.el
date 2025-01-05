;;; oo-emacs-lisp-mode-abbrevs.el --- abbrevs for emacs-lisp-mode -*- lexical-binding: t; -*-
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
;; These are abbrevs for emacs-lisp-mode.
;;
;;; Code:
;;;; requirements
(require 'abbrev)
;;;; emacs-lisp
(defun oo--use-emacs-lisp-mode-abbrevs-p ()
  "Return non-nil when emacs-lisp-mode abbrevs should expand.
This is when `emacs-lisp-mode' is enabled and point is not in a string or
comment."
  (and (derived-mode-p 'emacs-lisp-mode)
       (not (oo-in-string-or-comment-p))))
;;;; abbrev snippets
;; TODO: make a function or (most likely) macro for this.
;; These are the rules that must be strictly followed for a snippet abbrev to
;; work with tempel.
;; 1. It needs to be a function symbol that is passed in as the hook.  No
;;    lambdas.
;; 2. The symbol should have a `no-self-insert' property of t.  Otherwise, the
;;    abbrev will be inserted as well.
;; 3. When using evil, the snippet should call `evil-normalize-keymaps' after
;;    the snippet has been inserted otherwise the bindings for `tempel-map' will
;;    not be active until a state change.
;; 4. The definition of the abbrev must be an empty string.
;; 5. The hook function must return t.
(put 'oo--insert-defun-template 'no-self-insert t)
(defun oo--insert-defun-template ()
  (require 'tempel)
  (tempel-insert 'fn)
  (when (bound-and-true-p evil-mode)
    (evil-normalize-keymaps))
  t)

(define-abbrev global-abbrev-table "fun" "" 'oo--insert-defun-template :enable-function 'oo--use-emacs-lisp-mode-abbrevs-p)

(put 'oo--insert-alias-template 'no-self-insert t)
(defun oo--insert-alias-template ()
  (require 'tempel)
  (tempel-insert 'als)
  (when (bound-and-true-p evil-mode)
    (evil-normalize-keymaps))
  t)
(define-abbrev global-abbrev-table "als" "" 'oo--insert-alias-template :enable-function 'oo--use-emacs-lisp-mode-abbrevs-p)

(put 'oo--insert-defvar-template 'no-self-insert t)
(defun oo--insert-defvar-template ()
  (require 'tempel)
  (tempel-insert 'vr)
  (when (bound-and-true-p evil-mode)
    (evil-normalize-keymaps))
  t)
;; I found a surprising interaction with abbrev tables and enable functions.
;; Prior to this I had thought that if I added an enable property to a table I
;; could override this property for an individual abbrev by specifying its
;; enable function explicitly.  However, it seems that the enable function of
;; the table is still considered.  Adding an enable function makes it so that
;; both the table's enable function and the abbrev's enable function have to
;; return true.
(define-abbrev global-abbrev-table "dv" "" 'oo--insert-defvar-template :enable-function 'oo--use-emacs-lisp-mode-abbrevs-p)

(put 'oo--insert-msg-template 'no-self-insert t)
(defun oo--insert-msg-template ()
  (require 'tempel)
  (tempel-insert 'lg)
  (when (bound-and-true-p evil-mode)
    (evil-normalize-keymaps))
  t)

(put 'oo--insert-set-bang-template 'no-self-insert t)
(defun oo--insert-set-bang-template ()
  (require 'tempel)
  (tempel-insert '("(set! " (p "variable") " " (p "value") ")"))
  (when (bound-and-true-p evil-mode)
    (evil-normalize-keymaps))
  t)

(put 'oo--insert-set-bang-template 'no-self-insert t)

(defun oo--insert-rsf-template ()
  (require 'tempel)
  (tempel-insert '("(re-search-forward " (p "regexp") " " (p "beg") ")"))
  (when (bound-and-true-p evil-mode)
    (evil-normalize-keymaps))
  t)

(define-abbrev global-abbrev-table "rsf" "" 'oo--insert-rsf-template :enable-function 'oo--use-emacs-lisp-mode-abbrevs-p)

(define-abbrev global-abbrev-table "sett" "" 'oo--insert-set-bang-template :enable-function 'oo--use-emacs-lisp-mode-abbrevs-p)

(define-abbrev global-abbrev-table "msg" "" 'oo--insert-msg-template :enable-function 'oo--use-emacs-lisp-mode-abbrevs-p)

(define-abbrev global-abbrev-table "rnn" "return non-nil if" nil  :enable-function 'oo--use-emacs-lisp-mode-abbrevs-p)

(define-abbrev global-abbrev-table "nl" "nil" nil  :enable-function 'oo--use-emacs-lisp-mode-abbrevs-p)

(define-abbrev global-abbrev-table "pmin" "(point-min)" nil  :enable-function 'oo--use-emacs-lisp-mode-abbrevs-p)

(define-abbrev global-abbrev-table "pmax" "(point-max)" nil  :enable-function 'oo--use-emacs-lisp-mode-abbrevs-p)

(define-abbrev global-abbrev-table "optionaal" "optional" nil  :enable-function 'oo--use-emacs-lisp-mode-abbrevs-p)
;;; provide
(provide 'oo-emacs-lisp-mode-abbrevs)
;;; oo-emacs-lisp-mode-abbrevs.el ends here
