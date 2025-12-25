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
(require 'functions-call-after)

;; https://stackoverflow.com/questions/1609oo17/elisp-conditionally-change-keybinding
(defvar o-alt-cmds nil
  "")

(o-defun o-get-alt-cmd (cmd)
  "Return an alternate command that should be called instead of COMMAND."
  (pcase-dolist (`(,feature . ,alt) (alist-get cmd o-alt-cmds))
    (if (or (featurep feature) (require feature nil t))
        (o-return alt)))
  cmd)

(defun o-remap-alt (feature orig new)
  "Remap ORIG command to NEW if FEATURE is loaded."
  (setf (alist-get feature (alist-get orig o-alt-cmds)) new)
  (define-key global-map `[remap ,orig] `(menu-item "" ,orig :filter o-get-alt-cmd)))

;; The point of this function is to give me a uniform interface for binding keys
;; where I do not have to worry about whether the keymap is defined or whether
;; evil is loaded.  Furthermore by having a function I can apply a change from
;; one to all bindings.
(o-defun o--set-mode-local-vars (hook)
  "Set local variables for mode corresponding to HOOK."
  (o-set failmsg "Failed to set local variable %s to value %S")
  (pcase-dolist (`(,symbol . ,value) (alist-get hook o-local-var-alist))
    (o-set bodyform `(setq-local ,symbol ,value))
    (o-set handlerbody `(o-log 'failure ,failmsg ',symbol (car err) (cdr err)))
    (o-pushing forms `(condition-case err ,bodyform (error ,handlerbody))))
  (eval (macroexp-progn (nreverse forms)) t))
;;; provide
(provide 'functions-2)
;;; functions-2.el ends here
