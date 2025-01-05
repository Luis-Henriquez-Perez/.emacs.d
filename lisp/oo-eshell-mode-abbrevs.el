;;; oo-eshell-mode-abbrevs.el --- abbrevs for eshell-mode -*- lexical-binding: t; -*-
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
;; Abbrevs for eshell-mode.
;;
;;; Code:
;;;; requirements
(require 'base)
(require 'abbrev)
;;;; abbrev pred
(defun oo--use-eshell-mode-abbrev-p ()
  "Return non-nil if eshell-mode abbrev should be expanded."
  (derived-mode-p 'eshell-mode))
;;;; abbrevs
;;;;; git abbrevs
(define-abbrev global-abbrev-table "gba"   "git --no-pager branch -a" nil :enable-function #'oo--use-eshell-mode-abbrev-p)
;; (define-abbrev global-abbrev-table "gb"    "git --no-pager branch"    nil :enable-function #'oo--use-eshell-mode-abbrev-p)
(define-abbrev global-abbrev-table "gb"    "git for-each-ref --sort=creatordate --format='%(creatordate:short) %(refname:short)' refs/heads/" nil :enable-function #'oo--use-eshell-mode-abbrev-p)
(define-abbrev global-abbrev-table "gs"    "git status -s"            nil :enable-function #'oo--use-eshell-mode-abbrev-p)
(define-abbrev global-abbrev-table "gp"    "git push"                 nil :enable-function #'oo--use-eshell-mode-abbrev-p)
(define-abbrev global-abbrev-table "gl"    "git log --oneline -n 15"  nil :enable-function #'oo--use-eshell-mode-abbrev-p)
(define-abbrev global-abbrev-table "ga"    "git commit --amend"       nil :enable-function #'oo--use-eshell-mode-abbrev-p)
(define-abbrev global-abbrev-table "glast" "git log -1 HEAD --stat"   nil :enable-function #'oo--use-eshell-mode-abbrev-p)
;;;;; Miscellaneous
(define-abbrev global-abbrev-table "py" "python"     nil :enable-function #'oo--use-eshell-mode-abbrev-p)
(define-abbrev global-abbrev-table "cl" "clear"      nil :enable-function #'oo--use-eshell-mode-abbrev-p)
(define-abbrev global-abbrev-table "dj" "dired-jump" nil :enable-function #'oo--use-eshell-mode-abbrev-p)
;;;;; snippet abbrevs
(put 'oo--insert-drop-stash-template 'no-self-insert t)
(defun oo--insert-drop-stash-template ()
  (require 'tempel)
  (tempel-insert '("git stash drop stash@{" (p "STASH NUMBER") "}" p))
  (when (bound-and-true-p evil-mode)
    (evil-normalize-keymaps))
  t)
(define-abbrev global-abbrev-table "gdrop" "" 'oo--insert-drop-stash-template :enable-function 'oo--use-eshell-mode-abbrev-p)

(put 'oo--insert-git-commit-template 'no-self-insert t)
(defun oo--insert-git-commit-template ()
  (require 'tempel)
  (tempel-insert '("git commit -m \"" (p "COMMIT MESSAGE") "\""))
  (when (bound-and-true-p evil-mode)
    (evil-normalize-keymaps))
  t)

(define-abbrev global-abbrev-table "gc" "" 'oo--insert-git-commit-template :enable-function 'oo--use-eshell-mode-abbrev-p)
;;; provide
(provide 'oo-eshell-mode-abbrevs)
;;; oo-eshell-mode-abbrevs.el ends here
