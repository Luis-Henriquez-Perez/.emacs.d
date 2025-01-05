;;; init-files.el --- initialize files -*- lexical-binding: t; -*-
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
;; Initialize files.
;;
;;; Code:
(require 'base)
;;;; backup files to trash
(setq backup-directory-alist '((".*" . "~/.Trash")))
;;;; don't make backups
(setq make-backup-files nil)
;;;; don't pass case-insensitive to =auto-mode-alist=
;; This is taken from =centaur-emacs=.  By default [[file:snapshots/*helpful variable: auto-mode-case-fold*.png][auto-mode-case-fold]] is
;; non-nil; when enabled the auto-mode-alist is traversed twice.  This double
;; traversal can be expensive and it seems unnecessary.
(setq auto-mode-case-fold nil)
;;;; stop asking me whether I want to enable file local variables
;; When installing packages with =quelpa=, I was prompted whether I wanted to apply
;; file local variables.  I'm guessing =straight.el= and =elpaca= disable this.
;; The value safe tells Emacs to only apply the "safe" local variables.  I'm
;; assuming this means ones like "mode" which tell Emacs to open the buffer at a
;; certain major mode.  At first I had this set to nil, but I wanted to open
;; [[][]] in =common-lisp-mode= and I realized Emacs wasn't doing it because I
;; told it not to with this variable.
(setq enable-local-variables :safe)
;;;; ensure there's always a newline at the end of files
;; Several linux programs require a newline at the end of a file, such as
;; chrontab--this is more or less what noctuid said and I'll take his word for
;; it.
(setq require-final-newline t)
;;;; designate location of trash
;; designate the location of the trash directory
;; I accidentally sent files to the trash and I could not find them in my trash
;; directory.  I was confused because I knew that the variable
;; [[file:_helpful_variable__delete-by-moving-to-trash_.png][delete-by-moving-to-trash]] was non-nil and I even verified this to be the case
;; with [[file:_helpful_function__helpful-variable_.png][helpful-variable]].  After reading the documentation of [[][]] I realized
;; that emacs uses the [[][]].  To be honest I had no idea what this actually was
;; but I extracted what looked like the location, [[][]].
(setq trash-directory (expand-file-name "~/Trash"))
;;;; diable auto-save-mode
(setq auto-save-default nil)
(auto-save-mode -1)

(setq idle-update-delay 1.0)
;;;; automatically kill any processes when exiting emacs
;; If I start a process, like the =eat= shell for example, stop me from exiting
;; to ask me whether I want to kill it, just do it.
;; https://emacsredux.com/blog/2020/07/18/automatically-kill-running-processes-on-exit/
(setq confirm-kill-processes nil)
;;;; confirm before quitting Emacs
(setq confirm-kill-emacs #'y-or-n-p)
;;;; silence save-buffer
(advice-add 'save-buffer :around #'oo-funcall-quietly)
;;;; keybindings
(bind! oo-find-map ";" #'save-buffer)
(bind! oo-find-map "o" #'find-file)
;;; provide
(provide 'init-files)
;;; init-files.el ends here
