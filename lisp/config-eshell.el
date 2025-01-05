;;; config-eshell.el --- configuration for eshell -*- lexical-binding: t; -*-
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
;; This is my configuration for eshell.
;;
;;; Code:
(require 'eshell-z)
(require 'eshell-up)
;;;; Make eshell prompt read-only
;; Using evil and pressing backspace deletes.
(defun! oo--make-read-only (fn &rest args)
  "Make eshell prompt read-only."
  (set! prompt (apply fn args))
  (set! properties '(read-only t
                               front-sticky (font-lock-face read-only)
                               rear-nonsticky (font-lock-face read-only)))
  (add-text-properties 0 (length prompt) properties prompt)
  prompt)

(advice-add 'epe-theme-lambda :around #'oo--make-read-only)
;;;; eshell history
(opt! eshell-hist-ignoredups t)
;; boost eshell history-size
;; Increase the history size from 128 to 1000.
(opt! eshell-history-size 1000)
;;;; clear
;; Unexpectedly for me the eshell clear scrolled to the bottom.  As seen in a
;; stackoverflow answer as well as multiple blog posts, the solution is to use
;; "clear 1" instead, essentually telling emacs to use "clear-scrollback".  I
;; still do not like this though because it actually erases the contents of the
;; buffer and I do not want to do this unnecessarily.  I just want it to scroll up.
;; TODO: make into a snippet and/or abbrev
;; (message "current buffer %S" (buffer-name))
;; TODO: edit surrounding form so that it works in comments
;; (message "var %S" var)
;; (defun! eshell/scroll-to-top ()
;;   ;; The function `recenter' does not seem to work in the eshell buffer.  I do
;;   ;; not know why.
;;   ;; (call-interactively #'recenter-top-bottom)
;;   ;; (call-interactively #'recenter-top-bottom)
;;   ;; Need a function of the number of lines in the window for the following to
;;   ;; work.
;;   ;; (set! line-number (line-number-at-pos))
;;   ;; (goto-char (point-min))
;;   ;; (forward-line (1- line-number))
;;   )
;; (eshell/alias "clear" )
;;;; eshell
(defun! eshell/less (&rest files)
  "Essentially an alias to the `view-file' function."
  (set! (first . rest) files)
  (when files
	(view-file first)
	(when rest
	  (mapc #'view-file-other-window rest))))
;;; provide
(provide 'config-eshell)
;;; config-eshell.el ends here
