;;; 990-config-eshell.el --- configuration for eshell -*- lexical-binding: t; -*-
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
(require '050-base)
;;;; Prompt function
(defun! oo-eshell-prompt ()
  (set! path (abbreviate-file-name default-directory))
  (set! branch (aand! (car-safe (vc-git-branches)) (format "[ %s ]" it)))
  ;; Get the current time.
  (set! time (format-time-string "%H:%M"))
  (string-join (delq nil (list time path branch "λ\s")) "\s"))
;;;; clear
;; TODO: make into a snippet and/or abbrev
;; (message "current buffer %S" (buffer-name))
;; TODO: edit surrounding form so that it works in comments
;; (message "var %S" var)
;; Unexpectedly for me the eshell clear scrolled to the bottom.  As seen in a
;; stackoverflow answer as well as multiple blog posts, the solution is to use
;; "clear 1" instead, essentually telling emacs to use "clear-scrollback".  I
;; still do not like this though because it actually erases the contents of the
;; buffer and I do not want to do this unnecessarily.  I just want it to scroll
;; up.  I figured out why.
(defun oo-scroll-to-top-h (&rest _)
  "Hook that scrolls eshell to top of window."
  (recenter 0)
  (remove-hook 'eshell-post-command-hook #'oo-scroll-to-top-h 'local))

(defun eshell/scroll-to-top ()
  "Scroll the Eshell to the top without clearing the buffer."
  (add-hook 'eshell-post-command-hook #'oo-scroll-to-top-h nil 'local))

;; Replace `eshell/clear' with this function.
;; (defalias 'eshell/clear 'eshell/scroll-to-top)

;; I do not necessarily want to always scroll to the top but I want eshell to
;; preserve it is previous position in the window.
;;;; eshell
(defun! eshell/less (&rest files)
  "Essentially an alias to the `view-file' function."
  (set! (first . rest) files)
  (when files
	(view-file first)
	(when rest
	  (mapc #'view-file-other-window rest))))
;;;; settings
(setopt eshell-banner-message "")
(setopt eshell-highlight-prompt nil)
(autoload! oo-eshell-prompt "990-config-eshell")
;; For now outsource to epe, but later I will make my own.  Also epe uses static
;; faces by which I mean constant faces, not existing ones that change with
;; themes.  So the prompt is difficult to read with certain themes, particularly
;; light themes.
(autoload! oo-eshell-prompt "990-config-eshell")
(setopt eshell-prompt-function 'oo-eshell-prompt)
;; This is obsolete as of Emacs 30.1.
(setopt eshell-prompt-regexp "^[^λ]+λ ")
(setopt eshell-hist-ignoredups t)
;; Prefer external commands over lisp functions.
(setopt eshell-prefer-lisp-functions t)
;; Represent buffers as #<buffer-name>
(setopt eshell-buffer-shorthand t)
;; boost eshell history-size
;; Increase the history size from 128 to 1000.
(setopt eshell-history-size 1000)
;; By "highlight" eshell does not just mean coloring the font with the
;; `eshell-prompt' face.  It also makes the prompt read-only.  Strangely, the
;; prompt is not read-only by default.  Furthermore, there is no way to override
;; the text properties `eshell-emit-prompt' adds to the prompt without advising
;; the it.
(setopt eshell-highlight-prompt t)
(setopt eshell-hist-ignoredups t)
;; boost eshell history-size
;; Increase the history size from 128 to 1000.
(setopt eshell-history-size 1000)
;; Stop eshell from printing messages.

(nmap eshell-mode-map "J" #'eshell-previous-prompt)
(nmap eshell-mode-map "K" #'eshell-next-prompt)
;;; provide
(provide '990-config-eshell)
;;; 990-config-eshell.el ends here
