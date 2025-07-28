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
;;;; Make eshell prompt read-only
;; (defun! oo-eshell-prompt ()
;;   ;; (set! path (abbreviate-file-name default-directory))
;;   ;; (set! branch (car-safe (vc-git-branches)))
;;   ;; (format "%s [ %s ] λ " path branch)
;;   (epe-theme-pipeline)
;;   )
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
(defun! eshell/scroll-to-top ()
  ;; The function `recenter' does not seem to work in the eshell buffer.  I do
  ;; not know why.
  ;; (let ((inhibit-read-only t))
  ;;   (recenter 0))
  )
;; (eshell/alias "clear" )
(defun oo-scroll-to-top-h (&rest _)
  "Hook that scrolls eshell to top of window."
  (recenter 0)
  (remove-hook 'eshell-post-command-hook #'oo-scroll-to-top-h 'local))

(defun eshell/scroll-to-top ()
  "Scroll the Eshell to the top without clearing the buffer."
  (add-hook 'eshell-post-command-hook #'oo-scroll-to-top-h nil 'local))

;; Replace `eshell/clear' with this function.
;; (defalias 'eshell/clear 'eshell/scroll-to-top)
;;;; eshell
(defun! eshell/less (&rest files)
  "Essentially an alias to the `view-file' function."
  (set! (first . rest) files)
  (when files
	(view-file first)
	(when rest
	  (mapc #'view-file-other-window rest))))
;;; provide
(provide '990-config-eshell)
;;; 990-config-eshell.el ends here
