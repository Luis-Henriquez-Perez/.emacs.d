;;; init-spaceline.el --- Initialize spaceline -*- lexical-binding: t; -*-
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
;; Initialize spaceline.
;;
;;; Code:
;;;; requirements
(require 'spaceline)
(require 'spaceline-segments)
(require 'all-the-icons)
;;;; settings
(o-opt spaceline-highlight-face-func #'spaceline-highlight-face-evil-state)
(o-opt powerline-height 40)
(o-opt powerline-default-separator 'arrow)
(setq spaceline-separator-dir-left '(left . left))
(setq spaceline-separator-dir-right '(right . right))
;; Although this saves time the longer you use the modeline, it means that the
;; call to `spaceline-compile' is called takes significantly longer which is
;; particularly undesirable during startup.  Despite what the README says the
;; rendering/updating of the modeline does not make a noticeable difference to
;; me.  I imagine it matters more for particularly expensive modeline segments.
;; Still I will byte-compile it but not during startup, at some point during
;; idle time.
;; TODO: during idle time byte-compile the spaceline function.
(setq spaceline-byte-compile nil)
;;;; reset powerline after theme change
(add-hook 'enable-theme-functions #'powerline-reset)
;;;; segments
(spaceline-define-segment +kbd-macro
  (o-modeline-component--kbd-macro))

(spaceline-define-segment +narrow
  (o-modeline-component--narrow))

(spaceline-define-segment +buffer-read-only
  (o-modeline-component--read-only))

(spaceline-define-segment +buffer-modified
  (o-modeline-component--buffer-modified))

(spaceline-define-segment +pomodoro
  (o-modeline-component--pomodoro))

(spaceline-define-segment +version-control
  (o-modeline-component--version-control))

(spaceline-define-segment +evil-state
  (o-modeline-component--evil-state))

(spaceline-define-segment +current-time
  (o-modeline-component--current-time))

(spaceline-define-segment +buffer-name
  (o-modeline-component--buffer-name))

(spaceline-define-segment +emms
  (o-modeline-component--emms))
;;;; define main modeline
(spaceline-compile
  'main
  '((+evil-state :face (spaceline-highlight-face-evil-state))
    (+narrow +kbd-macro +buffer-read-only +buffer-modified +buffer-name :face 'powerline-active0)
    (+version-control :face 'powerline-active1))
  '((+pomodoro :face 'powerline-active1)
    +emms
    major-mode
    (+current-time :face (spaceline-highlight-face-evil-state))))
;;;; toggle default separator
;; I want the ability to quickly switch between different separators.

;; (o-defun o-choose-modeline-separator ()
;;   ;; "Choose a separator for the modeline."
;;   (interactive)
;;   (o-set separators '(alternate arrow arrow-fade bar box brace
;;                                butt chamfer contour curve rounded roundstub wave zigzag
;;                                slant utf-8))
;;   (o-awhen (completing-read "Choose separator: " separators)
;;     (setq powerline-default-separator it)
;;     (spaceline-compile)))

;; (o-defun o-choose-random-separator ()
;;   "Set a random separator."
;;   (interactive)
;;   (o-set separators '(alternate arrow arrow-fade bar box brace
;;                                butt chamfer contour curve rounded roundstub wave zigzag
;;                                slant utf-8))
;;   (setq powerline-default-separator (seq-random-elt separators))
;;   (spaceline-compile)
;;   (message "set separator to %s" powerline-default-separator))
;;;; initialize modeline at startup
(o-defhook o-hook--initialize-modeline (after-init-hook :depth 90)
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))
;;; provide
(provide 'init-spaceline)
;;; init-spaceline.el ends here
