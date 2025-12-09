;;; init-telephone-line.el --- Initialize telephone-line -*- lexical-binding: t; -*-
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
;; Initialize telephone-line.
;;
;;; Code:
(require 'base)
(require 'seq)
(require 'telephone-line)
;;;; basic setup
(defun o-setup-telephone-line-h ()
  "Enable telephone-line mode.
Additionally make it so that whenever telephone-line-mode is called after this,
the modeline is updated."
  (telephone-line-mode 1)
  (add-hook 'telephone-line-mode-hook #'o-update-modeline))

(add-hook 'emacs-startup-hook #'o-setup-telephone-line-h)

;; Add a timer to toggle the separators.
;; (o-opt telephone-line-height 30)
(o-opt telephone-line-height 24)
(o-opt telephone-line-evil-use-short-tag nil)
;;;; fn for updating the ml
(o-defun +telephone-line-update ()
  "Update the telephone-line modeline."
  (interactive)
  (o-set modeline (if telephone-line-mode `("%e" ,@(telephone-line--generate-mode-line)) telephone-line--default-mode-line))
  (setq-default mode-line-format modeline)
  (o-update-modeline))
;;;; allow toggling different separators
(defun +telephone-line-apply-gradient-separator (&optional update)
  (interactive)
  (setq update (or update (called-interactively-p 'any)))
  (o-opt telephone-line-primary-left-separator 'telephone-line-gradient)
  (o-opt telephone-line-secondary-left-separator 'telephone-line-nil)
  (o-opt telephone-line-primary-right-separator 'telephone-line-gradient)
  (o-opt telephone-line-secondary-right-separator 'telephone-line-nil)
  (when update (+telephone-line-update)))

(defun +telephone-line-apply-abs-separator (&optional update)
  (interactive)
  (setq update (or update (called-interactively-p 'any)))
  (o-opt telephone-line-primary-right-separator 'telephone-line-abs-right)
  (o-opt telephone-line-secondary-right-separator 'telephone-line-abs-hollow-right)
  (o-opt telephone-line-primary-left-separator 'telephone-line-abs-left)
  (o-opt telephone-line-secondary-left-separator 'telephone-line-abs-hollow-left)
  (when update (+telephone-line-update)))

(defun +telephone-line-apply-cos-separator (&optional update)
  (interactive)
  (setq update (or update (called-interactively-p 'any)))
  (o-opt telephone-line-primary-right-separator 'telephone-line-cos-right)
  (o-opt telephone-line-secondary-right-separator 'telephone-line-cos-hollow-right)
  (o-opt telephone-line-primary-left-separator 'telephone-line-cos-left)
  (o-opt telephone-line-secondary-left-separator 'telephone-line-cos-hollow-left)
  (when update (+telephone-line-update)))

(defun +telephone-line-apply-cubed-separator (&optional update)
  (interactive)
  (setq update (or update (called-interactively-p 'any)))
  (o-opt telephone-line-primary-left-separator 'telephone-line-cubed-left)
  (o-opt telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left)
  (o-opt telephone-line-primary-right-separator 'telephone-line-cubed-right)
  (o-opt telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  (when update (+telephone-line-update)))
;;;; segments
;; Choose a random separator.  Probably I will remove this in favor of
;; consistency later.  For now though, it is pretty cool.
(funcall (seq-random-elt '(+telephone-line-apply-cubed-separator
                           +telephone-line-apply-gradient-separator
                           +telephone-line-apply-abs-separator
                           +telephone-line-apply-cos-separator))
         nil)

(o-opt telephone-line-lhs
      '((evil   telephone-line-evil-tag-segment)
        (accent +telephone-line-vc-segment telephone-line-process-segment)
        (nil    +telephone-line-buffer-segment)))

(o-opt telephone-line-rhs
      '((nil    telephone-line-misc-info-segment)
        (accent +telephone-line-pomodoro-segment
                +telephone-line-major-mode-segment
                +telephone-line-kbd-macro-segment)
        (evil +telephone-line-current-time-segment)))
;;; provide
(provide 'init-telephone-line)
;;; init-telephone-line.el ends here
