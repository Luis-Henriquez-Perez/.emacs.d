;;; config-telephone-line.el --- Configure telephone-line -*- lexical-binding: t; -*-
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
;; Configure telephone-line.
;;
;;; Code:
(require 'base)
(require 'telephone-line)
;;;; custom segments
;;;;; major-mode information
(defun +telephone-line-major-mode-segment nil
  (lambda (face) (alet! (format-mode-line (funcall (telephone-line-major-mode-segment) face)) (if (string-match "\\`ELisp" it) (substring it (match-beginning 0) (match-end 0)) it))))
;;;;; kbd-macro information
(defun +telephone-line-kbd-macro-segment nil
  (lambda (_) (oo-modeline-component--kbd-macro)))
;;;;; narrowing information
(defun +telephone-line-narrow-segment nil
  (lambda (_) (oo-modeline-component--narrow)))
;;;;; buffer
(defun +telephone-line-buffer-segment nil
  (lambda (_) (oo-modeline-component--buffer-name)))
;;;;; pomodoro
(defun +telephone-line-pomodoro-segment nil
  (lambda (_) (oo-modeline-component--pomodoro)))
;;;;; org timer (what I use as pomodoro)
;;;;; current-time
;; TODO: how to display somet
(defun +telephone-line-current-time-segment nil
  (lambda (_) (oo-modeline-component--current-time)))
;;;;; battery
(defun +telephone-line-battery-segment nil
  (lambda (_) (oo-modeline-component--battery)))
;;;;; emms
;; TODO: Add how much time is left plaing...
(defun +telephone-line-emms-segment nil
  (lambda (_) (oo-modeline-component--emms)))
;;;;; version control information
(defun +telephone-line-vc-segment nil
  (lambda (_) (oo-modeline-component--version-control)))
;;;;; read-only
(defun +telephone-line-read-only-segment nil
  (lambda (_) (oo-modeline-component--read-only)))
;;;; add utilities for updating the modeline
(defun! +telephone-line-update ()
  "Update the telephone-line modeline."
  (interactive)
  (set! modeline (if telephone-line-mode `("%e" ,@(telephone-line--generate-mode-line)) telephone-line--default-mode-line))
  (setq-default mode-line-format modeline)
  (oo-update-modeline))
;;; provide
(provide 'config-telephone-line)
;;; config-telephone-line.el ends here
