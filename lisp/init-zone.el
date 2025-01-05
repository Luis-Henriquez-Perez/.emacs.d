;;; init-zone.el --- Initialize zone -*- lexical-binding: t; -*-
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
;; Initialize `zone'.
;;
;;; Code:
;; (autoload #'+zone-choose "zone" nil t 'function)
;; https://www.emacswiki.org/emacs/ZoneMode
(defun +zone-choose (pgm)
  "Choose a PGM to run for `zone'."
  (interactive (list (completing-read "Program: " (mapcar 'symbol-name zone-programs))))
  (require 'zone)
  (zone))

(defun oo-enable-zone ()
  (require 'zone nil t)
  (zone))

;; TODO: get rid of some zones.
(defhook! oo-start-zone-timer-h (emacs-startup-hook)
  (setq oo-zone-timer (run-with-idle-timer 5 t #'oo-enable-zone)))

(defun oo-cancel-zone ()
  (cancel-timer oo-zone-timer))

;; I intentionally set the timer object returned to a variable so I can cancel
;; zoning if I want to.
(defvar oo-zone-timer nil
  "Timer for when to zone out.")
;;; provide
(provide 'init-zone)
;;; init-zone.el ends here
