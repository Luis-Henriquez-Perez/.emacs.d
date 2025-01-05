;;; init-evil-fringe-mark.el --- Initialize evil-fringe-mark -*- lexical-binding: t; -*-
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
;; Initialize evil-fringe-mark.
;;
;;; Code:
(require 'base)

(defun oo-enable-evil-fringe-mark-a (orig-fn &rest args)
  "Enable `evil-fringe-mark'."
  (prog1 (apply orig-fn args)
    (cond ((require 'evil-fringe-mark nil t)
           (global-evil-fringe-mark-mode 1)
           (advice-remove 'evil-set-marker #'oo-enable-evil-fringe-mark-a))
          (t
           (warn! "Could not load `evil-fringe-mark-mode'.")))))

(advice-add 'evil-set-marker :around #'oo-enable-evil-fringe-mark-a)
;;; provide
(provide 'init-evil-fringe-mark)
;;; init-evil-fringe-mark.el ends here
