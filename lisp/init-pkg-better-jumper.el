;;; init-pkg-better-jumper.el --- Initialize better-jumper -*- lexical-binding: t; -*-
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
;; Initialize `better-jumper'.
;;
;;; Code:
(o-declare-package 'better-jumper)

(declare-function better-jumper-set-jump "better-jumper")
(declare-function better-jumper-jump-forward "better-jumper")
(declare-function better-jumper-jump-backward "better-jumper")

(autoload 'better-jumper-jump-backward "better-jumper" nil t 'function)
(autoload 'better-jumper-jump-forward "better-jumper" nil t 'function)
(autoload 'better-jumper-set-jump "better-jumper" nil t 'function)

(o-setq better-jumper-max-length 500)

(advice-add 'meep--mark-on-motion-set :around #'o-hook--register-jump)

(defun o-hook--register-jump (fn pos always)
  "Register the jump."
  (better-jumper-set-jump pos)
  (funcall fn pos always))
;;; provide
(provide 'init-pkg-better-jumper)
;;; init-pkg-better-jumper.el ends here
