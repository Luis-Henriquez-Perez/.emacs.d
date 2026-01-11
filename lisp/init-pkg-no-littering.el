;;; init-pkg-no-littering.el --- initialize no-littering -*- lexical-binding: t; -*-
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
;; Initialize no-littering.
;;
;;; Code:
(require 'init-core)

(eval-and-compile (defvar no-littering-etc-directory o-etc-dir)
                  (defvar no-littering-var-directory o-var-dir))

(defun o-hook--load-no-littering ()
  "Load `no-littering'."
  (require 'no-littering))

(add-hook 'after-init-hook #'o-hook--load-no-littering -95)
;;; provide
(provide 'init-pkg-no-littering)
;;; init-pkg-no-littering.el ends here
