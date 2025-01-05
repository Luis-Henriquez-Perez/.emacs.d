;;; init-savehist.el --- savehist configuration -*- lexical-binding: t; -*-
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
;; This is my configuration for savehist.
;;
;;; Code:
(require 'base)
(require 'savehist)

(hook! on-first-input-hook savehist-mode)

(opt! savehist-save-minibuffer-history t)
(opt! savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(opt! savehist-autosave-interval (* 60 5))

(opt! savehist-additional-variables (cl-adjoin 'register-alist savehist-additional-variables))

(defun! oo--remove-kill-ring-properties (&rest _)
  (flet! when-fn (pred function) (lambda (x) (if (funcall pred x) (funcall function x) x)))
  (setq kill-ring (mapcar (when-fn #'stringp #'substring-no-properties) kill-ring)))

(advice-add 'savehist-save :before #'oo--remove-kill-ring-properties)
;;; provide
(provide 'init-savehist)
;;; init-savehist.el ends here
