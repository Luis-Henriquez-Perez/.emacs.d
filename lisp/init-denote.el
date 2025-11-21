;;; init-denote.el --- initialize denote -*- lexical-binding: t; -*-
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
;; Initialize denote.
;;
;;; Code:
(require 'base)

(opt! denote-directory "~/Documents/notes/")
(opt! denote-file-type 'org)
(opt! denote-prompts nil)
(opt! denote-org-front-matter "#+title: %1$s\n#+filetags: %3$s\n#+identifier: %4$s\n")
(opt! denote-save-buffers t)
(declare-function denote-rename-buffer-mode "denote")
(oo-call-after-load 'denote #'denote-rename-buffer-mode)
;; Set denote to insert state after entering.
(declare-function evil-insert-state "evil")
(defun oo-enter-insert-state-h ()
  "Enter insert state after entering denote buffer."
  ;; (message "LOG %s : %S" (format-time-string "%X") (current-buffer))
  (when (bound-and-true-p evil-mode)
    (evil-insert-state 1)))

(add-hook 'denote-after-new-note-hook #'oo-enter-insert-state-h)

(opt! denote-rename-confirmations nil)
;;; provide
(provide 'init-denote)
;;; init-denote.el ends here
