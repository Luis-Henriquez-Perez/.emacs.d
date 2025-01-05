;;; config-restart-emacs.el --- Configure restart-emacs -*- lexical-binding: t; -*-
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
;; Configure restart-emacs.
;;
;;; Code:
;;;; fix interactive call
;; When using the function `restart-emacs-start-new-emacs' I find that restart
;; Emacs does not properly work with prefix arguments because in its body it
;; converts the prefix argument to shell arguments only if its called
;; interactively but its not.
(defun oo--work-interactively (&optional args)
  "Call `restart-emacs' interactively."
  (let ((restart-emacs--inhibit-kill-p t))
    (funcall-interactively #'restart-emacs args)))

(advice-add 'restart-emacs-start-new-emacs :override #'oo--work-interactively)
;;; provide
(provide 'config-restart-emacs)
;;; config-restart-emacs.el ends here
