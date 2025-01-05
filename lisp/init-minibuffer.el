;;; init-minibuffer.el --- Initialize minibuffer -*- lexical-binding: t; -*-
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
;; Initialize minibuffer.
;;
;;; Code:
(defun oo-completion-in-region-function (&rest args)
  (apply (if (and (bound-and-true-p vertico-mode) (featurep 'consult))
             #'consult-completion-in-region
           #'completion--in-region)
         args))

(setq completion-in-region-function #'oo-completion-in-region-function)
;;; provide
(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
