;;; config-outli.el --- Configure outli -*- lexical-binding: t; -*-
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
;; Configure `outli'.
;;
;;; Code:
;; Different styling for stem and leaf characters.
(setf (cl-fourth (assoc 'emacs-lisp-mode outli-heading-config)) nil)

(setf (alist-get 'lua-mode    outli-heading-config) '("--" ?- nil t))
(setf (alist-get 'python-mode outli-heading-config) '("#" ?# nil t))
(setf (alist-get 'conf-mode outli-heading-config) '("#" ?# nil t))
(setf (alist-get 'sh-mode outli-heading-config) '("#" ?# nil t))
(setf (alist-get 'fennel-mode outli-heading-config)
      (alist-get 'emacs-lisp-mode outli-heading-config))
(setf (alist-get 'lisp-mode outli-heading-config)
      (alist-get 'emacs-lisp-mode outli-heading-config))
(setf (alist-get 'hy-mode     outli-heading-config)
      (alist-get 'emacs-lisp-mode outli-heading-config))

(hook! enable-theme-functions outli-reset-all-faces :ignore-args t)
;;; provide
(provide 'config-outli)
;;; config-outli.el ends here
