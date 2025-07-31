;;; 130-init-auto-insert.el --- Initialize auto-insert -*- lexical-binding: t; -*-
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
;; Initialize auto-insert.
;;
;;; Code:
(require '050-base)

(opt! auto-insert-query nil)

(hook! oo-first-file-hook auto-insert-mode)

(autoload! oo-auto-insert-elisp-template     "990-config-auto-insert")
(autoload! oo-auto-insert-python-file-header "990-config-auto-insert")
(autoload! oo-auto-insert-html-template      "990-config-auto-insert")
(autoload! oo-auto-insert-hy-file-header     "990-config-auto-insert")
(autoload! oo-auto-insert-bash-file-header   "990-config-auto-insert")
(autoload! oo-auto-insert-org-file-header    "990-config-auto-insert")
(autoload! oo-auto-insert-script-file-header "990-config-auto-insert")


(alet! (concat (regexp-quote (expand-file-name "~/.local/bin/")) "[^./]+$")
  (define-auto-insert it #'oo-auto-insert-script-file-header))
(define-auto-insert "\\.el$"   #'oo-auto-insert-elisp-template)
(define-auto-insert "\\.html$" #'oo-auto-insert-html-template)
(define-auto-insert "\\.py$"   #'oo-auto-insert-python-file-header)
(define-auto-insert "\\.sh$"   #'oo-auto-insert-bash-file-header)
(define-auto-insert "\\.hy$"   #'oo-auto-insert-hy-file-header)
(define-auto-insert "\\.org$"  #'oo-auto-insert-org-file-header)
;;; provide
(provide '130-init-auto-insert)
;;; 130-init-auto-insert.el ends here
