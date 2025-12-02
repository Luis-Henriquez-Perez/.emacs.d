;;; init-auto-insert.el --- Initialize auto-insert -*- lexical-binding: t; -*-
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
(require 'base)

(opt! auto-insert-query nil)

(add-hook 'o-first-file-hook #'auto-insert-mode)

(autoload 'o-auto-insert--pound-comment-header "lib-auto-insert" nil nil 'function)
(autoload 'o-auto-insert-elisp-template        "lib-auto-insert" nil nil 'function)
(autoload 'o-auto-insert-python-file-header    "lib-auto-insert" nil nil 'function)
(autoload 'o-auto-insert-html-template         "lib-auto-insert" nil nil 'function)
(autoload 'o-auto-insert-hy-file-header        "lib-auto-insert" nil nil 'function)
(autoload 'o-auto-insert-bash-file-header      "lib-auto-insert" nil nil 'function)
(autoload 'o-auto-insert-org-file-header       "lib-auto-insert" nil nil 'function)
(autoload 'o-auto-insert-script-file-header    "lib-auto-insert" nil nil 'function)

(define-auto-insert "\\.\\(?:service\\|timer\\)$" #'o-auto-insert--pound-comment-header)
(define-auto-insert (concat (regexp-quote (expand-file-name "~/.local/bin/")) "[^./]+$") #'o-auto-insert-script-file-header)
(define-auto-insert "\\.el$"   #'o-auto-insert-elisp-template)
(define-auto-insert "\\.html$" #'o-auto-insert-html-template)
(define-auto-insert "\\.py$"   #'o-auto-insert-python-file-header)
(define-auto-insert "\\.sh$"   #'o-auto-insert-bash-file-header)
(define-auto-insert "\\.hy$"   #'o-auto-insert-hy-file-header)
(define-auto-insert "\\.org$"  #'o-auto-insert-org-file-header)
;;; provide
(provide 'init-auto-insert)
;;; init-auto-insert.el ends here
