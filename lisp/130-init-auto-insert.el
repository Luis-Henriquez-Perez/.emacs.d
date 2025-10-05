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
(require! "^0[01]")

(opt! auto-insert-query nil)

(add-hook 'oo-first-file-hook #'auto-insert-mode)

(autoload 'auto-insert||pound-comment-header "lib-auto-insert" nil nil 'function)
(autoload 'auto-insert|elisp-template        "lib-auto-insert" nil nil 'function)
(autoload 'auto-insert|python-file-header    "lib-auto-insert" nil nil 'function)
(autoload 'auto-insert|html-template         "lib-auto-insert" nil nil 'function)
(autoload 'auto-insert|hy-file-header        "lib-auto-insert" nil nil 'function)
(autoload 'auto-insert|bash-file-header      "lib-auto-insert" nil nil 'function)
(autoload 'auto-insert|org-file-header       "lib-auto-insert" nil nil 'function)
(autoload 'auto-insert|script-file-header    "lib-auto-insert" nil nil 'function)

(define-auto-insert "\\.\\(?:service\\|timer\\)$" #'auto-insert||pound-comment-header)
(define-auto-insert (concat (regexp-quote (expand-file-name "~/.local/bin/")) "[^./]+$") #'auto-insert|script-file-header)
(define-auto-insert "\\.el$"   #'auto-insert|elisp-template)
(define-auto-insert "\\.html$" #'auto-insert|html-template)
(define-auto-insert "\\.py$"   #'auto-insert|python-file-header)
(define-auto-insert "\\.sh$"   #'auto-insert|bash-file-header)
(define-auto-insert "\\.hy$"   #'auto-insert|hy-file-header)
(define-auto-insert "\\.org$"  #'auto-insert|org-file-header)
;;; provide
(provide '130-init-auto-insert)
;;; 130-init-auto-insert.el ends here
