;;; config-grugru.el --- grugru configuration -*- lexical-binding: t; -*-
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
;; This is my configuration for grugru.
;;
;;; Code:
(require 'grugru)

(grugru-define-global 'symbol (grugru-metagenerator-keep-case '("big" "medium" "small")))

(grugru-define-global 'symbol (grugru-metagenerator-keep-case '("word" "sentence" "paragraph")))

(grugru-define-global 'symbol (grugru-metagenerator-keep-case '("always" "never")))

(grugru-define-global 'symbol (grugru-metagenerator-keep-case '("open" "close")))

(grugru-define-global 'symbol (grugru-metagenerator-keep-case '("yes" "no")))

(grugru-define-global 'symbol (grugru-metagenerator-keep-case '("up" "down")))

(grugru-define-global 'symbol (grugru-metagenerator-keep-case '("left" "right")))

(grugru-define-global 'symbol (grugru-metagenerator-keep-case '("wrong" "right")))

(grugru-define-global 'symbol (grugru-metagenerator-keep-case '("red" "orange" "yellow" "green" "blue" "indigo" "violet")))

(grugru-define-global 'symbol (grugru-metagenerator-keep-case '("front" "back")))

(grugru-define-global 'symbol (grugru-metagenerator-keep-case '("inner" "outer")))

(grugru-define-global 'symbol (grugru-metagenerator-keep-case '("is" "was")))
;;;; emacs-lisp
(grugru-define-on-major-mode 'emacs-lisp-mode 'symbol '("unless" "when"))

(grugru-define-on-major-mode 'emacs-lisp-mode 'symbol '("pop-to-buffer" "display-buffer"))

(grugru-define-on-major-mode 'emacs-lisp-mode 'symbol '("let" "let*" "let!" "-let"))

(grugru-define-on-major-mode 'emacs-lisp-mode 'symbol '("define-key" "evil-define-key*"))

(grugru-define-on-major-mode 'emacs-lisp-mode 'symbol '("abbrev-table-put" "abbrev-table-get"))

(grugru-define-on-major-mode 'emacs-lisp-mode 'symbol '("gethash" "puthash"))

(grugru-define-on-major-mode 'emacs-lisp-mode 'symbol '("get-register" "set-register"))

(grugru-define-on-major-mode 'c-mode 'symbol '("unsigned" "signed"))
;;; provide
(provide 'config-grugru)
;;; config-grugru.el ends here
