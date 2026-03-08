;;; init-after-grugru.el --- grugru configuration -*- lexical-binding: t; -*-
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

(grugru-define-global 'char (grugru-metagenerator-keep-case '("(" ")")))

(grugru-define-global 'char (grugru-metagenerator-keep-case '("{" "}")))

(grugru-define-global 'char (grugru-metagenerator-keep-case '("[" "]")))

(grugru-define-global 'char (grugru-metagenerator-keep-case '("<" ">")))

(grugru-define-global 'word (grugru-metagenerator-keep-case '("a" "an" "the")))

(grugru-define-global 'word (grugru-metagenerator-keep-case '("pop" "push")))

(grugru-define-global 'word (grugru-metagenerator-keep-case '("bottom" "top")))

(grugru-define-global 'word (grugru-metagenerator-keep-case '("activate" "deactivate")))

(grugru-define-global 'word (grugru-metagenerator-keep-case '("round" "angle" "curly" "square")))

(grugru-define-global 'word (grugru-metagenerator-keep-case '("inside" "around")))

(grugru-define-global 'word (grugru-metagenerator-keep-case '("expand" "contract")))

(grugru-define-global 'word (grugru-metagenerator-keep-case '("beg" "end")))

(grugru-define-global 'word (grugru-metagenerator-keep-case '("forward" "backward")))

(grugru-define-global 'word (grugru-metagenerator-keep-case '("past" "present" "future")))

(grugru-define-global 'word (grugru-metagenerator-keep-case '("prev" "next")))

(grugru-define-global 'word (grugru-metagenerator-keep-case '("word" "sentence" "paragraph")))

(grugru-define-global 'word (grugru-metagenerator-keep-case '("words" "sentences" "paragraphs")))

(grugru-define-global 'word (grugru-metagenerator-keep-case '("up" "down")))

(grugru-define-global 'word (grugru-metagenerator-keep-case '("pre" "post")))

(grugru-define-global 'word (grugru-metagenerator-keep-case '("north" "east" "west" "south")))

(grugru-define-global 'word (grugru-metagenerator-keep-case '("big" "medium" "small")))

(grugru-define-global 'word (grugru-metagenerator-keep-case '("word" "sentence" "paragraph")))

(grugru-define-global 'word (grugru-metagenerator-keep-case '("always" "never")))

(grugru-define-global 'word (grugru-metagenerator-keep-case '("open" "close")))

(grugru-define-global 'word (grugru-metagenerator-keep-case '("yes" "no")))

(grugru-define-global 'word (grugru-metagenerator-keep-case '("up" "down")))

(grugru-define-global 'word (grugru-metagenerator-keep-case '("left" "right")))

(grugru-define-global 'word (grugru-metagenerator-keep-case '("wrong" "right")))

(grugru-define-global 'word (grugru-metagenerator-keep-case '("red" "orange" "yellow" "green" "blue" "indigo" "violet")))

(grugru-define-global 'word (grugru-metagenerator-keep-case '("front" "back")))

(grugru-define-global 'word (grugru-metagenerator-keep-case '("inner" "outer")))

(grugru-define-global 'word (grugru-metagenerator-keep-case '("is" "was" "were")))
;;;; emacs-lisp
(grugru-define-on-major-mode 'emacs-lisp-mode 'char (grugru-metagenerator-keep-case '("'" "#'")))

(grugru-define-on-major-mode 'emacs-lisp-mode 'symbol '("defun" "defmacro"))

(grugru-define-on-major-mode 'emacs-lisp-mode 'symbol '("when-let" "when-let*"))

(grugru-define-on-major-mode 'emacs-lisp-mode 'symbol '("funcall" "apply"))

(grugru-define-on-major-mode 'emacs-lisp-mode 'symbol '("after-init-hook" "emacs-startup-hook"))

(grugru-define-on-major-mode 'emacs-lisp-mode 'symbol '("setq" "defvar"))

(grugru-define-on-major-mode 'emacs-lisp-mode 'symbol '("dolist" "pcase-dolist"))

(grugru-define-on-major-mode 'emacs-lisp-mode 'symbol '("car" "cdr"))

(grugru-define-on-major-mode 'emacs-lisp-mode 'symbol '("t" "nil"))

(grugru-define-on-major-mode 'emacs-lisp-mode 'word '("normal" "visual" "motion" "insert"))

(grugru-define-on-major-mode 'emacs-lisp-mode 'symbol '("keymap-set" "keymap-unset"))

(grugru-define-on-major-mode 'emacs-lisp-mode 'symbol '("unless" "when"))

(grugru-define-on-major-mode 'emacs-lisp-mode 'symbol '("pop-to-buffer" "display-buffer"))

(grugru-define-on-major-mode 'emacs-lisp-mode 'symbol '("let" "let*"))

(grugru-define-on-major-mode 'emacs-lisp-mode 'symbol '("define-key" "evil-define-key*"))

(grugru-define-on-major-mode 'emacs-lisp-mode 'symbol '("abbrev-table-put" "abbrev-table-get"))

(grugru-define-on-major-mode 'emacs-lisp-mode 'symbol '("gethash" "puthash"))

(grugru-define-on-major-mode 'emacs-lisp-mode 'symbol '("get-register" "set-register"))

(grugru-define-on-major-mode 'c-mode 'symbol '("unsigned" "signed"))
;;; provide
(provide 'init-after-grugru)
;;; init-after-grugru.el ends here
