;;; macros-keybinding.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; These are keybinding macros that help me define bindings cleanly and
;; concisely.
;;
;;; Code:
(require 'macros-autolet)
(require 'macros-loop)
(require 'macros-config-helpers)

(declare-function evil-define-key* "evil")

(o-defmacro o-defvar-keymap (keymap &rest pairs)
  "Wrapper around `defvar-keymap'.
In contrast to `defvar-keymap' this macro declares to avoid byte-compilation
warnings.  Also it auto defines a prefix with the same name as KEYMAP."
  (declare (indent 1))
  (o-set plist (o-stripplist pairs))
  (o-for ((_ def) pairs :by #'cddr)
    (pcase def
      (`(function ,fn)
       (o-collecting declareforms `(declare-function ,fn nil)))))
  `(progn ,@declareforms
          (defvar-keymap ,keymap
            :prefix ',keymap
            ,@plist
            ,@pairs)))
;;; provide
(provide 'macros-keybinding)
;;; macros-keybinding.el ends here
