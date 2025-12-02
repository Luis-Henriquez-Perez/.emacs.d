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
(require '016-base-macros)

(declare-function evil-define-key* "evil")

(defmacro! generate-evil-keybinders! (&rest specs)
  "Generate evil keybinding macros based on SPECS."
  ;; The top-level `defmacro!' registers the key, def, and keymap variables from
  ;; within the backquoted form.  I do not think completely understand why these
  ;; variables are considered not used (and unusually not `states'?) but this
  ;; fixes it.
  (ignore key def keymap)
  (set! alist '((?g . global)
                (?n . normal)
                (?v . visual)
                (?i . insert)
                (?e . emacs)
                (?m . motion)
                (?o . operator)))
  (flet! to-text (symbols)
    ;; "Convert a symbol or list of symbols SYMBOLS to a natural language string."
    (let ((items (mapcar #'symbol-name (ensure-list symbols))))
      (pcase items
        (`() "")
        (`(,only) only)
        (`(,first ,second) (format "%s and %s" first second))
        (_ (let ((all-but-last (butlast items))
                 (last (car (last items))))
             (format "%s, and %s" (string-join all-but-last ", ") last))))))
  (flet! state-name (char)
    (alist-get char alist nil nil #'char-equal))
  (flet! split-spec (spec)
    (mapcar #'state-name (string-to-list (symbol-name spec))))
  `(progn
     ,@(collect! (spec specs)
         (set! states (split-spec spec))
         (set! docstring (format "Define an evil keybinding in %s state." (to-text states)))
         (set! macroname (intern (concat (symbol-name spec) "map!")))
         `(defmacro! ,macroname (&rest args)
            ,docstring
            (set! (key def) (last args 2))
            (set! keymap (if (nth 2 args) (car args) 'global-map))
            (set! states ',states)
            `(progn (defvar ,keymap)
                    ;; Stop byte-compilation warnings for functions I bind.
                    ,@(pcase def
                        (`(function ,fn)
                         `((declare-function ,fn nil))))
                    (o-bind-key ',keymap ,key ,def ',states))))))

(generate-evil-keybinders! n i v nv ni eg g)

(defalias 'emap! 'egmap!)

(defmacro! defvar-keymap! (keymap &rest pairs)
  "Wrapper around `defvar-keymap'.
In contrast to `defvar-keymap' this macro declares to avoid byte-compilation
warnings.  Also it auto defines a prefix with the same name as KEYMAP."
  (declare (indent 1))
  (set! plist (stripplist! pairs))
  (for! ((_ def) pairs :by #'cddr)
    (pcase def
      (`(function ,fn)
       (collecting! declareforms `(declare-function ,fn nil)))))
  `(progn ,@declareforms
          (defvar-keymap ,keymap
            :prefix ',keymap
            ,@plist
            ,@pairs)))

(defmacro! iotmap! (key inner outer)
  "Define evil keybindings for text object map.
INNER and OUTER are the key definitions for `evil-inner-text-objects-map' and
`evil-outer-text-objects-map' respectively."
  (cl-once-only (key inner outer)
    `(progn
       (defvar evil-inner-text-objects-map)
       (defvar evil-outer-text-objects-map)
       (o-bind-key 'evil-inner-text-objects-map ,key ,inner)
       (o-bind-key 'evil-outer-text-objects-map ,key ,outer))))

(defmacro! llmap (&rest args)
  "Define localleader key."
  (set! (key def) (last args 2))
  (set! keymap (if (nth 2 args) (car args) 'global-map))
  (flet! lkey (leader key)
    `(alet! ,key (if (vectorp it) it (concat ,leader "\s" it))))
  (flet! bind (leader state)
    `(o-bind-key ',keymap ,(lkey leader key) ,def ',state))
  `(progn (defvar ,keymap)
          ,(bind 'o-emacs-localleader-key 'global)
          ,(bind 'o-normal-localleader-key 'normal)
          ,(bind 'o-normal-localleader-short-key 'normal)
          ,(bind 'o-insert-localleader-key 'insert)
          ,(bind 'o-insert-localleader-short-key 'insert)
          ,(bind 'o-emacs-localleader-key 'emacs)))
;;; provide
(provide 'macros-keybinding)
;;; macros-keybinding.el ends here
