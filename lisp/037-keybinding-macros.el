;;; 037-keybinding-macros.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
(require '032-after-load-functions)
(require '035-base-macros)
(require '036-base-functions)

(declare-function evil-define-key* "evil")

(defmacro! generate-evil-keybinders! (&rest specs)
  "Generate evil keybinding macros based on SPECS."
  ;; The top-level `defmacro!' registers the key, def, and keymap variables from
  ;; within the backquoted form.  I do not think completely understand why these
  ;; variables are considered not used (and unusually not `states'?) but this
  ;; fixes it.
  (ignore key def keymap)
  (set! alist '((?n . normal)
                (?v . visual)
                (?i . insert)
                (?e . emacs)
                (?m . motion)
                (?o . operator)))
  (flet! state-name (char)
    (alist-get char alist nil nil #'char-equal))
  (flet! split-spec (spec)
    (mapcar #'state-name (string-to-list (symbol-name spec))))
  `(progn
     ,@(accumulate! (spec specs)
         (set! states (split-spec spec))
         (set! docstring (format "Define an evil keybinding in %s state." (oo-symbols-to-text states)))
         (set! macroname (intern (concat (symbol-name spec) "map")))
         `(defmacro! ,macroname (&rest args)
            ,docstring
            (set! (key def) (last args 2))
            (set! keymap (if (nth 2 args) (car args) 'global-map))
            (set! states ',states)
            `(afterfeature! evil
               (afterbound! ,keymap
                 ,(cl-once-only (key)
                    `(progn
                       (setq ,key (if (vectorp ,key) ,key (kbd ,key)))
                       (evil-define-key* ',states ,keymap ,key ,def)))))))))

(generate-evil-keybinders! n i v nv ni)

;; Here I use Emacs state plus vannilla Emacs keybindings which is why I do not
;; define it with `evil-binding-generate'.
(defmacro! emap (&rest args)
  "Define evil keybinding in Emacs state as well as vanilla Emacs."
  (set! (key def) (last args 2))
  (set! keymap (if (nth 2 args) (car args) 'global-map))
  `(afterfeature! evil
     (afterbound! ,keymap
       ,(cl-once-only (key def)
          `(progn
             (setq ,key (if (vectorp ,key) ,key (kbd ,key)))
             (keymap-set ,keymap ,key ,def)
             (evil-define-key* 'emacs ,keymap ,key ,def))))))

(defmacro stripplist! (list)
  "Strip and return plist from the front of LIST.
LIST is a list symbol."
  (cl-with-gensyms (plist)
    `(let (,plist)
       (while (keywordp (car ,list))
         (prepending! ,plist (list (pop ,list) (pop ,list))))
       ,plist)))

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

(defmacro! iotmap (key inner outer)
  "Define evil keybindings for text object map.
INNER and OUTER are the key definitions for `evil-inner-text-objects-map' and
`evil-outer-text-objects-map' respectively."
  `(afterfeature! evil
     ,(cl-once-only (key)
        `(progn
           (defvar evil-inner-text-objects-map)
           (defvar evil-outer-text-objects-map)
           (setq ,key (if (vectorp ,key) ,key (kbd ,key)))
           (keymap-set evil-inner-text-objects-map ,key ,inner)
           (keymap-set evil-outer-text-objects-map ,key ,outer)))))

(defmacro! llmap (&rest args)
  "Define localleader key."
  (set! (key def) (last args 2))
  (set! keymap (if (nth 2 args) (car args) 'global-map))
  (flet! lkey (leader key)
    `(alet! ,key (if (vectorp it) it (kbd (concat ,leader "\s" it)))))
  (flet! ebind (leader state)
    `(evil-define-key* ',state ,keymap ,(lkey leader key) ,def))
  `(afterbound! ,keymap
     (keymap-set ,keymap (concat oo-emacs-localleader-key "\s" ,key) ,def)
     (afterfeature! evil
       ,(ebind 'oo-normal-localleader-key 'normal)
       ,(ebind 'oo-normal-localleader-short-key 'normal)
       ,(ebind 'oo-insert-localleader-key 'insert)
       ,(ebind 'oo-insert-localleader-short-key 'insert)
       ,(ebind 'oo-emacs-localleader-key 'emacs))))
;;; provide
(provide '037-keybinding-macros)
;;; 037-keybinding-macros.el ends here
