;;; 036-keybinding-macros.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; These are keybinding sugars to help me define bindings.
;;
;;; Code:
(require '032-after-load-functions)
(require '035-base-macros)

(defvar evil-inner-text-objects-map)
(defvar evil-outer-text-objects-map)
(declare-function evil-define-key* "evil")

;; (defun oo-wrap-forms ()
;;   ""
;;   )

;; (defmacro wrap! (wrappers &rest body)
;;   )

;; (defmacro evil-binding-generate (&rest bindings)
;;   "Generate evil keybinding macros.
;; Each element in BINDINGS should be a list of the form:
;;   (NAME STATES DOCSTRING)
;; where
;;   NAME is a symbol like n, i, nv, e,
;;   STATES is a symbol or list of symbols for evil states,
;;   DOCSTRING is a string describing the macro."
;;   `(progn
;;      ,@(cl-loop for (name states docstring) in bindings
;;                 for macro-name = (intern (format "%smap" name))
;;                 collect
;;                 `(defmacro! ,macro-name (&rest args)
;;                    ,docstring
;;                    (set! (key def) (last args 2))
;;                    (set! keymap (if (nth 2 args) (car args) 'global-map))
;;                    (set! states ',(if (listp states) states (list states)))
;;                    `(afterfeature! evil
;;                       (afterbound! ,keymap
;;                         ,(cl-once-only (key)
;;                            `(progn
;;                               (setq ,key (if (vectorp ,key) ,key (kbd ,key)))
;;                               (evil-define-key* ',states
;;                                 ,keymap ,key ,def)))))))))

;; (evil-binding-generate n i v nv e)

;; (evil-binding-generate
;;  (n  normal  "Define evil keybinding in normal state.")
;;  (i  insert  "Define evil keybinding in insert state.")
;;  (v  visual  "Define evil keybinding in visual state.")
;;  (nv (normal visual) "Define evil keybinding in normal and visual state.")
;;  (e  emacs   "Define evil keybinding in Emacs state."))

(defmacro! nmap (&rest args)
  "Define evil keybinding in normal state."
  (set! (key def) (last args 2))
  (set! keymap (if (nth 2 args) (car args) 'global-map))
  `(afterfeature! evil
     (afterbound! ,keymap
       ,(cl-once-only (key)
          `(progn
             (setq ,key (if (vectorp ,key) ,key (kbd ,key)))
             (evil-define-key* 'normal ,keymap ,key ,def))))))

(defmacro! imap (&rest args)
  "Define evil keybinding in insert state."
  (set! (key def) (last args 2))
  (set! keymap (if (nth 2 args) (car args) 'global-map))
  `(afterfeature! evil
     (afterbound! ,keymap
       ,(cl-once-only (key)
          `(progn
             (setq ,key (if (vectorp ,key) ,key (kbd ,key)))
             (evil-define-key* 'insert ,keymap ,key ,def))))))

(defmacro! vmap (&rest args)
  "Define evil keybinding in visual state."
  (set! (key def) (last args 2))
  (set! keymap (if (nth 2 args) (car args) 'global-map))
  `(afterfeature! evil
     (afterbound! ,keymap
       ,(cl-once-only (key)
          `(progn
             (setq ,key (if (vectorp ,key) ,key (kbd ,key)))
             (evil-define-key* 'visual ,keymap ,key ,def))))))

(defmacro! nvmap (&rest args)
  "Define evil keybinding in normal and visual state."
  (set! (key def) (last args 2))
  (set! keymap (if (nth 2 args) (car args) 'global-map))
  `(afterfeature! evil
     (afterbound! ,keymap
       ,(cl-once-only (key)
          `(progn
             (setq ,key (if (vectorp ,key) ,key (kbd ,key)))
             (evil-define-key* '(normal visual) ,keymap ,key ,def))))))

(defmacro! emap (&rest args)
  "Define evil keybinding in Emacs state."
  (set! (key def) (last args 2))
  (set! keymap (if (nth 2 args) (car args) 'global-map))
  `(afterfeature! evil
     (afterbound! ,keymap
       ,(cl-once-only (key)
          `(progn
             (setq ,key (if (vectorp ,key) ,key (kbd ,key)))
             (evil-define-key* 'emacs ,keymap ,key ,def))))))

(defmacro! iotmap (key inner outer)
  "Define evil keybinding in Emacs state.
Inner is the definition of the key in `evil-inner'.  Outer is the definition
in."
  `(afterfeature! evil
     ,(cl-once-only (key)
        `(progn
           (setq ,key (if (vectorp ,key) ,key (kbd ,key)))
           (keymap-set evil-inner-text-objects-map ,key ,inner)
           (keymap-set evil-outer-text-objects-map ,key ,outer)))))

(defmacro! llmap (&rest args)
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
(provide '036-keybinding-macros)
;;; 036-keybinding-macros.el ends here
