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
(require '035-base-macros)

(defvar evil-inner-text-objects-map)
(defvar evil-outer-text-objects-map)
(declare-function evil-define-key* "evil")

(defmacro! nmap (&rest args)
  "Define evil keybinding in normal state."
  (set! (key def) (last args 2))
  (set! keymap (if (nth 2 args) (car args) 'global-map))
  `(with-eval-after-load 'evil
     (if (boundp ',keymap)
         (evil-define-key* 'normal ,keymap ,key ,def)
       (push '(evil-define-key* 'normal ,keymap ,key ,def)
             (gethash symbol oo-after-load-hash-table)))))

(defmacro! imap (&rest args)
  "Define evil keybinding in insert state."
  (set! (key def) (last args 2))
  (set! keymap (if (nth 2 args) (car args) 'global-map))
  `(with-eval-after-load 'evil
     (if (boundp ',keymap)
         (evil-define-key* 'insert ,keymap ,key ,def)
       (push '(evil-define-key* 'insert ,keymap ,key ,def)
             (gethash symbol oo-after-load-hash-table)))))

(defmacro! nvmap (&rest args)
  "Define evil keybinding in normal and visual state."
  (set! (key def) (last args 2))
  (set! keymap (if (nth 2 args) (car args) 'global-map))
  `(with-eval-after-load 'evil
     (if (boundp ',keymap)
         (evil-define-key* '(normal visual) ,keymap ,key ,def)
       (push '(evil-define-key* '(normal visual) ,keymap ,key ,def)
              (gethash symbol oo-after-load-hash-table)))))

(defmacro! emap (&rest args)
  "Define evil keybinding in Emacs state."
  (set! (key def) (last args 2))
  (set! keymap (if (nth 2 args) (car args) 'global-map))
  (cl-with-gensyms (key def)
    `(with-eval-after-load 'evil
       (cond ((boundp ',keymap)
              (let ((key ,key)
                    (def ,def))
                (keymap-set ,keymap ,key ,def)
                (evil-define-key* 'emacs ,keymap ,key ,def)))
             (t
              (push '(progn (keymap-set ,keymap ,key ,def)
                            (evil-define-key* 'emacs ,keymap ,key ,def))
                     (gethash symbol oo-after-load-hash-table)))))))

(defmacro! iotmap (key inner outer)
  "Define evil keybinding in Emacs state."
  `(with-eval-after-load 'evil
     (evil-define-key* ' ,keymap ,key ,inner)
     (evil-define-key* ' ,keymap ,key ,outer)))

(defmacro! leadermap (key def)
  "Define evil keybinding in Emacs state."
  `(with-eval-after-load 'evil
     (evil-define-key* 'insert oo-leader-map ,key ,inner)
     (evil-define-key* 'insert ,keymap ,key ,outer)))

(defmacro! localleadermap (key def)
  "Define evil keybinding in Emacs state."
  (flet! leader (leader)
    (kbd (concat leader "\s" key)))
  (define-key keymap (leader oo-emacs-localleader-key) def)
  `(with-eval-after-load 'evil
     (let ()
       (evil-define-key* 'emacs ,keymap ,key ,def)
       (evil-define-key* 'normal ,keymap ,key ,def)
       (evil-define-key* 'normal ,keymap ,key ,def)
       (evil-define-key* 'insert ,keymap ,key ,def)
       (evil-define-key* 'insert ,keymap ,key ,def))))
;;; provide
(provide '036-keybinding-macros)
;;; 036-keybinding-macros.el ends here
