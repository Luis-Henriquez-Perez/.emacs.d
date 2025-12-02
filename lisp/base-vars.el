;;; base-vars.el --- core variables -*- lexical-binding: t; -*-
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
;;; Code:
(defconst o-lisp-dir (expand-file-name "lisp/" user-emacs-directory)
  "Directory where handcrafted configuration files go.")

(defconst o-local-dir (expand-file-name ".local/" user-emacs-directory)
  "Directory that stores subdirectories.")

(defconst o-etc-dir (expand-file-name "etc/" o-local-dir)
  "Directory where package configuration files go.")

(defvaralias 'o-data-dir 'o-etc-dir)

(defconst o-var-dir (expand-file-name "var/" o-local-dir)
  "Directory where persistent data files go.")

(defvaralias 'o-cache-dir 'o-var-dir)

(defvar o-custom-faces-alist nil
  "An alist that update the background of faces based on existing faces.

Each element is of the form (custom-face . built-in-face).  Whenever the theme
is changed CUSTOM-FACE updates its background and foreground similar to
built-in-face.  See `o--enable-theme-functions--set-state-faces-from-theme-h'.")

;; I need to process the `command-line-args' for font here so that I can set the
;; font before the frame is loaded.
(defvar o-init-theme nil
  "Initial theme.")

(defvar o-init-font nil
  "Initial font.")

;; Since it is inexpensive I set this to non-nil by default.
(defvar o-init-profile-p t
  "Non-nil if Emacs configuration should be profiled at startup.")

;; I set this to non-nil so that I can get a functional Emacs instance and can
;; debug errors.  If I want to see the specific error I can restart Emacs with
;; this enabled.
(defvar o-init-noerrors-p t
  "Non-nil if errors in init files should be ignored at startup.")

(defvar o-init-data nil
  "Initialization data.
This includes the time that features took to load.")

(defvar o-local-var-alist nil
  "An alist of (HOOK . VARIABLES-AND-VALUES).")
;;;; LEADERS
;; This file provides leaders keys for evil and non-evil states and it binds
;; these leader keys.

;; These leaders are specifically for evil mode states (not including insert and
;;                                                          Emacs).  I choose the space (=SPC=) key for evil leaders because it is one of if
;; not the easiest key to press because of its central placement on the keyboard
;; and its sheer size--at least on the [[https://en.wikipedia.org/wiki/QWERTY][qwerty]] keyboard that I use.  The choice
;; of =SPC m= for the major mode specific keys is simply for the pnemonic =m= which
;; stands for "major mode".  The short major mode prefix key =,= is for cases when I
;; want to shorten a key binding.  Although obviously not as easy to remember as
;; =m=, it provides me with one shorter keypress in certain situations.
(defconst o-normal-leader-key "SPC"
  "The evil leader prefix key.")

(defconst o-normal-localleader-key "SPC m"
  "The localleader prefix key for major-mode specific commands.")

(defconst o-normal-localleader-short-key ","
  "A shorter alternative `o-localleader-key'.")
;; These leaders are for evil insert and emacs states as well as vanilla
;; Emacs.  Note that evil Emacs state is different from vanilla Emacs.  One of the
;; goals with these bindings is to set up keybindings in the case that I disable
;; evil mode or in the case that I want to use my bindings in insert or Emacs
;; state--or even vanilla Emacs.  The choice behind the bindings is the same as
;; [[id:][before]], except I just prepended the =Meta= (a.k.a. the =Alt= key) to everything.
(defconst o-insert-leader-key "M-SPC"
  "The leader prefix key used for Insert state.")

(defconst o-insert-localleader-key "M-SPC m"
  "The localleader prefix key for major-mode specific commands.")

(defconst o-insert-localleader-short-key "M-,"
  "A short non-normal `o-localleader-key'.")

(defconst o-emacs-leader-key "C-c l"
  "The leader prefix key used for Emacs states.")

(defconst o-emacs-alt-leader-key "C-c SPC")

(defconst o-emacs-localleader-key "C-c l m"
  "The localleader prefix key for major-mode specific commands.")
;;; provide
(provide 'base-vars)
;;; base-vars.el ends here
