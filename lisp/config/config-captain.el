;;; config-captain.el --- captain configuration. -*- lexical-binding: t; -*-
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
;; This is my configuration for captain.
;;
;;; Code:
;; Man, this is more involved than I thought it would be at first because
;; capitalization rules are different depending on whether your in prog-mode or
;; not and whether you are in a doc-string, or comment, or just a normal string.
(require 'base)
(require 'captain)
(require 'rx)
(require 'lispy)
;;;; determine where I am
;; TODO: generalize regexp with `defun!', `cl-defun', etc.
(defvar oo-docstring-regexp "(\\(?:def\\(?:advice!\\|hook!\\|macro\\|un!?\\)\\)[[:blank:]]\\([^[:space:]]+\\)[[:blank:]](\\(.*\\))\n[[:blank:]]*\"")

;; TODO: generalize this regexp for comments in different languages.
(defun oo--beg-comment-block-rx ()
  "Return a regular expression that matches the beginning of a comment block."
  (rx-to-string
   `(: (or bos
           ;; comments with more
           (: bol (>= 3 ,comment-start) (0+ any) eol "\n")
           ;; blank lines
           (: bol eol "\n")
           (: bol (not ,comment-start) (* any) eol "\n"))
       (: bol (zero-or-more blank) (= 2 ,comment-start) blank))))

(defvar oo--definer-list '("defun"
						   "defmacro"
						   "cl-defun"
						   "cl-defmacro"
						   "defun*"
						   "defmacro*"
						   "lambda"
						   "defmacro!"
						   "defun!"
						   "-lambda"))

;; Influenced from smartparens.  This does it for emacs-lisp but I wonder if
;; there is a general way to determine.
(defun! oo--in-elisp-docstring-p ()
  "Return the bounds of docstring."
  (alet! (bounds-of-thing-at-point 'string)
    (and (derived-mode-p 'emacs-lisp-mode)
         (save-excursion
	       (goto-char (car it))
	       (ignore-errors (backward-sexp 3))
	       (looking-at-p (regexp-opt oo--definer-list)))
         it)))

(defun! +captain--prog-mode-sentence-start ()
  "Return point where sentence should be capitalized."
  (pcase (oo-in-string-or-comment-p)
    ('comment
     ;; For now use `lispy--bounds-comment' because I do not think there is a
     ;; built-in alternative.
     (set! beg (car (lispy--bounds-comment)))
     ;; The reason I go forwared one character is that I could be at the first
     ;; word of the sentence.  I am doubtful this method is perfect but I could
     ;; not think of a better way yet.
     (save-excursion (goto-char (1+ (point)))
                     (backward-sentence)
                     (goto-char (max (point) beg))
                     (when (looking-at comment-start-skip)
                       (goto-char (match-end 0)))
                     (point)))
    ('string
     (aand! (car (oo--in-elisp-docstring-p))
    	   (max it (or (car (bounds-of-thing-at-point 'sentence)) it))))))
;;; provide
(provide 'config-captain)
;;; config-captain.el ends here
