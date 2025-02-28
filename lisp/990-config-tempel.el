;;; 990-config-tempel.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; TODO: add commentary
;;
;;; Code:
(require 'tempel)

(defmacro deftempel! (name &rest body)
  "Define a tempel template."
  (declare (doc-string 2) (indent defun))
  (setq documentation (when (stringp (car body)) (list (pop body))))
  `(progn (defun ,name ()
            ,@documentation
            (interactive)
            (tempel-insert ',body)
            t)
          (put ',name 'no-self-insert t)
          ',name))

(deftempel! oo-expand-elisp-defhook
  "Expand to a `defun' form."
  "(defhook! " p " (" p ")" n>
  "\"" p "\"" n>
  p ")" n>)

(deftempel! oo-expand-elisp-oo-add-hook
  "Expand to a `oo-add-hook' form."
  "(oo-add-hook " p " " p ")")

(deftempel! oo-expand-elisp-let*
  "Expand to a `' form."
  "(let* (" p ")" n> p ")")

(deftempel! oo-expand-elisp-hook-bang
  "Expand to a `oo-add-hook' form."
  "(hook! " p " " p ")")

(deftempel! oo-expand-elisp-cond
  "Expand to a `cond' form."
  "(cond " ")")

(deftempel! oo-expand-elisp-defvar
  "Expand to `defmacro'."
  "(defun " p " (" p ")" n> "\"" p "\"" n> r ")")

(deftempel! oo-expand-to-defun
  "Expand to `defun'."
  "(defun " p " (" p ")" n> "\"" p "\"" n> r ")")

(deftempel! oo-expand-elisp-command
  "Expand to command."
  "(defun " p " (" p ")\n  \"" p "\"" n> "(interactive" p ")" n> r> ")")

(deftempel! oo-expand-elisp-defvar
  "Expand to `defvar'."
  "(defvar " p "\s" p "\n  \"" p "\"" ")")

(deftempel! oo-expand-elisp-message
  "Expand to `message'."
  "(message " \" p "\"" p ")")
;;; provide
(provide '990-config-tempel)
;;; 990-config-tempel.el ends here
