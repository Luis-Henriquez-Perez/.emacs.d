;;; 990-snippets.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
(require! "^0[01]")
(require 'tempel)

(deftempel! oo-expand-elisp-defhook
  "Expand to a `defun' form."
  "(defhook! " p " (" p ")" n> "\"" p "\"" n> r ")")

(deftempel! oo-expand-elisp-oo-add-hook
  "Expand to a `oo-add-hook' form."
  "(oo-add-hook " p " " p ")")

(deftempel! oo-expand-elisp-let*
  "Expand to a `let' form."
  "(let* (" p ")" n> r ")")

(deftempel! oo-expand-elisp-hook-bang
  "Expand to a `oo-add-hook' form."
  "(hook! " p " " r ")")

(deftempel! oo-expand-elisp-cond
  "Expand to a `cond' form."
  "(cond " ")")

(deftempel! oo-expand-elisp-defun
  "Expand to `defun'."
  "(defun " p " (" p ")" n> "\"" p "\"" n> r ")")

(deftempel! oo-expand-elisp-defun-bang
  "Expand to `defun'."
  "(defun! " p " (" p ")" n> "\"" p "\"" n> r ")")

(deftempel! oo-expand-elisp-command
  "Expand to command."
  "(defun " p " (" p ")\n  \"" p "\"" n> "(interactive" p ")" n> r> ")")

(deftempel! oo-expand-elisp-defvar
  "Expand to `defvar'."
  "(defvar " p "\s" p "\n  \"" q "\"" ")")

(deftempel! oo-expand-elisp-message
  "Expand to `message'."
  "(message \"" r  "\")")

(deftempel! oo-expand-elisp-message-var
  "Expand to printing a variable value with `message'."
  "(message \"" (s var)  " -> %S\" " var ")" q)

;; (deftempel! oo-expand-elisp-re-search-forward
;;   "Expand to `message'."
;;   "(rsf \"" p  "\")")

(deftempel! oo-expand-elisp-with-current-buffer
  "Expand to printing a variable value with `message'."
  "(with-current-buffer " p n> r ")")

(deftempel! oo-expand-elisp-setq
  "Expand to printing a variable value with `message'."
  "(setq " p "\s" r ")")

(deftempel! oo-expand-elisp-setq-bang
  "Expand to printing a variable value with `message'."
  "(set! " p "\s" r ")")

(defun oo-in-html-p ()
  (member major-mode '(mhtml-mode web-mode)))

(deftempel! oo-expand-html-elisp-source-block
  "Expand to source block."
  > "<div class=\"org-src-container\">" n
  > "<pre>" n
  > "<code class=\"elisp\">" n
  > r n
  > "</code>" n
  > "</pre>" n
  > "</div>" n)

(deftempel! oo-expand-html-bold
  "Expand to html bold tag"
  "<b>" r "</b>")
;;; provide
(provide '990-snippets)
;;; 990-snippets.el ends here
