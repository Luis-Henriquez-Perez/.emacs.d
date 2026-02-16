;;; init-after-tempel.el --- Configure tempel -*- lexical-binding: t; -*-
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
;; Configure tempel.
;;
;;; Code:
(require 'init-core)

(defvar o-tempel-global-templates nil
  "My global templates.")

(defvar-local o-tempel-local-templates nil
  "Buffer-local templates.")

(add-to-list 'tempel-template-sources 'o-tempel-global-templates)
(add-to-list 'tempel-template-sources 'o-tempel-local-templates)

(defun o-tempel-elisp-expand-let* ()
  "Expand to a `let' form."
  (interactive)
  (tempel-insert '("(let* (" p ")" n> r ")"))
  t)
(put 'o-tempel-elisp-expand-let* 'no-self-insert t)

(defun o-tempel-elisp-expand-defun ()
  "Expand to a `defun' form."
  (interactive)
  (tempel-insert '("(defun " p " (" p ")" n> "\"" p "\"" n> r ")"))
  t)
(put 'o-tempel-elisp-expand-defun 'no-self-insert t)

(defun o-tempel-elisp-expand-cond ()
  "Expand to a `cond' form."
  (interactive)
  (tempel-insert '("(cond " ")"))
  t)
(put 'o-tempel-elisp-expand-cond 'no-self-insert t)

(defun o-tempel-expand-elisp-command ()
  "Expand to command."
  (interactive)
  (list "(defun " p " (" p ")\n  \"" p "\"" n> "(interactive" p ")" n> r> ")"))

(defun o-tempel-elisp-expand-defvar ()
  "Expand to `defvar'."
  (interactive)
  (tempel-insert '("(defvar " p "\s" p "\n  \"" q "\"" ")")))
(put 'o-tempel-elisp-expand-defvar 'no-self-insert t)

(defun o-tempel-expand-elisp-message ()
  "Expand to `message'."
  (interactive)
  (list "(message \"" r  "\")"))
(put 'o-tempel-expand-elisp-message 'no-self-insert t)

(defun o-tempel-expand-elisp-message-var
    "Expand to printing a variable value with `message'."
  "(message \"" (s var)  " -> %S\" " var ")" q)

(defun o-tempel-expand-elisp-with-current-buffer
    "Expand to printing a variable value with `message'."
  "(with-current-buffer " p n> r ")")

(defun o-tempel-expand-elisp-setq
    "Expand to printing a variable value with `message'."
  "(setq " p "\s" r ")")

(defun o-tempel-expand-elisp-setq-bang
  "Expand to printing a variable value with `message'."
  "(o-set " p "\s" r ")")

(defun o-in-html-p ()
  (member major-mode '(mhtml-mode web-mode)))

(defun o-tempel-expand-html-elisp-source-block
    "Expand to source block."
  > "<div class=\"org-src-container\">" n
  > "<pre>" n
  > "<code class=\"elisp\">" n
  > r n
  > "</code>" n
  > "</pre>" n
  > "</div>" n)

(defun o-tempel-expand-html-bold
  "Expand to html bold tag"
  "<b>" r "</b>")
;;; provide
(provide 'init-after-tempel)
;;; init-after-tempel.el ends here
