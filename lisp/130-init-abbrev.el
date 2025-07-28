;;; 130-init-abbrev.el --- initialize abbrev-mode -*- lexical-binding: t; -*-
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
;; Initialize abbrev-mode.
;;
;;; Code:
(require '050-base)
;;;; hooks
(add-hook 'prog-mode-hook #'abbrev-mode)
(add-hook 'text-mode-hook #'abbrev-mode)
(autoload! oo-load-abbrevs-h "990-config-abbrev")
(add-hook 'abbrev-mode-hook #'oo-load-abbrevs-h)
;;;; Do not read abbrev at startup
;; Do not read the abbrev files at startup because I already load them myself.
(advice-add 'read-abbrev-file :around #'ignore)
(advice-add 'quietly-read-abbrev-file :around #'ignore)
;;;; Write abbrevs to files my way
(opt! save-abbrevs 'silently)

(defun oo-write-abbrev-file-a (&rest _)
  "Override `write-abbrev-file' with my own function."
  (quiet! (oo-update-abbrev-tables))
  (oo-log 'trace "Updating abbrevs."))

(advice-add 'write-abbrev-file :around #'oo-write-abbrev-file-a)
;;;; setup advices
(autoload! oo--pulse-expansion "990-config-abbrev")
(autoload! oo--add-period-maybe "990-config-abbrev")
(autoload! oo--ensure-self-insert "990-config-abbrev")

(advice-add 'abbrev--default-expand :around #'oo--pulse-expansion)
(advice-add 'abbrev--default-expand :around #'oo--add-period-maybe)
(advice-add 'abbrev--default-expand :around #'oo--ensure-self-insert)
;;;; Setup abbrev tables
(abbrev-table-put text-mode-abbrev-table :enable-function  #'oo-in-text-p)
(abbrev-table-put global-abbrev-table :parents (list text-mode-abbrev-table emacs-lisp-mode-abbrev-table))
;;; provide
(provide '130-init-abbrev)
;;; 130-init-abbrev.el ends here
