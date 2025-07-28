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
(defun oo-load-abbrevs-h ()
  "Load abbrev files.
This function is designed to be added to `abbrev-mode-hook'.  It loads all my
abbrevs and removes itself from the hook."
  (require '910-text-mode-abbrev-table)
  (require '910-emacs-lisp-mode-abbrev-table)
  ;; Abbrevs are loaded at startup so to properly defer this I need to load my
  ;; configuration when abbrev-mode is enabled.
  (require '990-post-abbrev)
  (remove-hook 'abbrev-mode-hook #'oo-load-abbrevs-h))

(add-hook 'abbrev-mode-hook #'oo-load-abbrevs-h)

(add-hook 'prog-mode-hook #'abbrev-mode)
(add-hook 'text-mode-hook #'abbrev-mode)
;;;; Do not read abbrev at startup
;; Do not read the abbrev files at startup because I already load them myself.
(advice-add 'read-abbrev-file :around #'ignore)
(advice-add 'quietly-read-abbrev-file :around #'ignore)
;;;; Write abbrevs to files my way
(setq save-abbrevs 'silently)
;;; provide
(provide '130-init-abbrev)
;;; 130-init-abbrev.el ends here
