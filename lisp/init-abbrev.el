;;; init-abbrev.el --- initialize abbrev-mode -*- lexical-binding: t; -*-
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
(require! "^0[01]")

(declare-function oo-abbrev-in-text-p "lib-abbrev")
(declare-function oo-abbrev-insert-period-maybe-a "lib-abbrev")
(declare-function oo-abbrev-pulse-expand-a "lib-abbrev")
(declare-function oo-abbrev-ensure-post-insert-a "lib-abbrev")
(declare-function oo-abbrev-write-abbrev-file-a "lib-abbrev")

(autoload 'oo-abbrev-in-text-p "lib-abbrev" nil nil 'function)
(autoload 'oo-abbrev-insert-period-maybe-a "lib-abbrev" nil nil 'function)
(autoload 'oo-abbrev-pulse-expand-a "lib-abbrev" nil nil 'function)
(autoload 'oo-abbrev-ensure-post-insert-a "lib-abbrev" nil nil 'function)
(autoload 'oo-abbrev-write-abbrev-file-a "lib-abbrev" nil nil 'function)

;; Write abbrevs to files my way
(setq save-abbrevs 'silently)

(add-hook 'prog-mode-hook #'abbrev-mode)
(add-hook 'text-mode-hook #'abbrev-mode)

;; Prevent greedy expansion with `backward-word'
(abbrev-table-put global-abbrev-table :regexp "\\<\\(\\sw+\\)\\Sw*")
;; PARENT TABLES
(abbrev-table-put text-mode-abbrev-table :enable-function  #'oo-abbrev-in-text-p)
(abbrev-table-put global-abbrev-table :parents (list text-mode-abbrev-table emacs-lisp-mode-abbrev-table))

;; These do not need to be autoloaded because they will only ever happen when
;; abbrev-mode is already enabled.
(advice-add 'abbrev--default-expand :around #'oo-abbrev-insert-period-maybe-a)
(advice-add 'abbrev--default-expand :around #'oo-abbrev-pulse-expand-a)
(advice-add 'abbrev--default-expand :around #'oo-abbrev-ensure-post-insert-a)
(advice-add 'write-abbrev-file :around #'oo-abbrev-write-abbrev-file-a)

;; Do not read the abbrev files at startup because I already load them myself.
;; Emacs loads abbrevs so fast.
(advice-add 'read-abbrev-file :around #'ignore)
(advice-add 'quietly-read-abbrev-file :around #'ignore)
;;; provide
(provide 'init-abbrev)
;;; init-abbrev.el ends here
