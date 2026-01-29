;;; init-pkg-abbrev.el --- initialize abbrev-mode -*- lexical-binding: t; -*-
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
(require 'init-core)

(declare-function o-abbrev-in-text-p "lib-abbrev")
(declare-function o-advice--abbrev-insert-period-maybe "lib-abbrev")
(declare-function o-advice--abbrev-pulse-expand "lib-abbrev")
(declare-function o-advice--abbrev-ensure-post-insert "lib-abbrev")
(declare-function o-advice--abbrev-write-abbrev-file "lib-abbrev")

(autoload 'o-abbrev-in-text-p "lib-abbrev" nil nil 'function)
(autoload 'o-advice--abbrev-insert-period-maybe "lib-abbrev" nil nil 'function)
(autoload 'o-advice--abbrev-pulse-expand "lib-abbrev" nil nil 'function)
(autoload 'o-advice--abbrev-ensure-post-insert "lib-abbrev" nil nil 'function)
(autoload 'o-advice--abbrev-write-abbrev-file "lib-abbrev" nil nil 'function)

;; Write abbrevs to files my way
(setq save-abbrevs 'silently)

(add-hook 'prog-mode-hook #'abbrev-mode)
(add-hook 'text-mode-hook #'abbrev-mode)

;; Prevent greedy expansion with `backward-word'
(abbrev-table-put global-abbrev-table :regexp "\\<\\(\\sw+\\)\\Sw*")
;; PARENT TABLES
(abbrev-table-put text-mode-abbrev-table :enable-function  #'o-abbrev-in-text-p)
(abbrev-table-put global-abbrev-table :parents (list text-mode-abbrev-table emacs-lisp-mode-abbrev-table))

;; These do not need to be autoloaded because they will only ever happen when
;; abbrev-mode is already enabled.
;; (advice-add 'abbrev--default-expand :around #'o-advice--abbrev-insert-period-maybe)
(advice-add 'abbrev--default-expand :around #'o-advice--abbrev-pulse-expand)
(advice-add 'abbrev--default-expand :around #'o-advice--abbrev-ensure-post-insert)
(advice-add 'write-abbrev-file :around #'o-advice--abbrev-write-abbrev-file)

;; Do not read the abbrev files at startup because I already load them myself.
;; Emacs loads abbrevs so fast.
(advice-add 'read-abbrev-file :around #'ignore)
(advice-add 'quietly-read-abbrev-file :around #'ignore)
;;; provide
(provide 'init-pkg-abbrev)
;;; init-pkg-abbrev.el ends here
