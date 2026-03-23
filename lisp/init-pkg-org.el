;;; init-pkg-org.el --- initialize org -*- lexical-binding: t; -*-
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
;; Initialize org.
;;
;;; Code:
;;;; requirements
(require 'init-core)

(o-declare-package 'org)

(o-each '(ol org-element-ast org-macs org-agenda org-refile org-src org-id org-clock org-timer org-capture org-compat calendar find-func format-spec thingatpt
             org-keys oc org-table org-fold org-cycle)
  (push it o-idle-features))

(autoload 'o-org-agenda-day-view "init-after-org-agenda" nil nil 'function)

(o-defer-load-require 'org 'init-after-org)
;;; provide
(provide 'init-pkg-org)
;;; init-pkg-org.el ends here
