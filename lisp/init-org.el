;;; 130-init-org.el --- initialize org -*- lexical-binding: t; -*-
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
(require! "^0[01]")

(each! '(ol org-element-ast org-macs org-agenda org-refile org-src org-id org-clock org-timer org-capture org-compat calendar find-func format-spec thingatpt
            org-keys oc org-table org-fold org-cycle)
  (push it oo-idle-features))

(autoload '+org-agenda-day-view "990-config-org-agenda" nil nil 'function)

(llmap org-mode-map "a" #'org-archive-subtree)
(llmap org-mode-map "n" #'org-add-note)
(llmap org-mode-map "t" #'org-todo)

(nmap! org-mode-map "T" #'org-todo)
(nmap! org-mode-map "t" #'+org-choose-tags)
(nmap! org-mode-map "R" #'org-refile)
(nmap! org-mode-map "n" #'org-add-note)

(oo-require-after-load 'org '990-config-org)
;;; provide
(provide '130-init-org)
;;; 130-init-org.el ends here
