;;; init-org.el --- initialize org -*- lexical-binding: t; -*-
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
(require 'base)
;;;; general
(opt! org-directory (expand-file-name "~/Documents/org/"))
(opt! org-default-notes-file (expand-file-name "notes.org" org-directory))
(opt! org-agenda-files (directory-files org-directory t "\\.org\\'"))
(opt! org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "ON-HOLD(h)"
                                    "BLOCKED(b)" "COOLDOWN(o)" "|" "DONE(d)"
                                    "CANCELLED(c)")))
(opt! org-src-fontify-natively t)
(opt! org-hide-emphasis-markers t)
(opt! org-log-done 'note)
(opt! org-priority-lowest ?F)
(opt! org-priority-highest ?A)
(opt! org-default-priority ?D)
(opt! org-enforce-todo-dependencies t)
(opt! org-tags-column 0)
(opt! org-archive-location (alet! (expand-file-name "archive.org" org-directory)
                             (format "%s::" it)))
(opt! org-archive-mark-done t)
(opt! org-global-properties `(("Effort_ALL" .
                               ,(string-join (mapcar (apply-partially #'format "0:%.2d")
                                                     (number-sequence 5 55 5))
                                             "\s"))))
(opt! org-ellipsis " ▼")
;;;; org-agenda
(autoload #'+org-agenda-day-view "config-org-agenda" nil t nil)
;;;; org-capture
(autoload #'+org-capture-plain "config-org-capture" nil t 'function)
(autoload #'+org-capture-todo "config-org-capture" nil t 'function)
(autoload #'+org-capture-open "config-org-capture" nil t 'function)
(autoload #'+org-capture-question "config-org-capture" nil t 'function)
(autoload #'+org-capture-bug "config-org-capture" nil t 'function)
(autoload #'+org-capture-choose-template "config-org-capture" nil t 'function)














(alt! org-capture +org-capture-choose-template org-capture)
;;;; org-refile
(defun +org-directory-files ()
  "Return a list of org files."
  (directory-files org-directory t "\\.org\\'"))

(opt! org-refile-allow-creating-parent-nodes t)
;; The variable =org-refile-targets= specifies the places from which information
;; is taken to create the list of possible refile targets.  So, for example,
(opt! org-refile-targets '((+org-directory-files :maxlevel . 10)))
(opt! org-outline-path-complete-in-steps nil)
(opt! org-refile-use-cache nil)
;; Without this setting, you can't actually refile to a generic file with
;; refiling; you can only refile to existing headings within that file.  The way
;; I use refiling, I'm refiling to files most of the time.
(opt! org-refile-use-outline-path 'file)
;; Although it is possible to have a parent headline that also has a source
;; block, I prefer not to.  I guess it is a stylistic thing.
;; TODO: Fix `oo-has-source-block-p' is not defined.
;; (opt! org-refile-target-verify-function (lambda () (not (oo-has-src-block-p))))
;;;; org-src
(oo-popup-at-bottom "\\*Org Src")
(opt! org-edit-src-persistent-message nil)
;; (adjoin! org-src-lang-modes '("emacs-lisp" . emacs-lisp))
;; (adjoin! org-src-lang-modes '("lua" . lua))
(opt! org-src-ask-before-returning-to-edit-buffer nil)
(opt! org-src-preserve-indentation t)
(opt! org-edit-src-content-indentation 0)
(opt! org-src-window-setup 'plain)
;;;; org-clock
;; TODO: do not load org-clock on `org-mode-hook'.
(hook! org-mode-hook org-clock-persistence-insinuate)
(opt! org-clock-persist t)
(opt! org-clock-sound (expand-file-name "~/Downloads/ding-101492.wav"))
;;;; org-id
(opt! org-id-track-globally t)
(opt! org-id-locations-file (expand-file-name "org-id-locations" oo-var-dir))

;; The way I see it, if I can have a universally unique identifier that also tells
;; me the date my headline was created; we hit two birds with one stone.  That way I
;; never need a =date-created= property.
(opt! org-id-method 'ts)

(opt! org-id-link-to-org-use-id t)
;;;; org-timer
(opt! org-timer-default-timer "0:05:00")
;;;; org-appear
;;; provide
(provide 'init-org)
;;; init-org.el ends here
