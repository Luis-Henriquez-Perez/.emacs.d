;;; 990-config-org.el --- Configure nil -*- lexical-binding: t; -*-
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
;; Configure nil.
;;
;;; Code:
(require! "^0[01]")
;;;; SETTINGS
;;;;; GENERAL
(setq org-directory (expand-file-name "~/Documents/org/"))
(setq org-default-notes-file (expand-file-name "notes.org" org-directory))
(setq org-agenda-files (directory-files org-directory t "\\.org\\'"))
(setq org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "ON-HOLD(h)"
                                    "BLOCKED(b)" "COOLDOWN(o)" "|" "DONE(d)"
                                    "CANCELLED(c)")))
(setq org-src-fontify-natively t)
(setq org-hide-emphasis-markers t)
(setq org-log-done 'note)
(setq org-priority-lowest ?F)
(setq org-priority-highest ?A)
(setq org-default-priority ?D)
(setq org-enforce-todo-dependencies t)
(setq org-tags-column 0)
(setq org-archive-location (alet! (expand-file-name "archive.org" org-directory)
                             (format "%s::" it)))
(setq org-archive-mark-done t)
(setq org-global-properties `(("Effort_ALL" .
                               ,(string-join (mapcar (apply-partially #'format "0:%.2d")
                                                     (number-sequence 5 55 5))
                                             "\s"))))
(setq org-ellipsis " ▼")
(setq org-log-done 'time)
;; https://emacs.stackexchange.com/questions/57677/how-to-set-effort-all-globally
(add-to-list 'org-modules 'org-habit :append)
;; (string-join (--map (format "0:%.2d" it) (number-sequence 5 55 5)) "\s")
;;;;; ORG-SRC
(oo-popup-at-bottom "\\*Org Src")
(opt! org-edit-src-persistent-message nil)
;; (adjoin! org-src-lang-modes '("emacs-lisp" . emacs-lisp))
;; (adjoin! org-src-lang-modes '("lua" . lua))
(opt! org-src-ask-before-returning-to-edit-buffer nil)
(opt! org-src-preserve-indentation t)
(opt! org-edit-src-content-indentation 0)
(opt! org-src-window-setup 'plain)
;;;;; ORG-CAPTURE
(autoload 'org-capture|plain "990-config-org-capture" nil nil 'function)
(autoload 'org-capture|todo "990-config-org-capture" nil nil 'function)
(autoload 'org-capture|open "990-config-org-capture" nil nil 'function)
(autoload 'org-capture|question "990-config-org-capture" nil nil 'function)
(autoload 'org-capture|bug "990-config-org-capture" nil nil 'function)
(autoload 'org-capture|choose-template "990-config-org-capture" nil nil 'function)

(alt! org-capture org-capture|choose-template org-capture)
;;;;; ORG-REFILE
(setq org-refile-allow-creating-parent-nodes t)
;; The variable =org-refile-targets= specifies the places from which information
;; is taken to create the list of possible refile targets.  So, for example,
(defun org|refile-targets ()
  "Return all org files in `org-directory'."
  (directory-files org-directory t "\\.org\\'"))

(setq org-refile-targets '((org|refile-targets :maxlevel . 10)))
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-use-cache nil)
;; Without this setting, you can't actually refile to a generic file with
;; refiling; you can only refile to existing headings within that file.  The way
;; I use refiling, I'm refiling to files most of the time.
(setq org-refile-use-outline-path 'file)
;; Although it is possible to have a parent headline that also has a source
;; block, I prefer not to.  I guess it is a stylistic thing.
;; TODO: Fix `oo-has-source-block-p' is not defined.
;; (opt! org-refile-target-verify-function (lambda () (not (oo-has-src-block-p))))
;;;;; ORG-CLOCK
;; TODO: do not load org-clock on `org-mode-hook'.
(add-hook 'org-mode-hook #'org-clock-persistence-insinuate)
(setq org-clock-persist t)
(setq org-clock-sound (expand-file-name "~/Downloads/ding-101492.wav"))
;;;;; ORG-ID
(setq org-id-track-globally t)
(setq org-id-locations-file (expand-file-name "org-id-locations" oo-cache-dir))

;; The way I see it, if I can have a universally unique identifier that also tells
;; me the date my headline was created; we hit two birds with one stone.  That way I
;; never need a =date-created= property.
(setq org-id-method 'ts)

(setq org-id-link-to-org-use-id t)
;;;;; ORG-TIMER
(opt! org-timer-default-timer "0:05:00")
;;;; COMMANDS
(defun! org|choose-tags ()
  "Choose tags to add to current headline.
If you choose a tag that is already in the current headline, remove it.  Any
tags that are not in the current headline are added to it.  The Resulting tags
are in alphabetical order."
  (interactive)
  (set! all (save-restriction (widen) (org-get-buffer-tags)))
  (set! current (mapcar #'substring-no-properties (org-get-tags (point) t)))
  (set! selected (completing-read-multiple "Choose tag: " all))
  (set! new (append (cl-set-difference selected current :test #'equal)
                    (cl-set-difference current selected :test #'equal)))
  (org-set-tags (sort (cl-remove-duplicates new :test #'equal) #'string<)))

(defun! org|alphabetize-tags ()
  "Alphabetize tags in current buffer."
  (interactive)
  (flet! fn (headline)
    (alet! (org-ml-get-property :tags headline)
      (org-ml-set-property :tags (sort it #'string<) headline)))
  (org-ml-do-headlines #'fn))

(defun! org|suppress-window-deletion (orig-fn &rest args)
  "Suppress window deletion."
  (nflet! delete-other-windows #'ignore)
  (apply orig-fn args))

(advice-add 'org-capture :around #'org|suppress-window-deletion)
;;; provide
(provide '990-config-org)
;;; 990-config-org.el ends here
