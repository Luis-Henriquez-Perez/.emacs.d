;;; oo-workspace.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; This is my own workspace management system built on top of `burly' and
;; `tab-bar'.  As it stands, the aforementioned have the potential for workspace
;; management but they are not integrated together--or at least have a very
;; shallow integration in the form of `burly-tabs-mode'.  In this file I will
;; try to provide that integration.
;;
;; - Allow non-visible buffers to be a part of burly bookmarks.
;; Right now, burly only records the visible window buffers.
;;
;; - Create a default buffer that a new workspace with no buffers will start with.
;; This buffer will be an "untitled" buffer.
;;
;; - Use a `switch-to-buffer' variant that only considers the buffers in the workspace.
;; - Save workspaces at the end of the session.
;;
;; The tab-bar package has a lot of emphasis on the location of the tabs and
;; that is something that I do not care at all.  Why would I bother tracking
;; the location of these tabs?  Who cares?  I just want them to exist and to
;; jump to to them.  Why would I give myself a headache and manage their
;; locations?
;;; Code:
;;;; reqs
(require 'tab-bar)
(require 'burly)
;;;; commands
;; On a side-note I am thinking that I should associate tabs to their buffers
;; somehow and that I wished that burly could associate more buffers than what
;; are just visible in the frame.
;; TODO: prevent creating one if the a workspace of same name already exists.
(defun oo-workspace-create (name)
  "Create a new workspace."
  (interactive "sWorkspace name: ")
  (message "New workspace `%s'!" name)
  (burly-tabs-mode -1)
  (unless (bound-and-true-p tab-bar-mode)
	(require 'tab-bar)
	(tab-bar-mode 1))
  (tab-bar-new-tab)
  (tab-bar-rename-tab name)
  (burly-bookmark-windows name))

;; If name exists switch to it.
(defun oo-workspace-open (name)
  "Open a new workspace."
  (interactive (list (completing-read "Open Burly bookmark: " (burly-bookmark-names)
			                          nil nil burly-bookmark-prefix)))
  (require 'burly)
  (burly-tabs-mode -1)
  (unless (bound-and-true-p tab-bar-mode)
	(require 'tab-bar)
	(tab-bar-mode 1))
  (tab-bar-new-tab)
  (tab-bar-rename-tab name)
  (bookmark-jump name))

;; TODO: potentially kill the buffers corresponding to the workspace, maybe that
;; should be an option.
(defun oo-workspace-close ()
  ""
  (interactive)
  (burly-bookmark-windows name)
  (tab-bar-close)
  (dolist (buffer (workspace-buffers))
	(kill-buffer buffer)))
;;; provide
(provide 'oo-workspace)
;;; oo-workspace.el ends here
