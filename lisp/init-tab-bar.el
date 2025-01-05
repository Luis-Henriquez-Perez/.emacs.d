;;; init-tab-bar.el --- Initialize tab-bar -*- lexical-binding: t; -*-
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
;; Initialize tab-bar.
;;
;;; Code:
(defun oo-new-untitled-buffer ()
  "Return the new buffer."
  (generate-new-buffer "untitled"))

;; Control the initial buffer of a new tab.  In my case, make a new tab start
;; with an untitled buffer.
(opt! tab-bar-new-tab-choice #'oo-new-untitled-buffer)
;; Do not show the tabs.  I prefer to save as much screen real-estate as
;; possible.

;; I need to create some link between tab-bar and burly.  As in when I open a
;; bookmark in burly it should be in a new tab.
;; TODO:
(opt! tab-bar-show nil)

(defun +tab-bar-toggle-show ()
  "Toggle the tab bar display."
  (interactive)
  (opt! tab-bar-show (not tab-bar-show)))

;; https://mmk2410.org/2022/02/11/using-emacs-tab-bar-mode

;; TODO: set the initial buffer when a new tab is created.
;; TODO: save information from tab with `burly-bookmark-windows' so when I
;; restore a bookmark it creates a tab with the same name as it was.  I do not
;; know if `burly-tabs-mode' does the exact same thing.
(bind! oo-find-map "t" #'tab-switch)
;; (bind! oo-workspace-map "t" #'tab-select)
;; (bind! oo-workspace-map "n" #'tab-new)
;; (bind! oo-workspace-map "j" #'tab-next)
;; (bind! oo-workspace-map "k" #'tab-previous)
;;; provide
(provide 'init-tab-bar)
;;; init-tab-bar.el ends here
