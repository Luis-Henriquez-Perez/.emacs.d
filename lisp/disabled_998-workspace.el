;;; 998-workspace.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; I wanted to utilize `burly' as well as the built-in `tab-bar-mode' for a
;; lightweight workspace management system.  An advan
;;
;;; Code:
(require '050-base)

;; Example of what workflow looks like.
;; 1. Create a workspace with `oo-workspace-create'.
;; 2. Save a snapshot of workspace with `oo-workspace-save'
;; 3. Switch to a new workspace naming it "foo".
;; 4. Mark one or more non-live buffers that you wish to save.

;; Tsb buffers globally available to each workspace and those that are only
;; specific to one workspace.
(defun oo-workspace--tab-name ()
  "Return name of current `tab-bar' tab or nil if none."
  (and (bound-and-true-p tab-bar-mode)
       (let* ((tabs (funcall tab-bar-tabs-function))
              (tab-number (1+ (tab-bar--current-tab-index tabs)))
              (tab-index (if (integerp tab-number)
                             (1- (max 0 (min tab-number (length tabs))))
                           (tab-bar--current-tab-index tabs)))
              (current-tab (nth tab-index tabs)))
         (alist-get 'name current-tab))))

;; I need to hook into `buffer-list' so the buffer list only shows me the
;; buffers in the current workspace.
(defun oo-workspace--ensure-tab-bar-mode ()
  "Ensure that `tab-bar-mode' is enabled."
  (unless (bound-and-true-p tab-bar-mode)
	(require 'tab-bar)
	(tab-bar-mode 1)))

;; This should start with a clean buffer list.
;; Should I have a function that associates a buffer with a workspace.  An if so.
(defun oo-workspace-create (name)
  "Create a new workspace."
  (interactive "sWorkspace name: ")
  (oo-workspace--ensure-tab-bar-mode)
  ;; Check whether there is a current workspace that exists with that name, if
  ;; there is just switch to that and open it.
  (tab-bar-new-tab)
  (tab-bar-rename-tab name)
  (message "New workspace `%s'!" name))

;; Ina function that saves the buffers in the current workspace.  Isbst
;; `burly-bookmark-windows' but it should not prompt me for the bookmark to save
;; the windows at.  Instead itsd by default at least save to the current
;; workspace (as specified by the tab).
(defun oo-workspace-save ()
  "Save the current workspace."
  (interactive)
  (oo-workspace--ensure-tab-bar-mode)
  ;; Check whether workspace has a name, if it does not prompt for one.
  ;; Check whether the workspace has already been saved.  Consequently, this
  ;; checks for whether there are any non-live buffers associated.
  (set! name (oo-workspace--tab-name))
  (set! record (list (cons 'url (burly-windows-url))
                     (cons 'handler #'oo-workspace-bookmark-handler)))
  (aif! (assoc 'nonlive (bookmark-get-bookmark-record name))
      (progn (cl-remove-if #'buffer-live-p (mapcar #'burly-buffer-url it))
             (bookmark-store name (cons it record) nil))
    (bookmark-store name record nil)))

(defun oo-workspace-open ()
  "Open an existing workspace."
  ;; Choose a new workspace to open.
  (awhen! (completing-read "Open workspace:" (burly-))
    (bookmark-jump it))
  ;; Check for open workspaces.
  ;; Only prompt for a workspace that has not been opened.  Or maybe prompt for
  ;; all of them and if it is an opened one just switch to it.
  )
;; This is just an alias for a tab-bar command.
(defun oo-workspace-switch ()
  "Switch to another workspace."
  ;; List all workspaces by name.
  (awhen! (completing-read "Switch to workspace:" (burly-))
    (bookmark-jump it))
  )

(defun! oo--workspace-add-buffer (buffer)
  "Add current buffer to workspace."
  ;; Make sure the buffer added is not one of the live buffers in the window configuration.
  (set! name (oo-workspace--tab-name))
  (set! existing-record (bookmark-get-bookmark-record "name"))
  (cond ((buffer-live-p buffer)
         (bookmark-store name workspace-record)
         ())
        (t
         (set! buffer-url (burly--bookmark-record-url (with-current-buffer buffer (bookmark-make-record))))
         (set! workspace-record (bookmark-get-bookmark-record "oo-workspace"))
         (pushing! (alist-get 'nonlive workspace-record) buffer-url)
         (bookmark-store name workspace-record)
         (bookmark-save)
         )))

(burly-windows-url)
;; => "emacs+burly+windows:?%28%28%28min-height%20.%204%29%20%28min-width%20.%2010%29%20%28min-height-ignore%20.%203%29%20%28min-width-ignore%20.%204%29%20%28min-height-safe%20.%201%29%20%28min-width-safe%20.%202%29%20%28min-pixel-height%20.%2092%29%20%28min-pixel-width%20.%20110%29%20%28min-pixel-height-ignore%20.%2069%29%20%28min-pixel-width-ignore%20.%2044%29%20%28min-pixel-height-safe%20.%2023%29%20%28min-pixel-width-safe%20.%2022%29%29%20leaf%20%28pixel-width%20.%201288%29%20%28pixel-height%20.%20767%29%20%28total-width%20.%20117%29%20%28total-height%20.%2033%29%20%28normal-height%20.%201.0%29%20%28normal-width%20.%201.0%29%20%28parameters%20%28burly-url%20.%20%22emacs%2Bburly%2Bbookmark%3A%2F%2Foo-workspace%3Ffilename%3D%2522~%252F.config%252Femacs%252Flisp%252F998-workspace.el%2522%26front-context-string%3D%2522%2529%255Cn%255Cn%2528defun%2520oo-wor%2522%26rear-context-string%3D%2522urly-windows-url%2522%26position%3D5102%26last-modified%3D%252826640%252045146%252073240%2520866000%2529%26defaults%3D%2528%2522oo-workspace%2522%2520%2522998-workspace.el%2522%2529%22%29%29%20%28buffer%20%22998-workspace.el%22%20%28selected%20.%20t%29%20%28hscroll%20.%200%29%20%28fringes%208%208%20nil%20nil%29%20%28margins%20nil%29%20%28scroll-bars%20nil%200%20t%20nil%200%20t%20nil%29%20%28vscroll%20.%200%29%20%28dedicated%29%20%28point%20.%205102%29%20%28start%20.%204849%29%29%20%28prev-buffers%20%28%22README.org%22%202385%204263%29%20%28%22998-workspace.el%22%201623%201623%29%20%28%22%2Ahelpful%20command%3A%20burly-bookmark-windows%2A%22%201078%201304%29%29%29"

(defun oo-workspace-add-buffer ()
  "Add current buffer to the workspace."
  (interactive)
  (oo--workspace-add-buffer (current-buffer)))

;; How are bookmarks set?  Can I change the bookmark set function (if any)?
;; Bookmarks use `bookmark-make-record-function', a local variable, to determine how to make a
;; bookmark record.

;; Save the non-live buffers of current tab.

(defun oo-workspace-bookmark-handler (bookmark)
  "Similar to `burly-bookmark-handler' but also handles non-live buffers."
  (burly-bookmark-handler bookmark)
  (dolist (bookmark (alist-get 'nonlive bookmark))
    (bookmark-jump bookmark #'ignore)))

;; I need to look into.

;; Burly is a program for managing visible buffers but I also want to associate
;; non-visible buffers into the workspace as well.  I have the general feeling
;; that I should have some data structure to manage this.  Right now there is no
;; strict enforcement.

;; I need a way to include non-visible buffers just like `perspective-mode'
;; does.  The function `burly-windows-url' can take care of storing the live
;; buffers but I need a way to open the non-live ones too.
;; (burly-windows-url)
;;; provide
(provide '998-workspace)
;;; 998-workspace.el ends here
