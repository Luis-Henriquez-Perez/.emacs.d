;;; 128-init.el -*- lexical-binding: t; -*-
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
;; This file loads everything that needs to be evaluated immediately on startup.
;;
;;; Code:
(require '050-base)
(require 'server)

;; https://christiantietze.de/posts/2023/09/kill-unsaved-buffer-ux-action-labels/
(defun! oo--prompt-clearly (_ buffer &rest _)
  "Ask user in the minibuffer whether to save before killing.
Replace `kill-buffer--possibly-save' as advice."
  (set! prompt (format "Buffer %s modified." (buffer-name)))
  (set! choices '((?s "Save and kill buffer" "save the buffer and then kill it")
                  (?d "Discard and kill buffer without saving" "kill buffer without saving")
                  (?c "Cancel" "Exit without doing anything")))
  (set! long-form (and (not use-short-answers) (not (use-dialog-box-p))))
  (set! response (car (read-multiple-choice prompt (reverse choices) nil nil long-form)))
  (cl-case response
    (?s (with-current-buffer buffer (save-buffer)) t)
    (?d t)
    (t nil)))

(advice-add 'kill-buffer--possibly-save :around #'oo--prompt-clearly)

(advice-add 'save-buffer :around #'oo-funcall-quietly)

(defun oo-completion-in-region-function (&rest args)
  (apply (if (and (bound-and-true-p vertico-mode) (featurep 'consult))
             #'consult-completion-in-region
           #'completion--in-region)
         args))

(setq completion-in-region-function #'oo-completion-in-region-function)
;;;; disable old themes before enabling new ones
;; We end up with remants of the faces of old themes when we load a new
;; one.  For this reason, I make sure to disable any enabled themes before applying
;; a new theme.

;; When you load a theme you'll end up with quite a surprise.  And it
;; stacks as well when working on a big configuration change I didn't
;; have this code and I could literally backtrack the themes.

;; Don't know why deleting the previous theme before enabling a new
;; one isn't the default behavior.  When would anyone want to layer
;; the colors of one theme on top of an older one.
(defun! oo--disable-old-themes (orig-fn &rest args)
  "Disable old themes before loading new ones."
  (mapc #'disable-theme custom-enabled-themes)
  (apply orig-fn args))

(advice-add 'load-theme :around #'oo--disable-old-themes)
;;;; make setting faces actually work
;; Surprisingly, the function `custom-theme-set-faces' and `custom-set-faces' do
;; not by default actually change any faces.  For that to happen the variable
;; `custom--inhibit-theme-enable' needs to be nil.  Furthermore, because I
;; disable existing themes before enabling new ones even after customizing a
;; theme the customization does not persist.  This function addresses both of
;; these issues ensuring that as expected the faces are set immediately if the
;; theme is loaded and that these changes persist even after theme change.
(defvar oo-custom-faces-alist nil
  "An alist of faces to be applied.
Each element is of the form (theme . faces).  THEME is the customized theme and
FACES is the list of customized faces for THEME.")

(defun oo-custom-set-faces (theme &rest faces)
  "Customize THEME with FACES.
If THEME is already enabled, also applies
faces immediately."
  (declare (indent defun))
  (when (and after-init-time
             (or (equal theme 'user) (member theme custom-enabled-themes)))
    (let ((custom--inhibit-theme-enable nil))
      (apply #'custom-theme-set-faces theme faces)))
  (setf (alist-get theme oo-custom-faces-alist)
        (cl-union faces (alist-get theme oo-custom-faces-alist) :key #'car)))

(defun oo-apply-custom-faces-h (current)
  "Apply any faces that need to be applied from `oo-custom-faces-alist'."
  (oo-log 'info "Current theme -> %s" current)
  (for! ((theme . faces) oo-custom-faces-alist)
    (when (or (equal theme 'user) (member theme custom-enabled-themes))
      (oo-log 'info "Applying faces for %s..." theme)
      (let ((custom--inhibit-theme-enable nil))
        (apply #'custom-theme-set-faces theme faces)))))

(add-hook 'enable-theme-functions #'oo-apply-custom-faces-h)
;;;; Prevent *Messages* and *scratch* buffers from being killed
;; "Locking" a file can mean two different things (or both of these things at
;; once).  It can mean that Emacs cannot be exited while there are "locked"
;; buffers; it can also mean that the locked buffers cannot be killed (e.g. via
;; [[file:snapshots/_helpful_command__kill-buffer_.png][kill-buffer]]).  I don't think I ever want the former behavior.  Setting
;; [[][emacs-default-locking-mode]] to kill tells Emacs just to prevent buffers
;; with =emacs-lock-mode= enabled from being killed.  If you were to try to kill
;; one with something like =kill-buffer=, it would fail and you'd get a message
;; saying the buffer cannot be killed.

;; The =*Messages*= buffer could contain important information and should never
;; really be killed. See [[https://www.emacswiki.org/emacs/ProtectingBuffers][ProtectingBuffers]].
(require 'emacs-lock)
(setq emacs-lock-default-locking-mode 'kill)
(with-current-buffer "*Messages*" (emacs-lock-mode 1))
;;;; autoload commands
(autoload! oo-open-emacs-config "989-commands")
(autoload! oo-open-emacs-init-file "989-commands")
(autoload! oo-open-emacs-lisp-dir "989-commands")
(autoload! oo-split-window-below-and-focus "989-commands")
(autoload! oo-split-window-right-and-focus "989-commands")
(autoload! oo-set-font-face "989-commands")
(autoload! oo-dwim-space "989-commands")
(autoload! oo-dwim-narrow "989-commands")
(autoload! oo-pop-to-buffer "989-commands")
(autoload! oo-dwim-align "989-commands")
(autoload! oo-kill-emacs-no-confirm "989-commands")
(autoload! oo-new-buffer "989-commands")
(autoload! oo-load-random-theme "989-commands")
(autoload! oo-sort-dwim "989-commands")
(autoload! oo-startup-time-table "989-commands")
(autoload! oo-one-line "989-commands")
(autoload! oo-remove-consequtive-spaces "989-commands")
(autoload! oo-open-emacs-lisp-dir "989-commands")
(autoload! oo-dwim-vc-action "989-commands")
;;;; xref
(opt! xref-search-program (if (executable-find "rg") 'ripgrep xref-search-program))
;; Select from xref candidates in minibuffer
(opt! xref-show-definitions-function #'xref-show-definitions-completing-read)
(opt! xref-show-xrefs-function #'xref-show-definitions-completing-read)
;;;; Give info buffers better names
;; This is taken from the package plk.
(defhook! info-rename-buffer-h (Info-selection-hook)
  "Rename current Info buffer to match its visiting manual."
  (unless (eq major-mode 'Info-mode) (user-error "This is not an Info buffer"))
  (unless (not (string-match-p "^\\*info" (buffer-name)))
    (rename-buffer (if (equal Info-current-file "dir") "*info*"
                     (format "*info %s*" (file-name-base Info-current-file)))
                   'unique)))
;;; provide
(provide '128-init)
;;; 128-init.el ends here
