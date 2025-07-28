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

(declare-function consult-completion-in-region "consult")
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
;;;; settings for unix
(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.timer\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.target\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.mount\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.automount\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.slice\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.socket\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.path\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.netdev\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.network\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.link\\'" . conf-unix-mode))
;;;; Give info buffers better names
;; This is taken from the package plk.
(defvar Info-current-file)

(declare-function Info-copy-current-node-name "info")

;; This is not enough, the name should also be updated whenever your at a new node.
(defhook! info-rename-buffer (Info-selection-hook)
  "Rename current Info buffer to match its visiting manual."
  (unless (eq major-mode 'Info-mode) (user-error "This is not an Info buffer"))
  (unless (not (string-match-p "^\\*info" (buffer-name)))
    (rename-buffer (if (equal Info-current-file "dir")
                       "*info*"
                     (format "*%S*" (Info-copy-current-node-name))
                     ;; (format "*info %s*" (file-name-base Info-current-file))
                     )
                   'unique)))
;;;; sh-mode
(hook! sh-mode-hook aggressive-indent-mode)
;; (after! smartparens (lambda () (sp-local-pair 'sh-mode "'")))
;;;; vc
;; Use text-mode tools when editing.
(add-hook 'vc-git-log-edit-mode-hook #'abbrev-mode)
(add-hook 'vc-git-log-edit-mode-hook #'captain-mode)
(add-hook 'vc-git-log-edit-mode-hook #'oo--enter-evil-insert-state-maybe 0)
;;;; make certain files read-only
(defun! oo-dwim-file-rules ()
  "Do special things depending on what file is opened.
If I open a file in my package directory, do it in `view-mode'.  If I open a
file that is in a git repo, enale git-gutter-mode."
  (flet! in-dir-p (apply-partially #'file-in-directory-p buffer-file-name))
  (flet! in-any-dir-p (&rest dirs) (seq-some #'in-dir-p (mapcar #'expand-file-name dirs)))
  (when buffer-file-name
    (when (in-any-dir-p "~/.config/emacs/elpa/" "~/Downloads/")
      (read-only-mode 1))
    ;; When in a git repo enable git-gutter-mode.
    (when (vc-root-dir)
      (git-gutter-mode 1)
      ;; If it is in anyone of my dotfile directories, enable auto-committing.
      ;; (when (in-any-dir-p "~/.config/awesome/" "~/.config/emacs/" "~/")
      ;;   (oo-auto-commit-mode 1))
      )))

;; Do not add this hook to `find-file-hook' immediately because anytime a file
;; is visited it will run this function.
;; Try to put this at the end.
(add-hook 'emacs-startup-hook (lambda () (add-hook 'find-file-hook #'oo-dwim-file-rules 90)))

(defun! oo-load-theme-maybe-h ()
  "Load theme."
  (set! theme oo-startup-theme)
  (message "Loading startup theme `%S'..." theme)
  (message "Theme in (custom-available-themes)" (member theme (custom-available-themes)))
  (when (and theme (member theme (custom-available-themes)))
    (load-theme theme :no-confirm)))

(add-hook 'after-init-hook #'oo-load-theme-maybe-h)
;;; provide
(provide '128-init)
;;; 128-init.el ends here
