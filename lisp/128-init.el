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
(require! "^0[01]")

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

(advice-add 'save-buffer :around #'oo-call-quietly-a)

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
(defun! oo-disable-old-themes-a (orig-fn &rest args)
  "Disable old themes before loading new ones."
  (mapc #'disable-theme custom-enabled-themes)
  (apply orig-fn args))

(advice-add 'load-theme :around #'oo-disable-old-themes-a)
;;;; Do not short-circuit themes with invalid box property
;; This comes from the fact that :box (:style unspecified ...) was silently
;; tolerated before, but in Emacs 30 the :style slot must be either nil,
;; 'released-button, or 'pressed-button.
(defun! oo-set-face-attribute-a (orig-fn face frame &rest args)
  "Remove :box properties from attributes."
  (condition-case nil
      (apply orig-fn face frame args)
    (error
     ;; Remove invalid :box properties from ATTRS.
     (flet! sanitize-attrs (attrs)
       (let ((plist (copy-sequence attrs)))
         (when-let* ((box (plist-get plist :box)))
           (when (and (listp box)
                      (eq (plist-get box :style) 'unspecified))
             ;; Drop the :style element entirely
             (setq box (plist-put (copy-sequence box) :style nil))
             (setq plist (plist-put plist :box box))))
         plist))
     (apply orig-fn face frame (sanitize-attrs args)))))

(advice-add 'set-face-attribute :around #'oo-set-face-attribute-a)
;;;; Only consider real themes
;; Annoyingly some themes, such spacemacs, solarized, moe and
;; immaterial, have helper files for defining themes that Emacs wrongly confuses
;; as actual themes.  When you select it as a theme you get a jarring "undefined
;; custom theme" error.  This advice filters `custom-available-themes' so I only
;; see actual themes and not these "fake" ones.

;; At first I tried to implementation this by using a cache but after that
;; implementation I realized that this was overkill.  There are only three
;; themes I use with this issue so I can just as easily hard-code it.  The
;; benefit implementation-wise is it is simple, fast, and does not have
;; the potential performance cost of the cache solution which would need to read
;; all the theme files the first time its called.
(defun! oo-only-real-themes-a (themes)
  "Do not count \"fake\" themes."
  (cl-set-difference themes '(light-blue spacemacs solarized moe immaterial)))

(advice-add 'custom-available-themes :filter-return #'oo-only-real-themes-a)
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
(autoload 'oo-open-emacs-config "989-commands" nil nil 'function)
(autoload 'oo-open-emacs-init-file "989-commands" nil nil 'function)
(autoload 'oo-open-emacs-lisp-dir "989-commands" nil nil 'function)
(autoload 'oo-split-window-below-and-focus "989-commands" nil nil 'function)
(autoload 'oo-split-window-right-and-focus "989-commands" nil nil 'function)
(autoload 'oo-set-font-face "989-commands" nil nil 'function)
(autoload 'oo-dwim-space "989-commands" nil nil 'function)
(autoload 'oo-dwim-narrow "989-commands" nil nil 'function)
(autoload 'oo-pop-to-buffer "989-commands" nil nil 'function)
(autoload 'oo-dwim-align "989-commands" nil nil 'function)
(autoload 'oo-kill-emacs-no-confirm "989-commands" nil nil 'function)
(autoload 'oo-new-buffer "989-commands" nil nil 'function)
(autoload 'oo-load-random-theme "989-commands" nil nil 'function)
(autoload 'oo-sort-dwim "989-commands" nil nil 'function)
(autoload 'oo-startup-time-table "989-commands" nil nil 'function)
(autoload 'oo-one-line "989-commands" nil nil 'function)
(autoload 'oo-remove-consequtive-spaces "989-commands" nil nil 'function)
(autoload 'oo-open-emacs-lisp-dir "989-commands" nil nil 'function)
(autoload 'oo-dwim-vc-action "989-commands" nil nil 'function)
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
(add-hook 'sh-mode-hook #'aggressive-indent-mode)
;; (after! smartparens (lambda () (sp-local-pair 'sh-mode "'")))
;;;; vc
;; Use text-mode tools when editing.
(add-hook 'vc-git-log-edit-mode-hook #'abbrev-mode)

(autoload 'captain-mode "captain" nil nil 'function)
(add-hook 'vc-git-log-edit-mode-hook #'captain-mode)
;;;; make certain files read-only
(autoload 'git-gutter-mode "git-gutter" nil nil 'function)

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
  (set! theme oo-init-theme)
  (cond ((member "--random-theme" command-line-args)
         (load-theme (seq-random-elt (custom-available-themes)) :no-confirm))
        ((not theme))
        ((member theme (custom-available-themes))
         (load-theme theme :no-confirm))
        (t
         (oo-log 'info "Theme %s not found." theme))))

(add-hook 'after-init-hook #'oo-load-theme-maybe-h 90)

;; This makes opening sh files way too slow.  These are simple files, it should
;; not be slow.
;; On second thought, this is what is responsible for ensuring the file is
;; indented properly so I cannot just outright ignore it.
;; (advice-add 'sh-set-shell :override  #'ignore)

;; Several packages such as org, eshell, and magit take a while to load in
;; session.  This attempts to mitigate that by loading features gradually during
;; idle time.  The idea is that by the time one of these big features is loaded,
;; many of its subfeatures will have been loaded thereby reducing the load time.
(defvar oo-idle-features nil
  "List of features to load during idle time.")

(defun oo-load-idle-features ()
  "Load one feature from `oo-idle-features' during idle time."
  (awhen! (pop oo-idle-features)
    (oo-log 'info "Idle loading: %s" it)
    (require it nil 'noerror)
    (run-with-idle-timer 1 nil #'oo-load-idle-features)))

(defun oo-setup-idle-loading-h ()
  "Setup the loading of idle features."
  (run-with-idle-timer 3 nil #'oo-load-idle-features))

(add-hook 'emacs-startup-hook #'oo-setup-idle-loading-h 90)
;;; provide
(provide '128-init)
;;; 128-init.el ends here
