;;; base-extra.el --- Define and set several hooks -*- lexical-binding: t; -*-
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
;; Define and set several hooks.
;;
;;; Code:
(require 'base-lib)
(require 'server)
;;;; setup custom hooks
(defvar o-first-file-hook nil
  "Hook run after the first file is loaded.")

(defun o-run-first-file-hook-h (&rest _)
  "Run `o-first-file-hook'."
  (run-hooks 'o-first-file-hook)
  (remove-hook 'find-file-hook 'o-run-first-file-hook-h))

(add-hook 'find-file-hook #'o-run-first-file-hook-h)

(defvar o-first-input-hook nil
  "Hook run after the first file is loaded.")

(defun o-run-first-input-hook-h (&rest _)
  "Run `o-first-input-hook'."
  (run-hooks 'o-first-input-hook)
  (remove-hook 'pre-command-hook 'o-run-first-input-hook-h))

(add-hook 'pre-command-hook #'o-run-first-input-hook-h)
;;;; hooks
(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'prog-mode-hook #'auto-fill-mode)
(add-hook 'prog-mode-hook #'hs-minor-mode)
;; (unless noninteractive
;;   ;; Do not use this hook until I fix the flyspell dict message that is
;;   ;; displayed.  Also flyspell is slow to start.
;;   ;; (add-hook 'prog-mode-hook #'flyspell-prog-mode)
;;   )
(add-hook 'text-mode-hook #'visual-line-mode)
;; (unless noninteractive
;;   (add-hook 'text-mode-hook #'flyspell-mode))
(add-hook 'after-init-hook #'window-divider-mode 12)
(add-hook 'o-first-input-hook #'minibuffer-depth-indicate-mode)

(defun o-setup-eval-after-bound-forms-h ()
  "Call `o-call-after-load-functions' once.
Also add it as a hook to `after-load-functions' so that it is invoked whenever a
file is loaded."
  (o-eval-after-bound-forms)
  (add-hook 'after-load-functions #'o-eval-after-bound-forms))

(add-hook 'after-init-hook #'o-setup-eval-after-bound-forms-h 99)
;;;; auto-filling
(o-setq-mode-local prog-mode normal-auto-fill-function #'o-progn-autofill-fn)

(defun o-progn-autofill-fn ()
  "Fill only if in a string or comment."
  (when (o-in-string-or-comment-p) (do-auto-fill)))
;;;; emacs-lisp-mode specific
(defun o-extend-elisp-font-lock-h ()
  "Add custom font-lock keywords."
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(o-def\\(?:macro\\|un\\)\\)\\_>\\s-*\\(\\(?:\\w\\|\\s_\\)+\\)?"
      (1 font-lock-keyword-face nil t)
      (2 font-lock-function-name-face nil t))
     ("\\_<\\(\\(?:it\\|this-fn\\)\\)\\_>"
      (1 font-lock-constant-face nil t)))))

;; This needs to be done before emacs-lisp-mode is enabled.  Otherwise it will
;; not work the first time (though it will work every subsequent time the mode
;; is enabled).  So I do not know, this could be a before advice to
;; emacs-lisp-mode perhaps ensuring it's run beforehand.  Even if I need to do
;; this in startup hook, it is not performance intensive.
(add-hook 'emacs-startup-hook #'o-extend-elisp-font-lock-h)

(defun o-require-base-h ()
  "Load base macros."
  (require 'base)
  (remove-hook 'emacs-lisp-mode-hook #'o-require-base-h))

(add-hook 'emacs-lisp-mode-hook #'o-require-base-h)
;;;; garbage collection
;; https://www.reddit.com/r/emacs/comments/yzb77m/an_easy_trick_i_found_to_improve_emacs_startup/
(defun o-increase-garbage-collection-h ()
  "Boost garbage collection settings to `gcmh-high-cons-threshold'."
  (set-register :gc-cons-threshold gc-cons-threshold)
  (set-register :gc-cons-percentage gc-cons-percentage)
  (setq gc-cons-threshold (* 32 1024 1024))
  (setq gc-cons-percentage 0.8))

(add-hook 'minibuffer-setup-hook #'o-increase-garbage-collection-h 10)

(defun o-decrease-garbage-collection-h ()
  "Reset garbage collection settings to `gcmh-low-cons-threshold'."
  (setq gc-cons-threshold (get-register :gc-cons-threshold))
  (setq gc-cons-percentage (get-register :gc-cons-percentage)))

(add-hook 'minibuffer-exit-hook #'o-decrease-garbage-collection-h 90)

(o-defun o--timer--lower-gc ()
  "Lower garbage collection until it reaches default values."
  (o-flet mb (x) (/ (float x) 1024 1024))
  (if (minibuffer-window-active-p (minibuffer-window))
      (run-with-timer 5 nil #'o--timer--lower-gc)
    (o-log 'trace "Running timer for lowering garbage collection...")
    (o-set reduction (/ (get-register :gc-cons-threshold) 10))
    (o-set gc-floor (* 8 1024 1024))
    (o-set gcp-default 0.2)
    (when (/= gc-cons-threshold gc-floor)
      (o-set old gc-cons-threshold)
      (o-set new (max (- old reduction) gc-floor))
      (setq gc-cons-threshold new)
      (o-log 'trace "Lower `gc-cons-threshold' from %.2f to %.2f MB..." (mb old) (mb new)))
    (when (/= gc-cons-percentage gcp-default)
      (o-set old (max gc-cons-percentage gcp-default))
      (o-set new (max (- gc-cons-percentage 0.1) gcp-default))
      (o-log 'trace "Lower `gc-cons-percentage' from %.1f to %.1f..." old new)
      (setq gc-cons-percentage new))
    (if (and (= gc-cons-threshold gc-floor)
             (= gc-cons-percentage gcp-default))
        (o-log 'trace "Done with timer.")
      (run-with-timer 7 nil #'o--timer--lower-gc))))

(defun o-restore-startup-values-h ()
  "Restore the values of `file-name-handler-alist' and `gc-cons-threshold'."
  (o-log 'trace "Restore the value of `file-name-handler-alist'.")
  (setq file-name-handler-alist (get-register :file-name-handler-alist))
  (setq gc-cons-threshold (* 40 1024 1024))
  (set-register :gc-cons-threshold gc-cons-threshold)
  (o-log 'trace "Set the value of `gc-cons-threshold' to 40 MB.")
  (run-with-timer 5 nil #'o--timer--lower-gc))

(add-hook 'emacs-startup-hook #'o-restore-startup-values-h 90)
;;;; trailing whitespace
(defun o-delete-trailing-whitespace-at-line-h ()
  "Delete the trailing whitespace in the buffer except for the current line.
Also if there is more than one trailing space in the current line, replace them
with a single space."
  (delete-trailing-whitespace (point-min) (line-beginning-position))
  (save-match-data
    (when (looking-back "^.*?\\(?1:[[:space:]]\\{2,\\}\\)$" (line-beginning-position))
      (replace-match "\s" nil nil nil 1)))
  (delete-trailing-whitespace (line-end-position) (point-max)))

(defun o-setup-delete-whitespace-h ()
  "Show trailing whitespace and delete it before saving."
  (setq-local show-trailing-whitespace t)
  (add-hook 'before-save-hook #'o-delete-trailing-whitespace-at-line-h nil 'local)
  (add-hook 'kill-buffer-hook #'delete-trailing-whitespace nil 'local))

(add-hook 'conf-mode-hook #'o-setup-delete-whitespace-h)
(add-hook 'prog-mode-hook #'o-setup-delete-whitespace-h)
(add-hook 'text-mode-hook #'o-setup-delete-whitespace-h)
;;;; startup time
(defun o-record-after-init-hook-start-time-h ()
  "Record the start of `after-init-hook'."
  :depth -100
  (o-log 'info "Running `after-init-hook'...")
  (set-register :after-init-start (float-time)))

(add-hook 'after-init-hook #'o-record-after-init-hook-start-time-h)

(o-defun o-record-after-init-hook-end-time-h ()
  "Record the end of `after-init-hook'."
  (o-set start (get-register :after-init-start))
  (o-set time (o-hundredths (- (float-time) start)))
  (set-register :after-init-hook-time time)
  (o-log 'success "Finished running `after-init-hook' in %.2f seconds" time))

(add-hook 'after-init-hook #'o-record-after-init-hook-end-time-h 100)

(defun o-record-emacs-startup-hook-start-time-h ()
  "Record the start of `emacs-startup-hook'."
  (o-log 'info "Running `emacs-startup-hook'...")
  (set-register :emacs-startup-start (float-time)))

(add-hook 'emacs-startup-hook #'o-record-emacs-startup-hook-start-time-h -100)

(o-defun o-record-emacs-startup-hook-end-time-h ()
  "Record the end of `emacs-startup-hook'."
  (o-set start (get-register :emacs-startup-start))
  (o-set time (o-hundredths (- (float-time) start)))
  (set-register :emacs-startup-hook-time time)
  (o-log 'success "Finished running `emacs-startup-hook' in %.2f seconds" time))

(add-hook 'emacs-startup-hook #'o-record-emacs-startup-hook-end-time-h 100)

(unless noninteractive
  (autoload 'highlight-indent-guides-mode "highlight-indent-guides-mode" nil nil 'function)
  (add-hook 'mhtml-mode-hook #'highlight-indent-guides-mode))

(o-opt highlight-indent-guides-method 'character)

(add-hook 'text-mode-hook #'delete-selection-mode)
(add-hook 'prog-mode-hook #'delete-selection-mode)
;;;; make setting faces actually work
;; Surprisingly, the function `custom-theme-set-faces' and `custom-set-faces' do
;; not by default actually change any faces.  For that to happen the variable
;; `custom--inhibit-theme-enable' needs to be nil.  Furthermore, because I
;; disable existing themes before enabling new ones even after customizing a
;; theme the customization does not persist.  The following hook is to add
;; basic.  Honestly I do not know if I.
(o-defun o-set-state-faces-from-theme-h (_)
  "Set face backgrounds dynamically based on theme faces.
Specifically for each element (face . built-in-face) in `o-custom-faces-alist'
set the background of FACE to the foreground of BUILT-IN-FACE and the foreground
of FACE to the background color of the `default' face."
  (pcase-dolist (`(,face . ,theme-face) o-custom-faces-alist)
    (o-set color (face-attribute theme-face :foreground nil 'default))
    (o-set bg (face-attribute 'default :background))
    (set-face-attribute face nil :background color :foreground bg)))

(add-hook 'enable-theme-functions #'o-set-state-faces-from-theme-h)
;;;; Enable server if it is not running
(defun o-init-server-h ()
  "Enable server if it is not running."
  (unless (server-running-p) (server-start)))

(add-hook 'o-emacs-startup-hook #'o-init-server-h)
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
(defun o-disable-old-themes-a (orig-fn &rest args)
  "Disable old themes before loading new ones."
  (mapc #'disable-theme custom-enabled-themes)
  (apply orig-fn args))

(advice-add 'load-theme :around #'o-disable-old-themes-a)
;;;; Do not short-circuit themes with invalid box property
;; This comes from the fact that :box (:style unspecified ...) was silently
;; tolerated before, but in Emacs 30 the :style slot must be either nil,
;; 'released-button, or 'pressed-button.
(o-defun o-set-face-attribute-a (orig-fn face frame &rest args)
  "Remove :box properties from attributes."
  (condition-case nil
      (apply orig-fn face frame args)
    (error
     ;; Remove invalid :box properties from ATTRS.
     (o-flet sanitize-attrs (attrs)
             (let ((plist (copy-sequence attrs)))
               (when-let* ((box (plist-get plist :box)))
                 (when (and (listp box)
                            (eq (plist-get box :style) 'unspecified))
                   ;; Drop the :style element entirely
                   (setq box (plist-put (copy-sequence box) :style nil))
                   (setq plist (plist-put plist :box box))))
               plist))
     (apply orig-fn face frame (sanitize-attrs args)))))

(advice-add 'set-face-attribute :around #'o-set-face-attribute-a)
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
(defun o-only-real-themes-a (themes)
  "Do not count \"fake\" themes."
  (cl-set-difference themes '(light-blue spacemacs solarized moe immaterial)))

(advice-add 'custom-available-themes :filter-return #'o-only-real-themes-a)
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
(autoload 'o-open-emacs-config "base-commands" nil nil 'function)
(autoload 'o-open-emacs-init-file "base-commands" nil nil 'function)
(autoload 'o-open-emacs-lisp-dir "base-commands" nil nil 'function)
(autoload 'o-split-window-below-and-focus "base-commands" nil nil 'function)
(autoload 'o-split-window-right-and-focus "base-commands" nil nil 'function)
(autoload 'o-set-font-face "base-commands" nil nil 'function)
(autoload 'o-dwim-space "base-commands" nil nil 'function)
(autoload 'o-dwim-narrow "base-commands" nil nil 'function)
(autoload 'o-pop-to-buffer "base-commands" nil nil 'function)
(autoload 'o-dwim-align "base-commands" nil nil 'function)
(autoload 'o-kill-emacs-no-confirm "base-commands" nil nil 'function)
(autoload 'o-new-buffer "base-commands" nil nil 'function)
(autoload 'o-load-random-theme "base-commands" nil nil 'function)
(autoload 'o-sort-dwim "base-commands" nil nil 'function)
(autoload 'o-startup-time-table "base-commands" nil nil 'function)
(autoload 'o-one-line "base-commands" nil nil 'function)
(autoload 'o-remove-consequtive-spaces "base-commands" nil nil 'function)
(autoload 'o-open-emacs-lisp-dir "base-commands" nil nil 'function)
(autoload 'o-dwim-vc-action "base-commands" nil nil 'function)
;;;; xref
(o-opt xref-search-program (if (executable-find "rg") 'ripgrep xref-search-program))
;; Select from xref candidates in minibuffer
(o-opt xref-show-definitions-function #'xref-show-definitions-completing-read)
(o-opt xref-show-xrefs-function #'xref-show-definitions-completing-read)
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
(defun o-info-rename-buffer-h ()
  "Rename current Info buffer to match its visiting manual."
  (unless (eq major-mode 'Info-mode) (user-error "This is not an Info buffer"))
  (unless (not (string-match-p "^\\*info" (buffer-name)))
    (rename-buffer (if (equal Info-current-file "dir")
                       "*info*"
                     (format "*%S*" (Info-copy-current-node-name))
                     ;; (format "*info %s*" (file-name-base Info-current-file))
                     )
                   'unique)))

(add-hook 'Info-selection-hook #'o-info-rename-buffer-h)
;;;; sh-mode
(add-hook 'sh-mode-hook #'aggressive-indent-mode)
;; (o-after smartparens (lambda () (sp-local-pair 'sh-mode "'")))
;;;; vc
;; Use text-mode tools when editing.
(add-hook 'vc-git-log-edit-mode-hook #'abbrev-mode)

(autoload 'captain-mode "captain" nil nil 'function)
(add-hook 'vc-git-log-edit-mode-hook #'captain-mode)
;;;; make certain files read-only
(autoload 'git-gutter-mode "git-gutter" nil nil 'function)

(o-defun o-dwim-file-rules ()
  "Do special things depending on what file is opened.
If I open a file in my package directory, do it in `view-mode'.  If I open a
file that is in a git repo, enale git-gutter-mode."
  (o-flet in-dir-p (apply-partially #'file-in-directory-p buffer-file-name))
  (o-flet in-any-dir-p (&rest dirs) (seq-some #'in-dir-p (mapcar #'expand-file-name dirs)))
  (when buffer-file-name
    (when (in-any-dir-p "~/.config/emacs/elpa/" "~/Downloads/")
      (read-only-mode 1))
    ;; When in a git repo enable git-gutter-mode.
    (when (vc-root-dir)
      (git-gutter-mode 1)
      ;; If it is in anyone of my dotfile directories, enable auto-committing.
      ;; (when (in-any-dir-p "~/.config/awesome/" "~/.config/emacs/" "~/")
      ;;   (o-auto-commit-mode 1))
      )))

;; Do not add this hook to `find-file-hook' immediately because anytime a file
;; is visited it will run this function.
;; Try to put this at the end.
(add-hook 'emacs-startup-hook (lambda () (add-hook 'find-file-hook #'o-dwim-file-rules 90)))

(o-defun o-load-theme-maybe-h ()
  "Load theme."
  (o-set theme o-init-theme)
  (cond ((member "--random-theme" command-line-args)
         (load-theme (seq-random-elt (custom-available-themes)) :no-confirm))
        ((not theme))
        ((member theme (custom-available-themes))
         (load-theme theme :no-confirm))
        (t
         (o-log 'info "Theme %s not found." theme))))

(add-hook 'after-init-hook #'o-load-theme-maybe-h 90)

;; This makes opening sh files way too slow.  These are simple files, it should
;; not be slow.
;; On second thought, this is what is responsible for ensuring the file is
;; indented properly so I cannot just outright ignore it.
;; (advice-add 'sh-set-shell :override  #'ignore)

;; Several packages such as org, eshell, and magit take a while to load in
;; session.  This attempts to mitigate that by loading features gradually during
;; idle time.  The idea is that by the time one of these big features is loaded,
;; many of its subfeatures will have been loaded thereby reducing the load time.
(defvar o-idle-features nil
  "List of features to load during idle time.")

(defun o-load-idle-features ()
  "Load one feature from `o-idle-features' during idle time."
  (o-awhen (pop o-idle-features)
    (condition-case err
        (progn (require it nil 'noerror)
               (o-log 'success "Idle-loaded %s" it))
      (error
       (o-log 'failure "Failed to idle-load %s: %S -> %S" feature (car err) (cdr err))))
    (run-with-idle-timer 1 nil #'o-load-idle-features)))

(defun o-setup-idle-loading-h ()
  "Setup the loading of idle features."
  (run-with-idle-timer 3 nil #'o-load-idle-features))

(add-hook 'emacs-startup-hook #'o-setup-idle-loading-h 90)

(add-hook 'o-first-file-hook #'global-auto-revert-mode)
;;; provide
(provide 'base-extra)
;;; base-extra.el ends here
