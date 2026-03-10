;;; init-core-extra.el --- Define and set several hooks -*- lexical-binding: t; -*-
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
(require 'init-core-lib)
(require 'server)
;;;; setup custom hooks
(defvar o-first-file-hook nil
  "Hook run after the first file is loaded.")

(defun o-hook--run-first-file-hook (&rest _)
  "Run `o-first-file-hook'."
  (run-hooks 'o-first-file-hook)
  (remove-hook 'find-file-hook 'o-hook--run-first-file-hook))

(add-hook 'find-file-hook #'o-hook--run-first-file-hook)

(defvar o-first-input-hook nil
  "Hook run after the first file is loaded.")

(defun o-hook--run-first-input-hook (&rest _)
  "Run `o-first-input-hook'."
  (run-hooks 'o-first-input-hook)
  (remove-hook 'pre-command-hook 'o-hook--run-first-input-hook))

(add-hook 'pre-command-hook #'o-hook--run-first-input-hook)
;;;; switch-buffer-hook
;; This is taken from https://github.com/10sr/switch-buffer-functions-el.
(defvar o--prev-buffer nil
  "Previous buffer used for `o-switch-buffer-hook'.")

(defvar o-switch-buffer-hook nil
  "Hook run after switching buffers.")

(defun o-hook--run-switch-buffers-hook ()
  "Run `o-switch-buffer-hook' if needed.
Check the result of `current-buffer', and run
`o-switch-buffer-hook' when it has been changed from
the last buffer.
This function should be hooked to `post-command-hook'."
  (unless (eq (current-buffer) o--prev-buffer)
    (let ((current (current-buffer))
          (previous o--prev-buffer))
      (setq o--prev-buffer current)
      (run-hook-with-args 'o-switch-buffer-hook previous current))))

(add-hook 'post-command-hook #'o-hook--run-switch-buffers-hook)
;;;; hooks
(add-hook 'prog-mode-hook #'hs-minor-mode)

(add-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'prog-mode-hook #'visual-line-mode)

(add-hook 'after-init-hook #'window-divider-mode 12)
(add-hook 'o-first-input-hook #'minibuffer-depth-indicate-mode)

(defun o-hook--setup-eval-after-bound-forms ()
  "Call `o-call-after-load-functions' once.
Also add it as a hook to `after-load-functions' so that it is invoked whenever a
file is loaded."
  (o-eval-after-bound-forms)
  (add-hook 'after-load-functions #'o-eval-after-bound-forms))

;;;; emacs-lisp-mode specific
(defun o-hook--extend-elisp-font-lock ()
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
(add-hook 'emacs-startup-hook #'o-hook--extend-elisp-font-lock)

(defun o-hook--require-core ()
  "Load core macros."
  (require 'init-core)
  (remove-hook 'emacs-lisp-mode-hook #'o-hook--require-core))

(add-hook 'emacs-lisp-mode-hook #'o-hook--require-core)
;;;; garbage collection
;; https://www.reddit.com/r/emacs/comments/yzb77m/an_easy_trick_i_found_to_improve_emacs_startup/
(defun o-hook--gc-increase ()
  "Boost garbage collection settings to `gcmh-high-cons-threshold'."
  (setq gc-cons-threshold o-gc-cons-threshold-high)
  (setq gc-cons-percentage o-gc-cons-percentage-high))

(add-hook 'minibuffer-setup-hook #'o-hook--gc-increase 10)

(defun o-hook--gc-decrease ()
  "Reset garbage collection settings to `gcmh-low-cons-threshold'."
  (setq gc-cons-threshold o-gc-cons-threshold-normal)
  (setq gc-cons-percentage o-gc-cons-percentage-normal))

(add-hook 'minibuffer-exit-hook #'o-hook--gc-decrease 90)

(o-defun o-timer--gc-lower-incrementally ()
  "Lower garbage collection until it reaches normal values.
Incrementally lower the value of `o-gc-cons-threshold-high' until it reaches the
value of `of-gc-cons-threshold-normal'."
  (o-flet mb (x) (/ (float x) 1024 1024))
  (if (minibuffer-window-active-p (minibuffer-window))
      (run-with-timer 5 nil #'o-timer--gc-lower-incrementally)
    (o-log 'trace "Running timer for lowering garbage collection...")
    (o-set decr (/ (- o-gc-cons-threshold-high o-gc-cons-threshold-normal) 10))
    (when (/= gc-cons-threshold o-gc-cons-threshold-normal)
      (o-set old gc-cons-threshold)
      (o-set new (max (- old decr) o-gc-cons-threshold-normal))
      (setq gc-cons-threshold new)
      (o-log 'trace "Lower `gc-cons-threshold' from %.2f to %.2f MB..." (mb old) (mb new)))
    (when (/= gc-cons-percentage o-gc-cons-percentage-normal)
      (o-set old (max gc-cons-percentage o-gc-cons-percentage-normal))
      (o-set new (max (- gc-cons-percentage 0.1) o-gc-cons-percentage-normal))
      (o-log 'trace "Lower `gc-cons-percentage' from %.1f to %.1f..." old new)
      (setq gc-cons-percentage new))
    (if (and (= gc-cons-threshold o-gc-cons-threshold-normal)
             (= gc-cons-percentage o-gc-cons-percentage-normal))
        (o-log 'trace "Done with timer.")
      (run-with-timer 7 nil #'o-timer--gc-lower-incrementally))))

(defun o-hook--gc-set-normal-value ()
  "Restore the values of `file-name-handler-alist' and `gc-cons-threshold'."
  (o-log 'trace "Restore the value of `file-name-handler-alist'.")
  (setq gc-cons-threshold o-gc-cons-threshold-high)
  (o-log 'trace "Set the value of `gc-cons-threshold' to 40 MB.")
  (run-with-timer 5 nil #'o-timer--gc-lower-incrementally))

(add-hook 'emacs-startup-hook #'o-hook--gc-set-normal-value 90)
;;;; trailing whitespace
;; (defun o-hook--delete-trailing-whitespace-at-line ()
;;   "Delete the trailing whitespace in the buffer except for the current line.
;; Also if there is more than one trailing space in the current line, replace them
;; with a single space."
;;   (delete-trailing-whitespace (point-min) (line-beginning-position))
;;   (save-match-data
;;     (when (looking-back "^.*?\\(?1:[[:space:]]\\{2,\\}\\)$" (line-beginning-position))
;;       (replace-match "\s" nil nil nil 1)))
;;   (delete-trailing-whitespace (line-end-position) (point-max)))

;; (defun o-hook--setup-delete-whitespace ()
;;   "Show trailing whitespace and delete it before saving."
;;   (setq-local show-trailing-whitespace t)
;;   (add-hook 'before-save-hook #'o-hook--delete-trailing-whitespace-at-line nil 'local)
;;   (add-hook 'kill-buffer-hook #'delete-trailing-whitespace nil 'local))

;; (add-hook 'conf-mode-hook #'o-hook--setup-delete-whitespace)
;; (add-hook 'prog-mode-hook #'o-hook--setup-delete-whitespace)
;; (add-hook 'text-mode-hook #'o-hook--setup-delete-whitespace)
;;;; startup time
(add-hook 'text-mode-hook #'delete-selection-mode)
(add-hook 'prog-mode-hook #'delete-selection-mode)
;;;; make setting faces actually work
;; Surprisingly, the function `custom-theme-set-faces' and `custom-set-faces' do
;; not by default actually change any faces.  For that to happen the variable
;; `custom--inhibit-theme-enable' needs to be nil.  Furthermore, because I
;; disable existing themes before enabling new ones even after customizing a
;; theme the customization does not persist.

(o-defun o-hook--set-faces-from-theme (_)
  "Set face backgrounds dynamically based on theme faces.
Specifically for each element (face . built-in-face) in `o-custom-faces-alist'
set the background of FACE to the foreground of BUILT-IN-FACE and the foreground
of FACE to the background color of the `default' face."
  (pcase-dolist (`(,face . ,theme-face) o-custom-faces-alist)
    (o-set color (face-attribute theme-face :foreground nil 'default))
    (o-set bg (face-attribute 'default :background))
    (set-face-attribute face nil :background color :foreground bg)))

(add-hook 'enable-theme-functions #'o-hook--set-faces-from-theme)
;;;; Enable server if it is not running
(defun o-hook--init-server ()
  "Enable server if it is not running."
  (unless (server-running-p) (server-start)))

(add-hook 'emacs-startup-hook #'o-hook--init-server)
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
(defun o-advice--disable-old-themes (orig-fn &rest args)
  "Disable old themes before loading new ones."
  (mapc #'disable-theme custom-enabled-themes)
  (apply orig-fn args))

(advice-add 'load-theme :around #'o-advice--disable-old-themes)
;;;; Do not short-circuit themes with invalid box property
;; This comes from the fact that :box (:style unspecified ...) was silently
;; tolerated before, but in Emacs 30 the :style slot must be either nil,
;; 'released-button, or 'pressed-button.
(o-defun o-advice--set-face-attribute (orig-fn face frame &rest args)
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

(advice-add 'set-face-attribute :around #'o-advice--set-face-attribute)
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
(defun o-advice--use-only-real-themes (themes)
  "Do not count \"fake\" themes."
  (cl-set-difference themes '(light-blue spacemacs solarized moe immaterial)))

(advice-add 'custom-available-themes :filter-return #'o-advice--use-only-real-themes)
;;;; Prevent *Messages* and *scratch* buffers from being killed
(defun o-hook--lock-buffers ()
  "Prevent important buffers from being killed."
  (dolist (buffer '("*Messages*" "*log*"))
    (with-current-buffer buffer
      (emacs-lock-mode 1))))

(add-hook 'emacs-startup-hook #'o-hook--lock-buffers)
;;;; autoload commands
(autoload 'o-emacs-open-config "init-core-commands" nil nil 'function)
(autoload 'o-emacs-open-init-file "init-core-commands" nil nil 'function)
(autoload 'o-emacs-open-lisp-dir "init-core-commands" nil nil 'function)
(autoload 'o-window-split-below-and-focus "init-core-commands" nil nil 'function)
(autoload 'o-window-split-right-and-focus "init-core-commands" nil nil 'function)
(autoload 'o-emacs-set-font-face "init-core-commands" nil nil 'function)
(autoload 'o-dwim-narrow-or-widen "init-core-commands" nil nil 'function)
(autoload 'o-emacs-kill-no-confirm "init-core-commands" nil nil 'function)
(autoload 'o-emacs-load-random-theme "init-core-commands" nil nil 'function)
(autoload 'o-open-emacs-lisp-dir "init-core-commands" nil nil 'function)
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
(defun o-hook--info-rename-buffer ()
  "Rename current Info buffer to match its visiting manual."
  (unless (eq major-mode 'Info-mode) (user-error "This is not an Info buffer"))
  (unless (not (string-match-p "^\\*info" (buffer-name)))
    (rename-buffer (if (equal Info-current-file "dir")
                       "*info*"
                     (format "*%S*" (Info-copy-current-node-name))
                     ;; (format "*info %s*" (file-name-base Info-current-file))
                     )
                   'unique)))

(add-hook 'Info-selection-hook #'o-hook--info-rename-buffer)
;;;; sh-mode
(add-hook 'sh-mode-hook #'aggressive-indent-mode)
;; (o-after smartparens (lambda () (sp-local-pair 'sh-mode "'")))
;;;; make certain files read-only
(o-defun o-hook--make-read-only-maybe ()
  "Do special things depending on what file is opened.
If I open a file in my package directory, do it in `view-mode'.  If I open a
file that is in a git repo, enale git-gutter-mode."
  (when buffer-file-name
    (dolist (dir (list "~/.config/emacs/elpa/" "~/.config/emacs/elpaca/" "~/Downloads/"))
      (when (file-in-directory-p buffer-file-name dir)
        (read-only-mode 1)
        (o-return)))))

(add-hook 'find-file-hook #'o-hook--make-read-only-maybe 90)

(o-defun o-hook--load-theme-maybe ()
  "Load theme."
  (o-set theme o-init-theme)
  (cond ((member "--random-theme" command-line-args)
         (load-theme (seq-random-elt (custom-available-themes)) :no-confirm))
        ((not theme))
        ((member theme (custom-available-themes))
         (load-theme theme :no-confirm))
        (t
         (o-log 'info "Theme %s not found." theme))))

(add-hook 'after-init-hook #'o-hook--load-theme-maybe 90)

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

(defun o-hook--start-idle-loading ()
  "Setup the loading of idle features."
  (run-with-idle-timer 3 nil #'o-load-idle-features))

(add-hook 'emacs-startup-hook #'o-hook--start-idle-loading 90)

(add-hook 'o-first-file-hook #'global-auto-revert-mode)

(defun o-hook--load-required-features ()
  "Load required features and log time each took to load."
  (let ((time nil)
        (success nil))
    (if (not o-required-features)
        (o-log 'info "No required features.")
      (dolist (feature (reverse o-required-features))
        (setq time (o-time-elapsed (setq success (require feature nil 'noerror))))
        (if success
            (o-log 'info "Required %s in %0.02f seconds" feature time)
          (o-log 'error "Failed to require %s" feature))))))

(add-hook 'after-init-hook #'o-hook--load-required-features 90)
;;; provide
(provide 'init-core-extra)
;;; init-core-extra.el ends here
