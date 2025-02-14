;;; 127-hooks.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; TODO: add commentary
;;
;;; Code:
(require '050-base)
(require 'server)

(defvar oo-first-file-hook nil
  "Hook run after the first file is loaded.")

(defhook! oo-run-first-file-hook-h (find-file-hook)
  :expire t
  (info 'log "Running `oo-first-file-hook'...")
  (run-hooks 'oo-first-file-hook))

(defvar oo-first-input-hook nil
  "Hook run after the first file is loaded.")

(defhook! oo-run-first-input-hook-h (pre-command-hook)
  :expire t
  (info 'log "Running `oo-first-input-hook'...")
  (run-hooks 'oo-first-input-hook))

;; I had been organizing the init file by packages and that is not entirely
;; useless but I think maybe an abstraction in which I look at what is happening
;; when as opposed to the configuration for over 50 individual packages.  The
;; focus is now on what is happening in my configuration as opposed to the many
;; individual configurations.
(hook! prog-mode-hook rainbow-mode)
(hook! prog-mode-hook auto-fill-mode)
(hook! prog-mode-hook hs-minor-mode)
(hook! text-mode-hook auto-fill-mode)
(hook! text-mode-hook visual-line-mode)
(hook! oo-first-input-hook minibuffer-depth-indicate-mode)
(hook! after-init-hook window-divider-mode :depth 12)
;; (hook! text-mode flyspell-mode)
;; (hook! prog-mode-hook flyspell-prog-mode)

(defhook! oo-enable-elisp-font-lock-h (emacs-lisp-mode-hook)
  "Add custom font-lock keywords."
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(\\(?:def\\(?:\\(?:advice\\|hook\\|macro\\|un\\)!\\)\\)\\)\\_>\\s-*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
      (1 font-lock-keyword-face nil t)
      (2 font-lock-function-name-face nil t))
     ("\\_<\\(\\(?:it\\|other\\|this-fn\\)\\)\\_>"
      (1 font-lock-constant-face nil t)))))

;; To ensure that =oo-override-mode-map= takes priority over evil states, we need
;; to make it an intercept map for all evil states.  In evil, intercept maps are
;; maps that take priority (intercept) evil bindings when they have a different
;; binding for the same key (this is opposed to =overriding-maps=, which completely
;; override an evil keymap).
(defvar override-global-map)
(declare-function evil-make-intercept-map "evil")
(defhook! oo-make-intercept-map-h (evil-mode-hook)
  "Register `oo-override-map' as an intercept map."
  (require 'bind-key)
  (evil-make-intercept-map override-global-map 'all t))

(oo-call-after-load 'evil #'oo-call-after-load-functions)

(defhook! init-after-load-functions-h (oo-first-input-hook :depth 99)
  "Call `oo-call-after-load-functions' once.
Also add it as a hook to `after-load-functions' so that it is invoked whenever a
file is loaded."
  (oo-call-after-load-functions)
  (hook! after-load-functions oo-call-after-load-functions))

;; The macros in my configuration are expanded during compilation thereby saving
;; time because they do not need to be expanded during startup.  The one caviat
;; is that since they are already expanded at runtime my emacs configuration
;; will have no knowledge of them.  The `oo-macros' file will not be loaded at
;; all.  And again this is great for reducing startup time but I still want the
;; macros to be defined when I am actually editing emacs-lisp.  Therefore, I
;; load the `oo-macros' file.
;; This only needs to happen when emacs is compiled.
(defhook! oo-require-macros-h (emacs-lisp-mode-hook)
  (require '035-base-macros))

;; https://www.reddit.com/r/emacs/comments/yzb77m/an_easy_trick_i_found_to_improve_emacs_startup/
(defhook! oo-increase-garbage-collection-h (minibuffer-setup-hook :depth 10)
  "Boost garbage collection settings to `gcmh-high-cons-threshold'."
  (set-register :gc-cons-threshold gc-cons-threshold)
  (set-register :gc-cons-percentage gc-cons-percentage)
  (setq gc-cons-threshold (* 32 1024 1024))
  (setq gc-cons-percentage 0.8))

(defhook! oo-decrease-garbage-collection-h (minibuffer-exit-hook :depth 90)
  "Reset garbage collection settings to `gcmh-low-cons-threshold'."
  (setq gc-cons-threshold (get-register :gc-cons-threshold))
  (setq gc-cons-percentage (get-register :gc-cons-percentage)))

(defhook! oo-manage-trailing-whitespace-h (prog-mode-hook conf-mode-hook)
  "Show trailing whitespace and delete it before saving."
  (setq show-trailing-whitespace t)
  (oo-add-hook 'before-save-hook #'delete-trailing-whitespace :local t))

(defhook! initialize-modeline-h (after-init-hook :depth 90)
  "Initialize modeline."
  ;; I need to put the modeline in a variable so that the modeline does not
  ;; treat any strings as modeline constructs.  Why?  I want to do it myself so
  ;; that I can precompute the length of the segment.
  (require '123-base-mode-line)
  (setq-default mode-line-format '("%e" (:eval (progn (setq-local oo-mode-line-main (oo-mode-line-main)) "")) oo-mode-line-main))
  (oo-mode-line-update))

(defhook! oo-load-initial-theme-h (after-init-hook)
  "Load `modus-operandi' theme."
  (require 'modus-themes)
  (load-theme 'modus-operandi :no-confirm nil))

(defhook! initialize-server-h ()
  "Enable server if it is not running."
  (unless (server-running-p) (server-start)))

(defhook! initialize-config-files-h (emacs-startup-hook :depth 91)
  "Setup config files to be loaded after their feature."
  (set! lisp-dir (expand-file-name "lisp/" user-emacs-directory))
  (set! rx "\\`990-config-\\([^[:space:]]+\\)\\.el\\'")
  (dolist (path (directory-files lisp-dir t rx))
    (set! filename (file-name-nondirectory (directory-file-name path)))
    (string-match rx filename)
    (set! parent-feature (intern (match-string 1 filename)))
    (set! feature (intern (file-name-sans-extension filename)))
    (cond ((featurep parent-feature)
           (oo-log 'info "Parent feature `%S' is loaded, requiring `%s'" parent-feature feature)
           (require feature nil nil))
          (t
           (oo-log 'info "Deferring `%s' until parent feature, `%s', is loaded." feature parent-feature)
           (set! fn `(lambda () (require ',feature nil nil)))
           (oo-log 'info "Function to load-after -> %S" fn)
           (oo-call-after-load parent-feature fn)))))

(defun! oo--timer--lower-garbage-collection ()
  "Lower garbage collection until it reaches default values."
  (flet! mb (x) (/ (float x) 1024 1024))
  (if (minibuffer-window-active-p (minibuffer-window))
      (run-with-timer 5 nil #'oo--timer--lower-garbage-collection)
    (oo-log 'info "Running timer for lowering garbage collection...")
    (set! reduction (/ (get-register :gc-cons-threshold) 10))
    (set! gc-floor (* 8 1024 1024))
    (set! gcp-default 0.2)
    (when (/= gc-cons-threshold gc-floor)
      (set! old gc-cons-threshold)
      (set! new (max (- old reduction) gc-floor))
      (setq gc-cons-threshold new)
      (oo-log 'info "Lower `gc-cons-threshold' from %.2f to %.2f MB..." (mb old) (mb new)))
    (when (/= gc-cons-percentage gcp-default)
      (set! old (max gc-cons-percentage gcp-default))
      (set! new (max (- gc-cons-percentage 0.1) gcp-default))
      (oo-log 'info "Lower `gc-cons-percentage' from %.1f to %.1f..." old new)
      (setq gc-cons-percentage new))
    (if (and (= gc-cons-threshold gc-floor)
             (= gc-cons-percentage gcp-default))
        (oo-log 'info "Done with timer.")
      (run-with-timer 7 nil #'oo--timer--lower-garbage-collection))))

(defhook! oo-restore-startup-values-h (emacs-startup-hook :depth 90)
  "Restore the values of `file-name-handler-alist' and `gc-cons-threshold'."
  (oo-log 'info "Restore the value of `file-name-handler-alist'.")
  (setq file-name-handler-alist (get-register :file-name-handler-alist))
  (setq gc-cons-threshold (* 40 1024 1024))
  (set-register :gc-cons-threshold gc-cons-threshold)
  (oo-log 'info "Set the value of `gc-cons-threshold' to 40 MB.")
  (run-with-timer 5 nil #'oo--timer--lower-garbage-collection))

(autoload! oo-dwim-vc-action "vc")
(defhook! oo-auto-commit-and-push-dotfile-h (after-save-hook)
  "Commit and push changes to dotfile on save.
When a buffer is saved, check whether the saved file is part of the dotfiles
repository and if it is, commit and push all changes.  Otherwise, do nothing."
  (aand! (vc-root-dir)
         (buffer-file-name)
         (or (not (equal "Discharging" (battery-format "%B" (funcall battery-status-function))))
             (> (string-to-number (battery-format "%p" (funcall battery-status-function))) 90))
         (or (file-equal-p it (expand-file-name user-emacs-directory))
             (file-equal-p it (expand-file-name "~")))
         (not (equal (vc-state (buffer-file-name)) 'unregistered))
         (save-restriction (oo-dwim-vc-action (buffer-file-name)))))

(defhook! oo-set-default-font-h (after-init-hook :depth 90)
  "Set the default font based on available fonts."
  (dolist (font oo-default-fonts)
    (oo-log 'trace "Checking whether %s font is available..." font)
    (awhen! (find-font font)
      (oo-log 'info "Setting font to...%s" it)
      (set-face-attribute 'default nil :font font)
      (done!)))
  (set! default-font (face-attribute 'default :family))
  ;; So font will take effect with emacs daemon.
  ;; (add-hook 'after-make-frame-functions
  ;;           `(lambda (frame)
  ;;              (with-selected-frame frame
  ;;                (set-face-attribute 'default nil :font ,default-font))))
  (oo-log 'info "Unable to set font to any in `oo-default-font-list', defaulting to `%s'." default-font))

;; (defun oo--record-init-end-time-h ()
;;   "Record the end of `emacs-startup-hook'."
;;   (setq oo-startup-end-time (current-time)))
;;; provide
(provide '127-hooks)
;;; 127-hooks.el ends here
