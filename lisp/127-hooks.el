;;; 127-hooks.el --- Define and set several hooks -*- lexical-binding: t; -*-
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
(require '050-base)
(require 'server)
(require '123-base-mode-line)
(require '998-mode-line-icons)
;;;; custom hooks
(defvar oo-first-file-hook nil
  "Hook run after the first file is loaded.")

(defhook! run-first-file-hook (find-file-hook)
  :ignore-args t
  :expire t
  :level 'info
  (run-hooks 'oo-first-file-hook))

(defvar oo-first-input-hook nil
  "Hook run after the first file is loaded.")

(defhook! run-first-input-hook (pre-command-hook)
  :ignore-args t
  :expire t
  :level 'info
  (run-hooks 'oo-first-input-hook))
;;;; hooks
;; I had been organizing the init file by packages and that is not entirely
;; useless but I think maybe an abstraction in which I look at what is happening
;; when as opposed to the configuration for over 50 individual packages.  The
;; focus is now on what is happening in my configuration as opposed to the many
;; individual configurations.
(hook! text-mode-hook auto-fill-mode)
(hook! prog-mode-hook auto-fill-mode)
(hook! prog-mode-hook rainbow-mode)
(hook! prog-mode-hook hs-minor-mode)
;; (unless noninteractive
;;   ;; Do not use this hook until I fix the flyspell dict message that is
;;   ;; displayed.  Also flyspell is slow to start.
;;   ;; (hook! prog-mode-hook flyspell-prog-mode)
;;   )
(hook! text-mode-hook visual-line-mode)
;; (unless noninteractive
;;   (hook! text-mode-hook flyspell-mode))
(hook! after-init-hook window-divider-mode :depth 12)
(hook! oo-first-input-hook minibuffer-depth-indicate-mode)

;; To ensure that =oo-override-mode-map= takes priority over evil states, we need
;; to make it an intercept map for all evil states.  In evil, intercept maps are
;; maps that take priority (intercept) evil bindings when they have a different
;; binding for the same key (this is opposed to =overriding-maps=, which completely
;; override an evil keymap).
(defvar override-global-map)
(declare-function evil-make-intercept-map "evil")
(defhook! make-intercept-map (evil-mode-hook)
  "Register `oo-override-map' as an intercept map."
  (require 'bind-key)
  (evil-make-intercept-map override-global-map 'all t))

(defhook! setup-eval-after-bound-forms (after-init-hook :depth 99)
  "Call `oo-call-after-load-functions' once.
Also add it as a hook to `after-load-functions' so that it is invoked whenever a
file is loaded."
  (oo-eval-after-bound-forms)
  (hook! after-load-functions oo-eval-after-bound-forms))
;;;; auto-filling
(setq-hook! text-mode-hook normal-auto-fill-function #'oo-dwim-autofill-fn)

(defun! oo-dwim-autofill-fn (&rest _)
  "Fill the current paragraph."
  (set! beg (save-excursion (start-of-paragraph-text) (point)))
  (set! end (save-excursion (end-of-paragraph-text) (point)))
  (cond ((equal (char-after) ? )
         ;; Fill the lines before.
         (set! end1 (save-excursion (skip-chars-backward " ") (point)))
         (save-excursion (fill-region beg end1 nil 'nosqueeze))
         ;; Fill the current line in a way that does not consume the spaces.
         (save-excursion (goto-char (line-end-position)) (do-auto-fill))
         ;; Fill the lines afterwards.
         (and (> end (line-end-position))
              (save-excursion (fill-region (line-end-position) end nil 'nosqueeze))))
        (t
         (when (looking-at "\n\n")
           (set! end (save-excursion (skip-chars-backward " ") (point))))
         (save-excursion (fill-region beg end nil 'nosqueeze)))))

(setq-hook! prog-mode-hook normal-auto-fill-function #'oo-progn-autofill-fn)

(defun! oo-progn-autofill-fn ()
  "Fill only if in a string or comment."
  (when (oo-in-string-or-comment-p) (do-auto-fill)))
;;;; emacs-lisp-mode specific
(defhook! extend-elisp-font-lock (emacs-lisp-mode-hook)
  "Add custom font-lock keywords."
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(\\(?:def\\(?:\\(?:advice\\|hook\\|macro\\|un\\)!\\)\\)\\)\\_>\\s-*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
      (1 font-lock-keyword-face nil t)
      (2 font-lock-function-name-face nil t))
     ("\\_<\\(\\(?:it\\|other\\|this-fn\\)\\)\\_>"
      (1 font-lock-constant-face nil t)))))

(defhook! require-base-macros (emacs-lisp-mode-hook)
  "Load base macros."
  :expire t
  :level 'info
  (require '031-anaphoric-macros)
  (require '031-autolet-macros)
  (require '031-modification-macros)
  (require '031-looping-macros)
  (require '031-looping-macros)
  (require '035-base-macros)
  (require '037-keybinding-macros))
;;;; garbage collection
;; https://www.reddit.com/r/emacs/comments/yzb77m/an_easy_trick_i_found_to_improve_emacs_startup/
(defhook! increase-garbage-collection (minibuffer-setup-hook :depth 10)
  "Boost garbage collection settings to `gcmh-high-cons-threshold'."
  (set-register :gc-cons-threshold gc-cons-threshold)
  (set-register :gc-cons-percentage gc-cons-percentage)
  (setq gc-cons-threshold (* 32 1024 1024))
  (setq gc-cons-percentage 0.8))

(defhook! decrease-garbage-collection (minibuffer-exit-hook :depth 90)
  "Reset garbage collection settings to `gcmh-low-cons-threshold'."
  (setq gc-cons-threshold (get-register :gc-cons-threshold))
  (setq gc-cons-percentage (get-register :gc-cons-percentage)))

(defun! oo--timer--lower-garbage-collection ()
  "Lower garbage collection until it reaches default values."
  (flet! mb (x) (/ (float x) 1024 1024))
  (if (minibuffer-window-active-p (minibuffer-window))
      (run-with-timer 5 nil #'oo--timer--lower-garbage-collection)
    (oo-log 'trace "Running timer for lowering garbage collection...")
    (set! reduction (/ (get-register :gc-cons-threshold) 10))
    (set! gc-floor (* 8 1024 1024))
    (set! gcp-default 0.2)
    (when (/= gc-cons-threshold gc-floor)
      (set! old gc-cons-threshold)
      (set! new (max (- old reduction) gc-floor))
      (setq gc-cons-threshold new)
      (oo-log 'trace "Lower `gc-cons-threshold' from %.2f to %.2f MB..." (mb old) (mb new)))
    (when (/= gc-cons-percentage gcp-default)
      (set! old (max gc-cons-percentage gcp-default))
      (set! new (max (- gc-cons-percentage 0.1) gcp-default))
      (oo-log 'trace "Lower `gc-cons-percentage' from %.1f to %.1f..." old new)
      (setq gc-cons-percentage new))
    (if (and (= gc-cons-threshold gc-floor)
             (= gc-cons-percentage gcp-default))
        (oo-log 'trace "Done with timer.")
      (run-with-timer 7 nil #'oo--timer--lower-garbage-collection))))

(defhook! restore-startup-values (emacs-startup-hook :depth 90 :level 'info)
  "Restore the values of `file-name-handler-alist' and `gc-cons-threshold'."
  (oo-log 'trace "Restore the value of `file-name-handler-alist'.")
  (setq file-name-handler-alist (get-register :file-name-handler-alist))
  (setq gc-cons-threshold (* 40 1024 1024))
  (set-register :gc-cons-threshold gc-cons-threshold)
  (oo-log 'trace "Set the value of `gc-cons-threshold' to 40 MB.")
  (run-with-timer 5 nil #'oo--timer--lower-garbage-collection))
;;;; trailing whitespace
(defun oo--dwim-delete-trailing-whitespace ()
  "Delete the trailing whitespace in the buffer except for the current line.
Also if there is more than one trailing space in the current line, replace them
with a single space."
  (delete-trailing-whitespace (point-min) (line-beginning-position))
  (save-match-data
    (when (looking-back "^.*?\\(?1:[[:space:]]\\{2,\\}\\)$" (line-beginning-position))
      (replace-match "\s" nil nil nil 1)))
  (delete-trailing-whitespace (line-end-position) (point-max)))

(defhook! dwim-delete-trailing-whitespace (text-mode-hook prog-mode-hook conf-mode-hook)
  "Show trailing whitespace and delete it before saving."
  (setq-local show-trailing-whitespace t)
  (oo-add-hook 'before-save-hook #'oo--dwim-delete-trailing-whitespace :local t))

(defhook! delete-trailing-whitespace (kill-buffer-hook)
  "Ensure that trailing whitespace is deleted.
If the current buffer is in `text-mode', `prog-mode' or `conf-mode' or any mode
derived from these, delete trailing whitespace from it."
  (when (derived-mode-p 'text-mode 'prog-mode 'conf-mode)
    (delete-trailing-whitespace (point-min) (point-max))))
;;;; startup time
(defhook! record-after-init-hook-start-time (after-init-hook)
  "Record the start of `after-init-hook'."
  :depth -100
  (oo-log 'info "Running `after-init-hook'...")
  (set-register :after-init-start (float-time)))

(defsubst oo-hundredths (n)
  "Return N rounded to the nearest hundredth."
  (/ (fround (* n 100)) 100.0))

(defhook! record-after-init-hook-end-time (after-init-hook :depth 100)
  "Record the end of `after-init-hook'."
  (set! start (get-register :after-init-start))
  (set! time (oo-hundredths (- (float-time) start)))
  (set-register :after-init-hook-time time)
  (oo-log 'info "Finished running `after-init-hook' in %.2f seconds" time))

(defhook! record-emacs-startup-hook-start-time (emacs-startup-hook :depth -100)
  "Record the start of `emacs-startup-hook'."
  (oo-log 'info "Running `emacs-startup-hook'...")
  (set-register :emacs-startup-start (float-time)))

(defhook! record-emacs-startup-hook-end-time (emacs-startup-hook :depth 100)
  "Record the end of `emacs-startup-hook'."
  (set! start (get-register :emacs-startup-start))
  (set! time (oo-hundredths (- (float-time) start)))
  (set-register :emacs-startup-hook-time time)
  (oo-log 'info "Finished running `emacs-startup-hook' in %.2f seconds" time))

(unless noninteractive
  (hook! mhtml-mode-hook highlight-indent-guides-mode))

(opt! highlight-indent-guides-method 'character)

(hook! text-mode-hook delete-selection-mode)
(hook! prog-mode-hook delete-selection-mode)
;;;; make setting faces actually work
;; Surprisingly, the function `custom-theme-set-faces' and `custom-set-faces' do
;; not by default actually change any faces.  For that to happen the variable
;; `custom--inhibit-theme-enable' needs to be nil.  Furthermore, because I
;; disable existing themes before enabling new ones even after customizing a
;; theme the customization does not persist.  The following hook is to add
;; basic.  Honestly I do not know if I.
(defhook! set-state-faces-from-theme (enable-theme-functions (_))
  "Set face backgrounds dynamically based on theme faces.
Specifically for each element (face . built-in-face) in `oo-custom-faces-alist'
set the background of FACE to the foreground of BUILT-IN-FACE and the foreground
of FACE to the background color of the `default' face."
  (for! ((face . theme-face) oo-custom-faces-alist)
    (set! color (face-attribute theme-face :foreground nil 'default))
    (set! bg (face-attribute 'default :background))
    (set-face-attribute face nil :background color :foreground bg)))
;;;; miscellaneous
(hook! after-init-hook oo-mode-line-icons-mode :depth 89 :level 'info)
(hook! after-init-hook oo-mode-line-mode :depth 90 :level 'info)

;; (defhook! initialize-server (emacs-startup-hook :level 'info)
;;   "Enable server if it is not running."
;;   (unless (server-running-p) (server-start)))

(defhook! initialize-config-files (emacs-startup-hook :depth 91 :level 'info)
  "Setup config files to be loaded after their feature."
  (set! lisp-dir (expand-file-name "lisp/" user-emacs-directory))
  (set! rx "\\`990-config-\\([^[:space:]]+\\)\\.el\\'")
  (dolist (path (directory-files lisp-dir t rx))
    (set! filename (file-name-nondirectory (directory-file-name path)))
    (string-match rx filename)
    (set! parent-feature (intern (match-string 1 filename)))
    (set! feature (intern (file-name-sans-extension filename)))
    (cond ((featurep parent-feature)
           (oo-log 'info "Requiring `%S' because `%s' is loaded" feature parent-feature)
           (condition-case err
               (require feature)
             (error
              (oo-log 'error "feature %s raised an error" feature)
              (signal (car err) (cdr err)))))
          (t
           (oo-log 'trace "Deferring `%s' until parent feature, `%s', is loaded." feature parent-feature)
           (set! fn `(lambda () (condition-case err
                                    (require ',feature)
                                  (error
                                   (oo-log 'error "feature %s raised an error" ',feature)
                                   (signal (car err) (cdr err))))))
           (oo-call-after-load parent-feature fn)))))

;; Define a minor mode where I can toggle auto committing and pushing in cases where it is
;; not convenient.  I am thinking ither this or adding a way to make hooks
;; toggleable easily.  Either one function that let us me select active hooks to
;; disable or maybe letting hooks be disableable interactive functions.  Not sure.
(define-minor-mode oo-auto-commit-mode
  "Auto commit my dotfiles."
  :group 'oo
  :global t
  (if oo-auto-commit-mode
      (add-hook 'after-save-hook #'oo-auto-commit-and-push-dotfile-h)
    (remove-hook 'after-save-hook #'oo-auto-commit-and-push-dotfile-h)))

(autoload! oo-dwim-vc-action "vc")
(defun oo-auto-commit-and-push-dotfile-h ()
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
;;; provide
(provide '127-hooks)
;;; 127-hooks.el ends here
