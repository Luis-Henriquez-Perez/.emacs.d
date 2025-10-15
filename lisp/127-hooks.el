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
(require! "^0")
(require 'server)
;;;; Setup custom hooks
(defvar oo-first-file-hook nil
  "Hook run after the first file is loaded.")

(defun oo-run-first-file-hook-h (&rest _)
  "Run `oo-first-file-hook'."
  (run-hooks 'oo-first-file-hook)
  (remove-hook 'find-file-hook 'oo-run-first-file-hook-h))

(add-hook 'find-file-hook #'oo-run-first-file-hook-h)

(defvar oo-first-input-hook nil
  "Hook run after the first file is loaded.")

(defun oo-run-first-input-hook-h (&rest _)
  "Run `oo-first-input-hook'."
  (run-hooks 'oo-first-input-hook)
  (remove-hook 'pre-command-hook 'oo-run-first-input-hook-h))

(add-hook 'pre-command-hook #'oo-run-first-input-hook-h)
;;;; hooks
;; I had been organizing the init file by packages and that is not entirely
;; useless but I think maybe an abstraction in which I look at what is happening
;; when as opposed to the configuration for over 50 individual packages.  The
;; focus is now on what is happening in my configuration as opposed to the many
;; individual configurations.
(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'prog-mode-hook #'auto-fill-mode)
(autoload 'rainbow-mode "rainbow-mode" nil nil 'function)
(add-hook 'prog-mode-hook #'rainbow-mode)
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
(add-hook 'oo-first-input-hook #'minibuffer-depth-indicate-mode)

(defun oo-setup-eval-after-bound-forms-h ()
  "Call `oo-call-after-load-functions' once.
Also add it as a hook to `after-load-functions' so that it is invoked whenever a
file is loaded."
  (oo-eval-after-bound-forms)
  (add-hook 'after-load-functions #'oo-eval-after-bound-forms))

(add-hook 'after-init-hook #'oo-setup-eval-after-bound-forms-h 99)
;;;; auto-filling
(setq-hook! prog-mode-hook normal-auto-fill-function #'oo-progn-autofill-fn)

(defun oo-progn-autofill-fn ()
  "Fill only if in a string or comment."
  (when (oo-in-string-or-comment-p) (do-auto-fill)))
;;;; emacs-lisp-mode specific
(defun oo-extend-elisp-font-lock-h ()
  "Add custom font-lock keywords."
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(\\(?:def\\(?:\\(?:advice\\|hook\\|macro\\|un\\)!\\)\\)\\)\\_>\\s-*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
      (1 font-lock-keyword-face nil t)
      (2 font-lock-function-name-face nil t))
     ("\\_<\\(\\(?:it\\|other\\|this-fn\\)\\)\\_>"
      (1 font-lock-constant-face nil t)))))

(add-hook 'emacs-lisp-mode-hook #'oo-extend-elisp-font-lock-h)

(defun oo-require-base-h ()
  "Load base macros."
  (require! "^0[01]")
  (remove-hook 'emacs-lisp-mode-hook #'oo-require-base-h))

(add-hook 'emacs-lisp-mode-hook #'oo-require-base-h)
;;;; garbage collection
;; https://www.reddit.com/r/emacs/comments/yzb77m/an_easy_trick_i_found_to_improve_emacs_startup/
(defun oo-increase-garbage-collection-h ()
  "Boost garbage collection settings to `gcmh-high-cons-threshold'."
  (set-register :gc-cons-threshold gc-cons-threshold)
  (set-register :gc-cons-percentage gc-cons-percentage)
  (setq gc-cons-threshold (* 32 1024 1024))
  (setq gc-cons-percentage 0.8))

(add-hook 'minibuffer-setup-hook #'oo-increase-garbage-collection-h 10)

(defun oo-decrease-garbage-collection-h ()
  "Reset garbage collection settings to `gcmh-low-cons-threshold'."
  (setq gc-cons-threshold (get-register :gc-cons-threshold))
  (setq gc-cons-percentage (get-register :gc-cons-percentage)))

(add-hook 'minibuffer-exit-hook #'oo-decrease-garbage-collection-h 90)

(defun! oo--timer--lower-gc ()
  "Lower garbage collection until it reaches default values."
  (flet! mb (x) (/ (float x) 1024 1024))
  (if (minibuffer-window-active-p (minibuffer-window))
      (run-with-timer 5 nil #'oo--timer--lower-gc)
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
      (run-with-timer 7 nil #'oo--timer--lower-gc))))

(defun oo-restore-startup-values-h ()
  "Restore the values of `file-name-handler-alist' and `gc-cons-threshold'."
  (oo-log 'trace "Restore the value of `file-name-handler-alist'.")
  (setq file-name-handler-alist (get-register :file-name-handler-alist))
  (setq gc-cons-threshold (* 40 1024 1024))
  (set-register :gc-cons-threshold gc-cons-threshold)
  (oo-log 'trace "Set the value of `gc-cons-threshold' to 40 MB.")
  (run-with-timer 5 nil #'oo--timer--lower-gc))

(add-hook 'emacs-startup-hook #'oo-restore-startup-values-h 90)
;;;; trailing whitespace
(defun oo-delete-trailing-whitespace-at-line-h ()
  "Delete the trailing whitespace in the buffer except for the current line.
Also if there is more than one trailing space in the current line, replace them
with a single space."
  (delete-trailing-whitespace (point-min) (line-beginning-position))
  (save-match-data
    (when (looking-back "^.*?\\(?1:[[:space:]]\\{2,\\}\\)$" (line-beginning-position))
      (replace-match "\s" nil nil nil 1)))
  (delete-trailing-whitespace (line-end-position) (point-max)))

(defun oo-setup-delete-whitespace-h ()
  "Show trailing whitespace and delete it before saving."
  (setq-local show-trailing-whitespace t)
  (add-hook 'before-save-hook #'oo-delete-trailing-whitespace-at-line-h nil 'local)
  (add-hook 'kill-buffer-hook #'delete-trailing-whitespace nil 'local))

(add-hook 'conf-mode-hook #'oo-setup-delete-whitespace-h)
(add-hook 'prog-mode-hook #'oo-setup-delete-whitespace-h)
(add-hook 'text-mode-hook #'oo-setup-delete-whitespace-h)
;;;; startup time
(defun oo-record-after-init-hook-start-time-h ()
  "Record the start of `after-init-hook'."
  :depth -100
  (oo-log 'info "Running `after-init-hook'...")
  (set-register :after-init-start (float-time)))

(add-hook 'after-init-hook #'oo-record-after-init-hook-start-time-h)

(defsubst oo-hundredths (n)
  "Return N rounded to the nearest hundredth."
  (/ (fround (* n 100)) 100.0))

(defun! oo-record-after-init-hook-end-time-h ()
  "Record the end of `after-init-hook'."
  (set! start (get-register :after-init-start))
  (set! time (oo-hundredths (- (float-time) start)))
  (set-register :after-init-hook-time time)
  (oo-log 'success "Finished running `after-init-hook' in %.2f seconds" time))

(add-hook 'after-init-hook #'oo-record-after-init-hook-end-time-h 100)

(defun oo-record-emacs-startup-hook-start-time-h ()
  "Record the start of `emacs-startup-hook'."
  (oo-log 'info "Running `emacs-startup-hook'...")
  (set-register :emacs-startup-start (float-time)))

(add-hook 'emacs-startup-hook #'oo-record-emacs-startup-hook-start-time-h -100)

(defun oo-record-emacs-startup-hook-end-time-h ()
  "Record the end of `emacs-startup-hook'."
  (set! start (get-register :emacs-startup-start))
  (set! time (oo-hundredths (- (float-time) start)))
  (set-register :emacs-startup-hook-time time)
  (oo-log 'success "Finished running `emacs-startup-hook' in %.2f seconds" time))

(add-hook 'emacs-startup-hook #'oo-record-emacs-startup-hook-end-time-h 100)

(unless noninteractive
  (autoload 'highlight-indent-guides-mode "highlight-indent-guides-mode" nil nil 'function)
  (add-hook 'mhtml-mode-hook #'highlight-indent-guides-mode))

(opt! highlight-indent-guides-method 'character)

(add-hook 'text-mode-hook #'delete-selection-mode)
(add-hook 'prog-mode-hook #'delete-selection-mode)
;;;; make setting faces actually work
;; Surprisingly, the function `custom-theme-set-faces' and `custom-set-faces' do
;; not by default actually change any faces.  For that to happen the variable
;; `custom--inhibit-theme-enable' needs to be nil.  Furthermore, because I
;; disable existing themes before enabling new ones even after customizing a
;; theme the customization does not persist.  The following hook is to add
;; basic.  Honestly I do not know if I.
(defun! oo-set-state-faces-from-theme-h (_)
  "Set face backgrounds dynamically based on theme faces.
Specifically for each element (face . built-in-face) in `oo-custom-faces-alist'
set the background of FACE to the foreground of BUILT-IN-FACE and the foreground
of FACE to the background color of the `default' face."
  (pcase-dolist (`(,face . ,theme-face) oo-custom-faces-alist)
    (set! color (face-attribute theme-face :foreground nil 'default))
    (set! bg (face-attribute 'default :background))
    (set-face-attribute face nil :background color :foreground bg)))

(add-hook 'enable-theme-functions #'oo-set-state-faces-from-theme-h)
;;;; miscellaneous
(defun oo-init-server-h ()
  "Enable server if it is not running."
  (unless (server-running-p) (server-start)))

(add-hook 'oo-emacs-startup-hook #'oo-init-server-h)
;;; provide
(provide '127-hooks)
;;; 127-hooks.el ends here
