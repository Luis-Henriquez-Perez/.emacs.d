;;; oo-init.el -*- lexical-binding: t; -*-
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
;;;; requirements
(require 'base)
(require 'on)
;;;; hooks
;; I had been organizing the init file by packages and that is not entirely
;; useless but I think maybe an abstraction in which I look at what is happening
;; when as opposed to the configuration for over 50 individual packages.  The
;; focus is now on what is happening in my configuration as opposed to the many
;; individual configurations.
;;;;; on-first-input-hook
(hook! on-first-input-hook minibuffer-depth-indicate-mode)
;;;;; emacs-lisp-mode-hook
(defhook! oo-enable-elisp-font-lock-h (emacs-lisp-mode-hook)
  "Add font lock keywords for definer macros."
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(\\(?:def\\(?:\\(?:advice\\|hook\\|macro\\|un\\)!\\)\\)\\)\\_>\\s-*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
      (1 font-lock-keyword-face nil t)
      (2 font-lock-function-name-face nil t))
     ("\\_<\\(\\(?:it\\|other\\|this-fn\\)\\)\\_>"
      (1 font-lock-constant-face nil t)))))
;;;;; oo-override-map
;; To ensure that =oo-override-mode-map= takes priority over evil states, we need
;; to make it an intercept map for all evil states.  In evil, intercept maps are
;; maps that take priority (intercept) evil bindings when they have a different
;; binding for the same key (this is opposed to =overriding-maps=, which completely
;; override an evil keymap).
(defhook! oo-make-intercept-map-h (evil-mode-hook)
  "Register `oo-override-map' as an intercept map."
  (evil-make-intercept-map override-global-map 'all t))
;;;;; emacs-startup-hook
(oo-call-after-load 'evil #'oo-call-after-load-functions)

(defhook! oo-init-after-load-functions-h (on-first-input-hook :depth 99)
  "Call `oo-call-after-load-functions' once.
Also add it as a hook to `after-load-functions' so that it is invoked whenever a
file is loaded."
  (oo-call-after-load-functions)
  (hook! after-load-functions oo-call-after-load-functions))
;;;;; load macros for init file
;; The macros in my configuration are expanded during compilation thereby saving
;; time because they do not need to be expanded during startup.  The one caviat
;; is that since they are already expanded at runtime my emacs configuration
;; will have no knowledge of them.  The `oo-macros' file will not be loaded at
;; all.  And again this is great for reducing startup time but I still want the
;; macros to be defined when I am actually editing emacs-lisp.  Therefore, I
;; load the `oo-macros' file.
;; This only needs to happen when emacs is compiled.
;; (defhook! oo-require-macros-h (emacs-lisp-mode-hook)
;;   (require 'base-macros))
;;;;; minibuffer
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
;;;; keybindings
;;;;; declare override-mode
(require 'bind-key)
(hook! after-init-hook override-global-mode :depth -100)
;;;;; keybindings
;;;;;; leader keymap
;;;;;;; root map
(defvar oo-leader-map (make-sparse-keymap))
(define-prefix-command 'oo-leader-prefix-command 'oo-leader-map)

;; This is the keymap that's going to contain my main bindings.  I like to think
;; about it as the root of a tree.  From this root I can access any of the leaves.  It will be bound
;; to my leader keys.
(bind! override-global-map oo-emacs-leader-key  #'oo-leader-prefix-command)
(bind! override-global-map "C-c SPC"  #'oo-leader-prefix-command)

(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements oo-leader-map "l" "localleader"))
;;;;;;; oo-buffer-map
(defvar oo-buffer-map (make-sparse-keymap))
(define-prefix-command 'oo-buffer-prefix-command 'oo-buffer-map)
(bind! oo-leader-map "b" #'oo-buffer-prefix-command :wk "buffer")
;;;;;;; oo-git-map
(defvar oo-git-map (make-sparse-keymap))
(define-prefix-command 'oo-git-prefix-command 'oo-git-map)
(bind! oo-leader-map "g" #'oo-git-prefix-command :wk "git")
(bind! oo-git-map "n" #'vc-next-action)
(bind! oo-git-map "b" #'vc-create-branch)
;;;;;;; oo-window-map
(defvar oo-window-map (make-sparse-keymap))
(define-prefix-command 'oo-window-prefix-command 'oo-window-map)
(bind! oo-leader-map "w" #'oo-window-prefix-command :wk "window")
;;;;;;; oo-app-map
(defvar oo-app-map (make-sparse-keymap))
(define-prefix-command 'oo-app-prefix-command 'oo-app-map)
(bind! oo-leader-map "a" #'oo-app-prefix-command :wk "app")
;;;;;;; oo-find-map
(defvar oo-find-map (make-sparse-keymap))
(define-prefix-command 'oo-find-prefix-command 'oo-find-map)
(bind! oo-leader-map "f" #'oo-find-prefix-command :wk "find")

(bind! oo-find-map ";" #'save-buffer)
(bind! oo-find-map "I" #'oo-open-emacs-config)
(bind! oo-find-map "i" #'imenu)
(bind! oo-find-map "j" #'oo-dwim-narrow)
(bind! oo-find-map "n" #'oo-new-buffer)
(bind! oo-find-map "o" #'find-file)
(bind! oo-find-map "f" #'switch-to-buffer)
(bind! oo-find-map "d" #'display-buffer)

(bind! oo-find-map "a" #'find-library)
;;;;;;; oo-help-map
(defvar oo-help-map (make-sparse-keymap))
(define-prefix-command 'oo-help-prefix-command 'oo-help-map)
(bind! oo-leader-map "h" #'oo-help-prefix-command :wk "help")

;; Emacs has a family of describe functions that are used for help and
;; introspection.  To name a few, there's [[file:snapshots/_helpful_command__describe-function_.png][describe-function]], [[file:snapshots/_helpful_command__describe-character_.png][describe-character]].
;; The command =describe-callable=  and =describe-variable= are the ones I use the most
;; by far and I them it to be accessible quickly.  The various snapshots you see
;; named are a result of these functions and you can already guess buy how many
;; such snapshots there are how much I use these commands.
(bind! oo-help-map "m" #'describe-mode)
(bind! oo-help-map "l" #'describe-function)
(bind! oo-help-map "f" #'describe-function)
(bind! oo-help-map "j" #'describe-variable)
(bind! oo-help-map "v" #'describe-variable)
(bind! oo-help-map "h" #'describe-variable)
(bind! oo-help-map "C" #'describe-char)
(bind! oo-help-map "k" #'describe-key)
;;;;;;; oo-emms-map
(defvar oo-emms-map (make-sparse-keymap))
(define-prefix-command 'oo-emms-prefix-command 'oo-emms-map)
(bind! oo-leader-map "e" #'oo-emms-prefix-command :wk "emms")
;;;;;;; oo-toggle-map
(defvar oo-toggle-map (make-sparse-keymap)
  "Keymap that contains bindings for things that should be toggled.")
(define-prefix-command 'oo-toggle-prefix-command 'oo-toggle-map)
(bind! oo-leader-map "t" #'oo-toggle-prefix-command :wk "toggle")

(bind! oo-toggle-map "w" #'whitespace-mode)
(bind! oo-toggle-map "l" #'display-line-numbers-mode)
(bind! oo-toggle-map "u" #'toggle-truncate-lines)
(bind! oo-toggle-map "n" #'oo-dwim-narrow)
(bind! oo-toggle-map "i" #'iedit-mode)
(bind! oo-toggle-map "e" #'eval-expression)
(bind! oo-toggle-map "f" #'oo-set-font-face)
(bind! oo-toggle-map "r" #'read-only-mode)
(bind! oo-toggle-map "d" #'toggle-debug-on-error)
(bind! oo-toggle-map "P" #'profiler-stop)
;;;;;;; oo-dotfile-map
(defvar oo-dotfile-map (make-sparse-keymap))
(define-prefix-command 'oo-dotfile-prefix-command 'oo-dotfile-map)
(bind! oo-leader-map "d" #'oo-dotfile-prefix-command :wk "dotfile")
;;;;;;; oo-quit-map
(defvar oo-quit-map (make-sparse-keymap))
(define-prefix-command 'oo-quit-prefix-command 'oo-quit-map)
(bind! oo-leader-map "q" #'oo-quit-prefix-command :wk "quit")
;;;;;;; oo-package-map
(defvar oo-package-map (make-sparse-keymap))
(define-prefix-command 'oo/package-prefix-command 'oo-package-map)
(bind! oo-leader-map "p" #'oo/package-prefix-command :wk "package")

;; (bind! oo-package-map "b" #'elpaca-browse)
;; (bind! oo-package-map "U" #'elpaca-update-all)
;; (bind! oo-package-map "u" #'elpaca-update)
;; (bind! oo-package-map "v" #'elpaca-visit)
;; (bind! oo-package-map "i" #'elpaca-try)
;; (bind! oo-package-map "r" #'elpaca-rebuild)
;; (bind! oo-package-map "d" #'elpaca-delete)
;; (bind! oo-package-map "l" #'elpaca-log)
;; (bind! oo-package-map "m" #'elpaca-manager)

(bind! oo-package-map "i" #'package-install)
;;;;;; opening Emacs places
(bind! oo-find-map "E" #'oo-open-emacs-config)
(bind! oo-find-map "I" #'oo-open-emacs-init-file)
(bind! oo-find-map "L" #'oo-open-emacs-lisp-dir)
(bind! oo-find-map "G" #'rgrep)
;;;; set initial font
;; This is very basic font setting based on available faces.  I have seen much
;; more complex font setups like in minemacs (which probably got its from doom)
;; but for now this will do.
(defvar oo-default-fonts (list (font-spec :family "CaskaydiaCove Nerd Font Mono"
                                          :weight 'regular
                                          :slant 'normal
                                          :width 'normal
                                          :size 18)
                               (font-spec :family "RecMonoDuotone Nerd Font"
                                          :weight 'regular
                                          :slant 'normal
                                          :width 'normal
                                          :size 18)
                               (font-spec :family "Mononoki Nerd Font"
                                          :weight 'regular
                                          :slant 'normal
                                          :width 'normal
                                          :size 18)
                               (font-spec :family "JetBrainsMono Nerd Font"
                                          :weight 'regular
                                          :slant 'normal
                                          :width 'normal
                                          :size 18))
  "List of fonts to check.")

(defhook! oo-set-default-font-h (after-init-hook :depth 90)
  "Set the default font based on available fonts."
  (dolist (font oo-default-fonts)
    (trace! "Checking whether %s font is available..." font)
    (awhen! (find-font font)
      (info! "Setting font to...%s" it)
      (set-face-attribute 'default nil :font font)
      (done!)))
  (set! default-font (face-attribute 'default :family))
  ;; So font will take effect with emacs daemon.
  ;; (add-hook 'after-make-frame-functions
  ;;           `(lambda (frame)
  ;;              (with-selected-frame frame
  ;;                (set-face-attribute 'default nil :font ,default-font))))
  (info! "Unable to set font to any in `oo-default-font-list', defaulting to `%s'." default-font))
;;;; sort lines
;; (defun! oo-sort-elpaca-forms-h ()
;;   "Sort elpaca package forms in current buffer."
;;   (set! rx "^\\(?:;; \\)?(elpaca \\(?:(\\(?1:\\(?:[[:alnum:]]\\|-\\)+\\)\\|\\(?1:\\(?:[[:alnum:]]\\|-\\)+\\)\\)[^z-a]+?$")
;;   (set! beg (point-min))
;;   (set! end (point-max))
;;   (save-excursion
;;     (goto-char beg)
;;     (when (re-search-forward rx end t nil)
;;       (sort-regexp-fields nil rx "\\1" (match-beginning 0) end))))

;; (defhook! oo-setup-auto-line-sorting-maybe-h (find-file-hook)
;;   "Setup auto line sorting for `init-elpaca'."
;;   (set! path "~/.config/emacs/lisp/init-elpaca.el")
;;   (when (f-same-p (buffer-file-name) (expand-file-name path))
;;     (info! "Setup auto-sorting for %s..." (oo-file-base path))
;;     (hook! before-save-hook oo-sort-elpaca-forms-h :local t)))

;; (defun! oo-align-abbrev-forms-h ()
;;   "Align all abbrev forms in current buffer."
;;   (set! regexp "(define-abbrev\\(?1:\\s-+\\)\\S-+\\(?2:\\s-+\\)\".*?\"\\(?3:\\s-+\\)\".*?\"\\(?4:\\s-+\\)\\S-+\\(?5:\\s-+\\):enable-function\\(?6:\\s-+\\).+)")
;;   (set! rules `((rule1 . ((regexp . ,regexp) (group . (1 2 3 4 5 6))))))
;;   (save-excursion
;;     (goto-char (point-min))
;;     (when (re-search-forward rx nil t nil)
;;       (quietly! (align (match-beginning 0) (point-max) nil rules)))))

;; (defhook! oo-setup-auto-alignment-maybe-h (find-file-hook)
;;   "Set up auto alignment for certain buffers."
;;   (set! path "~/.config/emacs/lisp/+abbrev-plain-text-abbrevs.el")
;;   (when (f-same-p (buffer-file-name) (expand-file-name path))
;;     (info! "Setup auto-aligning for %S..." (oo-file-base path))
;;     (add-hook 'before-save-hook #'oo-align-abbrev-forms-h nil t)))
;;;; enable initial theme
(defhook! oo-load-initial-theme-h (after-init-hook)
  "Load `modus-operandi' theme."
  (require 'modus-themes)
  (load-theme 'modus-operandi :no-confirm nil))
;;;; start emacs server
;; This is so that if I need to use some sort of program to open a file, it will
;; use he running emacs daemon.
(defhook! oo-initialize-server-h ()
  "Enable server if it is not running."
  (unless (server-running-p) (server-start)))
;;;; setup loading config files
(defhook! oo-initialize-config-files-h (emacs-startup-hook :depth 91)
  "Setup config files to be loaded after their feature."
  (set! lisp-dir (expand-file-name "lisp/" user-emacs-directory))
  (set! rx "\\`config-\\([^[:space:]]+\\)\\.el\\'")
  (dolist (path (directory-files lisp-dir t rx))
    (set! filename (file-name-nondirectory (directory-file-name path)))
    (string-match rx filename)
    (set! parent-feature (intern (match-string 1 filename)))
    (set! feature (intern (file-name-sans-extension filename)))
    (cond ((featurep parent-feature)
           (info! "Parent feature `%S' is loaded, requiring `%s'" parent-feature feature)
           (require feature filename nil))
          (t
           (info! "Deferring `%s' until parent feature, `%s', is loaded." feature parent-feature)
           (set! fn `(lambda () (require ',feature ,path nil)))
           (info! "Function to load-after -> %S" fn)
           (oo-call-after-load parent-feature fn)))))
;;; provide
(provide 'oo-init)
;;; oo-init.el ends here
