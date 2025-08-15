;;; 110-init-package.el --- Initialize package -*- lexical-binding: t; -*-
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
;; Initialize package.
;; Install all of my packages.
;;
;;; Code:
;; If this variable is not set beforehand, `package-gnupghome-dir' will not be
;; set to the right place.
(defvar package-user-dir)
(defvar package-archive-contents)

(setq package-user-dir (locate-user-emacs-file "elpa"))

(require 'cl-lib)
(require 'package)
(require 'package-vc)
(require '036-base-functions)
(eval-when-compile (require '035-base-macros))

;; This was taken from prot's recommendation.
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

(setq package-archives '(("gnu-elpa"     . "https://elpa.gnu.org/packages/")
                         ("melpa"        . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("nongnu"       . "https://elpa.nongnu.org/nongnu/")))

;; I experienced bugs from installing bleeding-edge org-mode.  Do not do this,
;; use the stable version.
(setq package-pinned-packages '((magit . "melpa-stable") (org . "melpa-stable")))

;; The package quickstart feature puts concatenates autoloads into one file so
;; they can be read all at once later.  This does speed up `package-initialize'
;; but not by much--maybe 0.2 seconds.  As I mention later
;; `package-read-archive-contents' accounts for the bulk of package-initialize's
;; slowness.
(setq package-quickstart-file (expand-file-name "package-quickstart.el" oo-cache-dir))
(setq package-quickstart t)

(setq package-archive-priorities '(("melpa" . 10) ("gnu-elpa" . 9) ("nongnu" . 8)))

(setq package-selected-packages '(ace-jump-helm-line
                                  ace-link
                                  ace-window
                                  aggressive-indent
                                  all-the-icons
                                  all-the-icons-nerd-fonts
                                  beacon
                                  buffer-terminator
                                  burly
                                  cape
                                  caps-lock
                                  captain
                                  chess
                                  consult
                                  corfu
                                  cyberpunk-theme
                                  dashboard
                                  date2name
                                  denote
                                  dimmer
                                  dirvish
                                  doct
                                  easy-escape
                                  eat
                                  edit-indirect
                                  elfeed
                                  elmacro
                                  embark
                                  embark-consult
                                  emmet-mode
                                  emms
                                  eros
                                  eshell-syntax-highlighting
                                  eshell-up
                                  eshell-z
                                  esup
                                  evil
                                  evil-cleverparens
                                  evil-collection
                                  evil-exchange
                                  evil-fringe-mark
                                  evil-goggles
                                  evil-iedit-state
                                  evil-keypad
                                  evil-lisp-state
                                  evil-nerd-commenter
                                  evil-surround
                                  evil-textobj-column
                                  evil-textobj-entire
                                  evil-textobj-line
                                  evil-textobj-syntax
                                  evil-visual-mark-mode
                                  expreg
                                  exwm
                                  f
                                  fancy-narrow
                                  fennel-mode
                                  filladapt
                                  fit-text-scale
                                  git-gutter
                                  goto-chg
                                  green-screen-theme
                                  grugru
                                  gruvbox-theme
                                  habamax-theme
                                  helm
                                  helm-system-packages
                                  helpful
                                  hemera-theme
                                  hide-mode-line
                                  highlight-indent-guides
                                  highlight-quoted
                                  htmlize
                                  ctable
                                  humanoid-themes
                                  hungry-delete
                                  hy-mode
                                  hydra
                                  immaterial-theme
                                  leuven-theme
                                  doom-themes
                                  lispy
                                  lispyville
                                  lorem-ipsum
                                  lua-mode
                                  macrostep
                                  magit
                                  marginalia
                                  markdown-mode
                                  material-theme
                                  modus-themes
                                  monokai-theme
                                  nerd-icons
                                  nerd-icons-completion
                                  no-littering
                                  noccur
                                  notmuch
                                  one-themes
                                  orderless
                                  org
                                  org-appear
                                  org-bookmark-heading
                                  org-fancy-priorities
                                  org-ml
                                  org-pretty-tags
                                  org-superstar
                                  org-tidy
                                  password-store
                                  pcre2el
                                  php-mode
                                  powerline
                                  rainbow-delimiters
                                  rainbow-mode
                                  redacted
                                  restart-emacs
                                  sdcv
                                  sly
                                  smartparens
                                  spacemacs-theme
                                  ssh-agency
                                  standard-themes
                                  sudo-edit
                                  super-save
                                  tango-plus-theme
                                  telephone-line
                                  tempel
                                  tempel-collection
                                  textsize
                                  transpose-frame
                                  transwin
                                  try
                                  ts
                                  vc-auto-commit
                                  vertico
                                  w3m
                                  web-mode
                                  wgrep
                                  which-key
                                  ws-butler
                                  xr
                                  yeetube))

(setq package-vc-selected-packages '((emacs-wallpaper :url "https://github.com/Luis-Henriquez-Perez/emacs-wallpaper" :branch "mine")
                                     (escr :url "https://github.com/Luis-Henriquez-Perez/escr")
                                     (evil-easymotion :url "https://github.com/Luis-Henriquez-Perez/evil-easymotion" :branch "master")
                                     (outli :url "https://github.com/jdtsmith/outli")
                                     (zone-matrix :url "https://github.com/ober/zone-matrix" :branch "master")))

;; The function `package-install-selected-packages' does not activate the
;; packages which causes a problem for me.

;; The function `package-initialize' is really slow.  The main culprit is
;; `package-read-all-archive-contents' which reads `package-archives' and
;; populates `package-archive-contents', an alist of package names and
;; package-desc objects that contains data about all available packages.  It is
;; needed when installing packages but not when all of our packages are already
;; installed, which is the situation most of the time.
(if (bound-and-true-p package--initialized)
    (oo-log 'warn "The variable `package--initialized' unexpectedly non-nil")
  ;; The variable `package-alist' is an alist of installed packages.  It is
  ;; populated by `package-load-all-descriptors'.
  (setq package-alist nil)
  ;; The descriptors are objects that are created by loading and reading many
  ;; files.  This is a relatively expensive process.
  ;; Attempt to cache the descriptors.
  ;; (package-load-all-descriptors)
  ;; I need to update the cache when I install a package and only then can I use
  ;; this cache code to save a bit more startup time.
  (package-load-all-descriptors)
  ;; (let ((cache (expand-file-name "package-alist" oo-cache-dir)))
  ;;   (if (file-exists-p cache)
  ;;       (setq package-alist (with-temp-buffer
  ;;                             (insert-file-contents cache)
  ;;                             (read (current-buffer))))
  ;;     (package-load-all-descriptors)
  ;;     (with-temp-file cache
  ;;       (prin1 package-alist (current-buffer)))))
  (setq package--initialized t)
  (package-activate-all)
  (package--build-compatibility-table)

  ;; This is inspired by centaur-emacs.  I add the the lisp directory to the
  ;; front of the load-path so files from here can load faster.
  (push (expand-file-name "lisp/" user-emacs-directory) load-path))

;; Make sure I do not have to initialize package contents from scratch when
;; installing package.
(defun oo--read-archive-contents (orig-fn &rest args)
  ;; Reading archive contents is really expensive and you do not want to do it
  ;; on startup.
  (prog2 (package-read-all-archive-contents)
      (apply orig-fn args)
    (advice-remove 'package-install #'oo--read-archive-contents)))

(advice-add 'package--archives-initialize :around #'oo--read-archive-contents)

(let ((read-archive-contents-p nil))
  (dolist (package package-selected-packages)
    (unless (package-installed-p package)
      (unless read-archive-contents-p
        (package-read-all-archive-contents)
        (setq read-archive-contents-p t)
        ;; (unless (cl-every (lambda (package) (assoc package package-archive-contents)) uninstalled)
        ;;   (package-refresh-contents))
        )
      (oo-log 'info "Installing %S..." package)
      (condition-case _
          (package-install package)
        (error
         (oo-log 'error "Failed to install package `%s'" package)
         t))
      ;; If the `gc-cons-threshold' is set to `most-positive-fixum' (essentially
      ;; disabling garbage collection), accumulating too much garbage via
      ;; installing packages will cause slowdowns, lags and freezes.  Here I need
      ;; to ensure I periodically garbage collect.
      (garbage-collect))))

(package-vc-install-selected-packages)
;;; provide
(provide '110-init-package)
;;; 110-init-package.el ends here
