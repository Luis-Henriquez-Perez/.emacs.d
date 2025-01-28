;;; 00-init-package.el --- Initialize package -*- lexical-binding: t; -*-
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
(setq package-user-dir (locate-user-emacs-file "packages/"))

(require 'cl-lib)
(require 'package)
(require 'package-vc)
(require 'base-lib)
(eval-when-compile (require 'base-macros))

;; This was taken from prot's recommendation.
(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

(add-to-list 'package-archives '("gnu-elpa"       . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("gnu-elpa-devel" . "https://elpa.gnu.org/devel/"))
(add-to-list 'package-archives '("melpa"          . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("nongnu"         . "https://elpa.nongnu.org/nongnu/"))

(setq package-archive-priorities '(("melpa" . 10) ("gnu-elpa" . 9) ("nongnu" . 8)))

(push 'ace-jump-helm-line package-selected-packages)
(push 'ace-link package-selected-packages)
(push 'ace-window package-selected-packages)
(push 'aggressive-indent package-selected-packages)
(push 'all-the-icons package-selected-packages)
(push 'all-the-icons-nerd-fonts package-selected-packages)
(push 'annalist package-selected-packages)
(push 'beacon package-selected-packages)
(push 'buffer-terminator package-selected-packages)
(push 'burly package-selected-packages)
(push 'cape package-selected-packages)
(push 'caps-lock package-selected-packages)
(push 'captain package-selected-packages)
(push 'consult package-selected-packages)
(push 'corfu package-selected-packages)
(push 'cyberpunk-theme package-selected-packages)
(push 'dashboard package-selected-packages)
(push 'denote package-selected-packages)
(push 'dimmer package-selected-packages)
(push 'dirvish package-selected-packages)
(push 'doct package-selected-packages)
(push 'easy-escape package-selected-packages)
(push 'eat package-selected-packages)
(push 'edit-indirect package-selected-packages)
(push 'elfeed package-selected-packages)
(push 'elmacro package-selected-packages)
(push 'embark package-selected-packages)
(push 'embark-consult package-selected-packages)
(push 'emmet-mode package-selected-packages)
(push 'emms package-selected-packages)
(push 'eros package-selected-packages)
(push 'eshell-prompt-extras package-selected-packages)
(push 'eshell-syntax-highlighting package-selected-packages)
(push 'eshell-up package-selected-packages)
(push 'eshell-z package-selected-packages)
(push 'esup package-selected-packages)
(push 'evenok package-selected-packages)
(push 'evil package-selected-packages)
(push 'evil-cleverparens package-selected-packages)
(push 'evil-collection package-selected-packages)
(push 'evil-exchange package-selected-packages)
(push 'evil-fringe-mark package-selected-packages)
(push 'evil-goggles package-selected-packages)
(push 'evil-iedit-state package-selected-packages)
(push 'evil-lisp-state package-selected-packages)
(push 'evil-nerd-commenter package-selected-packages)
(push 'evil-surround package-selected-packages)
(push 'evil-textobj-column package-selected-packages)
(push 'evil-textobj-entire package-selected-packages)
(push 'evil-textobj-line package-selected-packages)
(push 'evil-textobj-syntax package-selected-packages)
(push 'evil-visual-mark-mode package-selected-packages)
(push 'expreg package-selected-packages)
(push 'exwm package-selected-packages)
(push 'f package-selected-packages)
(push 'fancy-narrow package-selected-packages)
(push 'fennel-mode package-selected-packages)
(push 'filladapt package-selected-packages)
(push 'fit-text-scale package-selected-packages)
(push 'git-gutter package-selected-packages)
(push 'goto-chg package-selected-packages)
(push 'green-screen-theme package-selected-packages)
(push 'grugru package-selected-packages)
(push 'gruvbox-theme package-selected-packages)
(push 'habamax-theme package-selected-packages)
(push 'helm package-selected-packages)
(push 'helm-system-packages package-selected-packages)
(push 'helpful package-selected-packages)
(push 'hemera-theme package-selected-packages)
(push 'hide-mode-line package-selected-packages)
(push 'highlight-indent-guides package-selected-packages)
(push 'highlight-quoted package-selected-packages)
(push 'htmlize package-selected-packages)
(push 'ctable package-selected-packages)
(push 'humanoid-themes package-selected-packages)
(push 'hungry-delete package-selected-packages)
(push 'hy-mode package-selected-packages)
(push 'hydra package-selected-packages)
(push 'immaterial-theme package-selected-packages)
(push 'leuven-theme package-selected-packages)
(push 'doom-themes package-selected-packages)
(push 'lispy package-selected-packages)
(push 'lispyville package-selected-packages)
(push 'loopy package-selected-packages)
(push 'lorem-ipsum package-selected-packages)
(push 'lua-mode package-selected-packages)
(push 'macrostep package-selected-packages)
(push 'magit package-selected-packages)
(push 'marginalia package-selected-packages)
(push 'markdown-mode package-selected-packages)
(push 'material-theme package-selected-packages)
(push 'minimal-theme package-selected-packages)
(push 'modus-themes package-selected-packages)
(push 'monkeytype package-selected-packages)
(push 'monokai-theme package-selected-packages)
(push 'nerd-icons package-selected-packages)
(push 'no-littering package-selected-packages)
(push 'noccur package-selected-packages)
(push 'notmuch package-selected-packages)
(push 'one-themes package-selected-packages)
(push 'orderless package-selected-packages)
(push 'org-appear package-selected-packages)
(push 'org-bookmark-heading package-selected-packages)
(push 'org-fancy-priorities package-selected-packages)
(push 'org-ml package-selected-packages)
(push 'org-pretty-tags package-selected-packages)
(push 'org-superstar package-selected-packages)
(push 'org-tidy package-selected-packages)
(push 'orglink package-selected-packages)
(push 'password-store package-selected-packages)
(push 'pcre2el package-selected-packages)
(push 'php-mode package-selected-packages)
(push 'polymode package-selected-packages)
(push 'pomodoro package-selected-packages)
(push 'rainbow-delimiters package-selected-packages)
(push 'rainbow-mode package-selected-packages)
(push 'redacted package-selected-packages)
(push 'restart-emacs package-selected-packages)
(push 's package-selected-packages)
(push 'sdcv package-selected-packages)
(push 'sly package-selected-packages)
(push 'smartparens package-selected-packages)
(push 'solarized-theme package-selected-packages)
(push 'spacemacs-theme package-selected-packages)
(push 'ssh-agency package-selected-packages)
(push 'standard-themes package-selected-packages)
(push 'stripe-buffer package-selected-packages)
(push 'sudo-edit package-selected-packages)
(push 'super-save package-selected-packages)
(push 'tango-plus-theme package-selected-packages)
(push 'telephone-line package-selected-packages)
(push 'tempel package-selected-packages)
(push 'tempel-collection package-selected-packages)
(push 'textsize package-selected-packages)
(push 'transpose-frame package-selected-packages)
(push 'transwin package-selected-packages)
(push 'ts package-selected-packages)
(push 'vc-auto-commit package-selected-packages)
(push 'vertico package-selected-packages)
(push 'w3m package-selected-packages)
(push 'web-mode package-selected-packages)
(push 'wgrep package-selected-packages)
(push 'which-key package-selected-packages)
(push 'ws-butler package-selected-packages)
(push 'xr package-selected-packages)
(push 'yeetube package-selected-packages)
(push 'zoutline package-selected-packages)

(push '(emacs-wallpaper :url "https://github.com/Luis-Henriquez-Perez/emacs-wallpaper" :branch "mine") package-vc-selected-packages)
(push '(escr :url "https://github.com/Luis-Henriquez-Perez/escr") package-vc-selected-packages)
(push '(evil-easymotion :url "https://github.com/Luis-Henriquez-Perez/evil-easymotion" :branch "master") package-vc-selected-packages)
(push '(on :url "https://github.com/ajgrf/on.el") package-vc-selected-packages)
(push '(outli :url "https://github.com/jdtsmith/outli") package-vc-selected-packages)
(push '(spaceline :url "https://github.com/Luis-Henriquez-Perez/spaceline" :branch "add-evil-operator-state-face") package-vc-selected-packages)
(push '(zone-matrix :url "https://github.com/ober/zone-matrix" :branch "master") package-vc-selected-packages)

;; The function `package-install-selected-packages' does not activate the
;; packages which causes a problem fo rme.
(unless (bound-and-true-p package--initialized)
  (time-elapsed! (package-initialize)))

(unless package-archive-contents
  (message "Refreshing contents...")
  (time-elapsed! (package-refresh-contents)))

;; (remove-hook 'kill-emacs-hook #'emms-history-save)
;; Manage garbage collection myself.  U shouldn't just disable garbage
;; collection altogether for this becausee ur emacs could crash if it has too
;; much uncollected garbage.
(time-elapsed!
 (let ((refreshed-contents-p nil)
       (gc-cons-threshold most-positive-fixnum))
   (dolist (package package-selected-packages)
     (cond ((assq package package-archive-contents)
            (unless (package-installed-p package)
              (unless refreshed-contents-p
                (package-refresh-contents)
                (setq refreshed-contents-p (not refreshed-contents-p)))
              (message "package is not installed %s package" package)
              (with-demoted-errors "%S" (package-install package 'dont-select))
              (if (package-installed-p package)
                  (garbage-collect)
                (message "Failed to install package `%s'" package))))
           (t
            (message "Package %s is not available." package))))))

(time-elapsed! (package-vc-install-selected-packages))
;;; provide
(provide '00-init-package)
;;; 00-init-package.el ends here
