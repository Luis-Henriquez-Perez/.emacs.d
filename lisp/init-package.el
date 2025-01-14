;;; init-package.el --- Initialize package -*- lexical-binding: t; -*-
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
(require '04-base-lib)
(eval-when-compile (require '03-base-macros))

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

(cl-pushnew 'ace-jump-helm-line package-selected-packages)
(cl-pushnew 'ace-link package-selected-packages)
(cl-pushnew 'ace-window package-selected-packages)
(cl-pushnew 'aggressive-indent package-selected-packages)
(cl-pushnew 'all-the-icons package-selected-packages)
(cl-pushnew 'all-the-icons-nerd-fonts package-selected-packages)
(cl-pushnew 'annalist package-selected-packages)
(cl-pushnew 'beacon package-selected-packages)
(cl-pushnew 'buffer-terminator package-selected-packages)
(cl-pushnew 'burly package-selected-packages)
(cl-pushnew 'cape package-selected-packages)
(cl-pushnew 'caps-lock package-selected-packages)
(cl-pushnew 'captain package-selected-packages)
(cl-pushnew 'consult package-selected-packages)
(cl-pushnew 'corfu package-selected-packages)
(cl-pushnew 'cyberpunk-theme package-selected-packages)
(cl-pushnew 'dashboard package-selected-packages)
(cl-pushnew 'denote package-selected-packages)
(cl-pushnew 'dimmer package-selected-packages)
(cl-pushnew 'dirvish package-selected-packages)
(cl-pushnew 'doct package-selected-packages)
(cl-pushnew 'easy-escape package-selected-packages)
(cl-pushnew 'eat package-selected-packages)
(cl-pushnew 'edit-indirect package-selected-packages)
(cl-pushnew 'elfeed package-selected-packages)
(cl-pushnew 'elmacro package-selected-packages)
(cl-pushnew 'embark package-selected-packages)
(cl-pushnew 'embark-consult package-selected-packages)
(cl-pushnew 'emmet-mode package-selected-packages)
(cl-pushnew 'emms package-selected-packages)
(cl-pushnew 'eros package-selected-packages)
(cl-pushnew 'eshell-prompt-extras package-selected-packages)
(cl-pushnew 'eshell-syntax-highlighting package-selected-packages)
(cl-pushnew 'eshell-up package-selected-packages)
(cl-pushnew 'eshell-z package-selected-packages)
(cl-pushnew 'esup package-selected-packages)
(cl-pushnew 'evenok package-selected-packages)
(cl-pushnew 'evil package-selected-packages)
(cl-pushnew 'evil-cleverparens package-selected-packages)
(cl-pushnew 'evil-collection package-selected-packages)
(cl-pushnew 'evil-exchange package-selected-packages)
(cl-pushnew 'evil-fringe-mark package-selected-packages)
(cl-pushnew 'evil-goggles package-selected-packages)
(cl-pushnew 'evil-iedit-state package-selected-packages)
(cl-pushnew 'evil-lisp-state package-selected-packages)
(cl-pushnew 'evil-nerd-commenter package-selected-packages)
(cl-pushnew 'evil-surround package-selected-packages)
(cl-pushnew 'evil-textobj-column package-selected-packages)
(cl-pushnew 'evil-textobj-entire package-selected-packages)
(cl-pushnew 'evil-textobj-line package-selected-packages)
(cl-pushnew 'evil-textobj-syntax package-selected-packages)
(cl-pushnew 'evil-visual-mark-mode package-selected-packages)
(cl-pushnew 'expreg package-selected-packages)
(cl-pushnew 'exwm package-selected-packages)
(cl-pushnew 'f package-selected-packages)
(cl-pushnew 'fancy-narrow package-selected-packages)
(cl-pushnew 'fennel-mode package-selected-packages)
(cl-pushnew 'filladapt package-selected-packages)
(cl-pushnew 'fit-text-scale package-selected-packages)
(cl-pushnew 'git-gutter package-selected-packages)
(cl-pushnew 'goto-chg package-selected-packages)
(cl-pushnew 'green-screen-theme package-selected-packages)
(cl-pushnew 'grugru package-selected-packages)
(cl-pushnew 'gruvbox-theme package-selected-packages)
(cl-pushnew 'habamax-theme package-selected-packages)
(cl-pushnew 'helm package-selected-packages)
(cl-pushnew 'helm-system-packages package-selected-packages)
(cl-pushnew 'helpful package-selected-packages)
(cl-pushnew 'hemera-theme package-selected-packages)
(cl-pushnew 'hide-mode-line package-selected-packages)
(cl-pushnew 'highlight-indent-guides package-selected-packages)
(cl-pushnew 'highlight-quoted package-selected-packages)
(cl-pushnew 'htmlize package-selected-packages)
(cl-pushnew 'humanoid-themes package-selected-packages)
(cl-pushnew 'hungry-delete package-selected-packages)
(cl-pushnew 'hy-mode package-selected-packages)
(cl-pushnew 'hydra package-selected-packages)
(cl-pushnew 'immaterial-theme package-selected-packages)
(cl-pushnew 'leuven-theme package-selected-packages)
(cl-pushnew 'lgr package-selected-packages)
(cl-pushnew 'lispy package-selected-packages)
(cl-pushnew 'lispyville package-selected-packages)
(cl-pushnew 'loopy package-selected-packages)
(cl-pushnew 'lorem-ipsum package-selected-packages)
(cl-pushnew 'lua-mode package-selected-packages)
(cl-pushnew 'macrostep package-selected-packages)
(cl-pushnew 'marginalia package-selected-packages)
(cl-pushnew 'markdown-mode package-selected-packages)
(cl-pushnew 'material-theme package-selected-packages)
(cl-pushnew 'minimal-theme package-selected-packages)
(cl-pushnew 'modus-themes package-selected-packages)
(cl-pushnew 'monkeytype package-selected-packages)
(cl-pushnew 'monokai-theme package-selected-packages)
(cl-pushnew 'nerd-icons package-selected-packages)
(cl-pushnew 'no-littering package-selected-packages)
(cl-pushnew 'noccur package-selected-packages)
(cl-pushnew 'notmuch package-selected-packages)
(cl-pushnew 'one-themes package-selected-packages)
(cl-pushnew 'orderless package-selected-packages)
(cl-pushnew 'org-appear package-selected-packages)
(cl-pushnew 'org-bookmark-heading package-selected-packages)
(cl-pushnew 'org-fancy-priorities package-selected-packages)
(cl-pushnew 'org-ml package-selected-packages)
(cl-pushnew 'org-superstar package-selected-packages)
(cl-pushnew 'org-tidy package-selected-packages)
(cl-pushnew 'orglink package-selected-packages)
(cl-pushnew 'password-store package-selected-packages)
(cl-pushnew 'pcre2el package-selected-packages)
(cl-pushnew 'php-mode package-selected-packages)
(cl-pushnew 'polymode package-selected-packages)
(cl-pushnew 'pomodoro package-selected-packages)
(cl-pushnew 'rainbow-delimiters package-selected-packages)
(cl-pushnew 'rainbow-mode package-selected-packages)
(cl-pushnew 'redacted package-selected-packages)
(cl-pushnew 'restart-emacs package-selected-packages)
(cl-pushnew 's package-selected-packages)
(cl-pushnew 'sdcv package-selected-packages)
(cl-pushnew 'sly package-selected-packages)
(cl-pushnew 'smartparens package-selected-packages)
(cl-pushnew 'solarized-theme package-selected-packages)
(cl-pushnew 'spacemacs-theme package-selected-packages)
(cl-pushnew 'ssh-agency package-selected-packages)
(cl-pushnew 'standard-themes package-selected-packages)
(cl-pushnew 'stripe-buffer package-selected-packages)
(cl-pushnew 'sudo-edit package-selected-packages)
(cl-pushnew 'super-save package-selected-packages)
(cl-pushnew 'tango-plus-theme package-selected-packages)
(cl-pushnew 'telephone-line package-selected-packages)
(cl-pushnew 'tempel package-selected-packages)
(cl-pushnew 'tempel-collection package-selected-packages)
(cl-pushnew 'textsize package-selected-packages)
(cl-pushnew 'transpose-frame package-selected-packages)
(cl-pushnew 'transwin package-selected-packages)
(cl-pushnew 'ts package-selected-packages)
(cl-pushnew 'vc-auto-commit package-selected-packages)
(cl-pushnew 'vertico package-selected-packages)
(cl-pushnew 'w3m package-selected-packages)
(cl-pushnew 'web-mode package-selected-packages)
(cl-pushnew 'wgrep package-selected-packages)
(cl-pushnew 'which-key package-selected-packages)
(cl-pushnew 'ws-butler package-selected-packages)
(cl-pushnew 'xr package-selected-packages)
(cl-pushnew 'yeetube package-selected-packages)
(cl-pushnew 'zoutline package-selected-packages)

(cl-pushnew '(evil-easymotion :url "https://github.com/Luis-Henriquez-Perez/evil-easymotion" :branch "master") package-vc-selected-packages)
(cl-pushnew '(spaceline :url "https://github.com/Luis-Henriquez-Perez/spaceline" :branch "add-evil-operator-state-face") package-vc-selected-packages)
(cl-pushnew '(outli :url "https://github.com/jdtsmith/outli") package-vc-selected-packages)
(cl-pushnew '(on :url "https://github.com/ajgrf/on.el") package-vc-selected-packages)
(cl-pushnew '(zone-matrix :url "https://github.com/ober/zone-matrix" :branch "master") package-vc-selected-packages)

;; The function `package-install-selected-packages' does not activate the
;; packages which causes a problem fo rme.
(unless (bound-and-true-p package--initialized)
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

(let ((refreshed-contents-p nil))
  (dolist (package package-selected-packages)
    (cond ((assq package package-archive-contents)
           (unless (package-installed-p package)
             (unless refreshed-contents-p
               (package-refresh-contents)
               (setq refreshed-contents-p (not refreshed-contents-p)))
             (message "package is not installed %s package" package)
             (with-demoted-errors "%S" (package-install package 'dont-select)))
           (unless (package-installed-p package)
             (message "Failed to install package `%s'" package)))
          (t
           (message "Package %s is not available." package)))))

(package-vc-install-selected-packages)
;;; provide
(provide 'init-package)
;;; init-package.el ends here
