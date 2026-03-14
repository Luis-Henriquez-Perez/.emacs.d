;;; init-after-bray.el --- Configure bray -*- lexical-binding: t; -*-
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
(require 'init-core)
(require 'bray)
(require 'bray-state-map)
(require 'meep)
;; TODO: functions to bind
;; `join-line' - it is something that I do all the time, I did not know there was
;; a specific fn for it.
;; I want some way to.
;; `register-to-point'
;; `point-to-register'
;; `ffap'
;; `split-line' - the opposite of `join-line' though does not handle extra whitespace.
;; Change surrounding delimiters
;;;; magit
(o-setq-mode-local magit-mode bray-state-init 'motion)
;; Unfortunately I cannot use `bray-state-init' here because `git-commit-mode'
;; is not a major mode.
(add-hook 'git-commit-mode-hook #'o-hook--enable-bray-insert-state)
;;;; elpaca
(o-setq-mode-local elpaca-manager-mode bray-state-init 'motion)
;;;; org-capture
;; (o-bray-state-map-set 'normal 'eamcs-lisp-mode-map "x" #'lispy-delete)
(add-hook 'org-capture-mode-hook #'o-hook--enable-bray-insert-state)
;;;; helm
(o-after helm
  (bray-state-map-set 'insert helm-map "TAB" #'helm-next-line)
  ;; (bray-state-map-set 'insert 'helm-map [backtab] #'helm-previous-line)
  (bray-state-map-set 'insert helm-map "C-j" #'helm-next-line)
  (bray-state-map-set 'insert helm-map "C-k" #'helm-previous-line)
  (bray-state-map-set 'insert helm-map "C-a" #'helm-select-action)
  (bray-state-map-set 'insert helm-map "C-m" #'helm-toggle-visible-mark-forward)
  ;; (bray-state-map-set 'insert 'helm-map "RET" #'+helm-select-nth-action)
  ;; (bray-state-map-set 'insert 'helm-map "RET" #'+helm-select-nth-action)
  (bray-state-map-set 'insert helm-map "S-TAB" #'helm-mark-current-line)
  (bray-state-map-set 'insert helm-map "C-;" #'ace-jump-helm-line))
;;;; vertico
(o-after vertico
  (bray-state-map-set 'insert vertico-map "<escape>" #'o-dwim-escape)
  (bray-state-map-set 'insert vertico-map "C-j" #'vertico-next)
  (bray-state-map-set 'insert vertico-map "C-k" #'vertico-previous)
  (bray-state-map-set 'insert vertico-map "C-n" #'vertico-scroll-up)
  (bray-state-map-set 'insert vertico-map "C-p" #'vertico-scroll-down)
  (bray-state-map-set 'insert vertico-map "TAB" #'vertico-next)
  (bray-state-map-set 'insert vertico-map ";" #'vertico-quick-exit)
  (bray-state-map-set 'insert vertico-map "C-;" #'vertico-quick-exit)
  (bray-state-map-set 'insert vertico-map "<backtab>" #'vertico-previous))
;;;; corfu
(o-after corfu
  (bray-state-map-set 'insert corfu-map "<tab>" #'corfu-next)
  ;; (bray-state-map-set 'insert 'corfu-map [backtab] #'corfu-previous)
  (bray-state-map-set 'insert corfu-map "S-TAB" #'corfu-previous)
  (bray-state-map-set 'insert corfu-map "C-;" #'corfu-quick-complete)
  (bray-state-map-set 'insert corfu-map "C-j" #'corfu-next)
  (bray-state-map-set 'insert corfu-map "C-k" #'corfu-previous)
  (bray-state-map-set 'insert corfu-map "C-p" #'corfu-previous)
  (bray-state-map-set 'insert corfu-map ";" #'corfu-quick-complete)
  (bray-state-map-set 'insert corfu-map "SPC" #'corfu-insert))
;;;; tempel
(o-after tempel
  (bray-state-map-set 'insert tempel-map "C-l" #'tempel-abort)
  (bray-state-map-set 'insert tempel-map "C-j" #'tempel-next)
  (bray-state-map-set 'insert tempel-map "C-k" #'tempel-previous)
  ;; (keymap-set tempel-map "TAB" #'tempel-next)
  (bray-state-map-set 'insert tempel-map "TAB" #'tempel-next)
  (bray-state-map-set 'insert tempel-map [backtab] #'tempel-previous))
;;;; dired
(o-after dired
  (bray-state-map-set 'normal dired-mode-map "h" #'dired-up-directory)
  (bray-state-map-set 'normal dired-mode-map "l" #'dired-find-file)
  (bray-state-map-set 'normal dired-mode-map "RET" #'dired-find-file)
  (bray-state-map-set 'normal dired-mode-map "o" #'dired-omit-mode))
;;;; macrostep
;; (o-defafter o-after--bray-define-macrostep-binds (macrostep)
;;   (let ((prefixes (list o-key-localleader-normal o-key-localleader-normal-alt)))
;;     (dolist (prefix prefixes)
;;       (bray-state-map-set 'normal emacs-lisp-mode-map (concat prefix "\s" "m") nil)
;;       (bray-state-map-set 'normal emacs-lisp-mode-map (concat prefix "\s" "m e") #'macrostep-expand)
;;       (bray-state-map-set 'normal emacs-lisp-mode-map (concat prefix "\s" "m c") #'macrostep-collapse)
;;       (bray-state-map-set 'normal emacs-lisp-mode-map (concat prefix "\s" "m C") #'macrostep-collapse-all)
;;       (bray-state-map-set 'normal emacs-lisp-mode-map (concat prefix "\s" "m a") #'macrostep-collapse-all))))
;;; provide
(provide 'init-after-bray)
;;; init-after-bray.el ends here
