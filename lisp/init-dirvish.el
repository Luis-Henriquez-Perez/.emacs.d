;;; init-dirvish.el --- Initialize dirvish -*- lexical-binding: t; -*-
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
;; Initialize dirvish.
;;
;;; Code:
(oo-call-after-load 'dired #'dirvish-override-dired-mode)
(oo-call-after-load 'dired (apply-partially #'require 'all-the-icons))
;; Do not touch the mode-line.
(advice-add 'dirvish--setup-mode-line :override #'ignore)
;; (setq nerd-icons-scale-factor 1.0)
(opt! dirvish-use-mode-line t)
(opt! dirvish-mode-line-format nil)
(opt! dirvish-use-header-line nil)
(opt! dirvish-attributes '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))
(opt! dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group")

(opt! dirvish-layout-recipes (list '(0 0 0.8)
                                   '(0 0 0.4)
                                   dirvish-default-layout))
;; dirvish-yank-overwrite-existing-files 'never
;; dirvish-attributes '(all-the-icons file-size vc-state symlink-arrow)
(opt! dirvish-yank-new-name-style 'append-to-filename)
;; dirvish-yank-new-name-style 'append-to-ext
(opt! dirvish-mode-line-format nil)

;; (set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil 'append)
;; (set-fontset-font t 'unicode (font-spec :family "file-icons") nil 'append)
;; (set-fontset-font t 'unicode (font-spec :family "Material Icons") nil 'append)
;; (set-fontset-font t 'unicode (font-spec :family "github-octicons") nil 'append)
;; (set-fontset-font t 'unicode (font-spec :family "FontAwesome") nil 'append)
;; (set-fontset-font t 'unicode (font-spec :family "Weather Icons") nil 'append)

;; (set-fontset-font t 'unicode "FontAwesome" nil 'prepend)
;; (set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil)
;;;; make a specific modeline for dirvish
;; (defhook! oo-set-dired-modeline-h ()
;;   (setq-local mode-line-format))

;; (telephone-line-defsegment* +telephone-line-buffer-segment ()
;;   (telephone-line-raw mode-line-buffer-identification t))
;; (opt! telephone-line-lhs
;;       '((evil   telephone-line-evil-tag-segment)
;;         (accent +telephone-line-vc-segment +telephone-line-read-only-segment +telephone-line-narrow-segment telephone-line-process-segment)
;;         (nil    +telephone-line-buffer-segment)))

;; (opt! telephone-line-rhs
;;       '((nil    telephone-line-misc-info-segment)
;;         (accent +telephone-line-major-mode-segment +telephone-line-kbd-macro-segment)
;;         (evil +telephone-line-current-time-segment)))

;; '(:left
;;   (" " file-modes " " file-link-number " " file-user ":" file-group " "
;;    symlink omit vc-info)
;;   :right
;;   (sort yank index))
;; (opt! dirvish-header-line-format)
;; '(:left
;;   (path symlink)
;;   :right
;;   (free-space))
;; :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
;; (("C-c f" . dirvish-fd)
;;  :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
;;  ("a"   . dirvish-quick-access)
;;  ("f"   . dirvish-file-info-menu)
;;  ("y"   . dirvish-yank-menu)
;;  ("N"   . dirvish-narrow)
;;  ("^"   . dirvish-history-last)
;;  ("h"   . dirvish-history-jump) ; remapped `describe-mode'
;;  ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
;;  ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
;;  ("TAB" . dirvish-subtree-toggle)
;;  ("M-f" . dirvish-history-go-forward)
;;  ("M-b" . dirvish-history-go-backward)
;;  ("M-l" . dirvish-ls-switches-menu)
;;  ("M-m" . dirvish-mark-menu)
;;  ("M-t" . dirvish-layout-toggle)
;;  ("M-s" . dirvish-setup-menu)
;;  ("M-e" . dirvish-emerge-menu)
;;  ("M-j" . dirvish-fd-jump))
;;; provide
(provide 'init-dirvish)
;;; init-dirvish.el ends here
