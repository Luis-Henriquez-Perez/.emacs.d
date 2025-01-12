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
(oo-call-after-load 'dired (apply-partially #'require 'nerd-icons))
;; Do not touch the mode-line.
(advice-add 'dirvish--setup-mode-line :override #'ignore)
;; (setq nerd-icons-scale-factor 1.0)
(opt! dirvish-use-mode-line t)
(opt! dirvish-mode-line-format nil)
(opt! dirvish-use-header-line nil)
(opt! dirvish-attributes '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
(opt! dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group")

(opt! dirvish-layout-recipes (list '(0 0 0.8)
                                   '(0 0 0.4)
                                   dirvish-default-layout))
;; dirvish-yank-overwrite-existing-files 'never
;; dirvish-attributes '(all-the-icons file-size vc-state symlink-arrow)
(opt! dirvish-yank-new-name-style 'append-to-filename)
;; dirvish-yank-new-name-style 'append-to-ext
(opt! dirvish-mode-line-format nil)
;;; provide
(provide 'init-dirvish)
;;; init-dirvish.el ends here
