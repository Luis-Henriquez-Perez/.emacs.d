;;; 130-init-dired.el --- initialize dired -*- lexical-binding: t; -*-
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
;; Initialize dired.
;;
;;; Code:
(require '050-base)

(add-hook 'dired-mode-hook #'hl-line-mode)
(add-hook 'dired-mode-hook #'dired-omit-mode)
;; By default hide details.
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

(opt! dired-deletion-confirmer #'always)
;; This omits:
;; 1. Backup files
;; 2. Previous and current directory.
;; 3. Dotfiles
(opt! dired-omit-files "\\`\\.?#\\|\\`\\.\\.?\\'\\|^\\..*$")
(opt! dired-clean-confirm-killing-deleted-buffers nil)
(opt! dired-recursive-copies 'always)
(opt! dired-recursive-deletes 'always)

(nmap dired-mode-map "h" #'dired-up-directory)
(nmap dired-mode-map "l" #'dired-find-file)
(nmap dired-mode-map "RET" #'dired-find-file)
(nmap dired-mode-map "o" #'dired-omit-mode)
;;; provide
(provide '130-init-dired)
;;; 130-init-dired.el ends here
