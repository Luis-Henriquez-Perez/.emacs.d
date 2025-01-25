;;; oo-autoloads.el --- autoloads for lisp directory -*- lexical-binding: t; -*-
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
;; WITHoUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FoR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This file contains the autoloads for lisp directory.  I specifically write
;; them out by hand because I am not a fan of having to generate a file.  With
;; this out of the way, it is one less thing I have todo.
;;
;;; Code:
(autoload #'oo-set-font-face "oo-commands" nil t 'function)
(autoload #'oo-create-new-test-file "oo-commands" nil t 'function)
(autoload #'oo-create-new-config-file "oo-commands" nil t 'function)
(autoload #'oo-create-new-init-file "oo-commands" nil t 'function)
(autoload #'oo-kill-emacs-no-confirm "oo-commands" nil t 'function)
(autoload #'oo-open-emacs-config "oo-commands" nil t 'function)
(autoload #'oo-open-emacs-init-file "oo-commands" nil t 'function)
(autoload #'update-emacs-config "oo-commands" nil t 'function)
(autoload #'oo-new-buffer "oo-commands" nil t 'function)
(autoload #'oo-load-random-theme "oo-commands" nil t 'function)
(autoload #'oo-dwim-space "oo-commands" nil t 'function)
(autoload #'oo-sort-dwim "oo-commands" nil t 'function)
(autoload #'oo-open-emacs-lisp-dir "oo-commands" nil t 'function)
(autoload #'oo-split-window-below-and-focus "oo-commands" nil t 'function)
(autoload #'oo-split-window-right-and-focus "oo-commands" nil t 'function)
(autoload #'oo-pop-to-buffer "oo-commands" nil t 'function)
(autoload #'oo-dwim-narrow "oo-commands" nil t 'function)
;;; provide
(provide 'oo-autoloads)
;;; oo-autoloads.el ends here
