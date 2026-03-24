;;; init-pkg-consult.el --- initialize consult -*- lexical-binding: t; -*-
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
;; Initialize consult.
;;
;;; Code:
(require 'init-core)

(o-package-declare 'consult)

(o-opt consult-preview-key nil)
(o-opt consult-fontify-preserve nil)

(autoload 'o-pop-to-buffer "o-commands" nil nil 'function)
(o-remap-alt 'consult #'display-buffer   #'o-pop-to-buffer)
(o-remap-alt 'consult #'pop-to-buffer    #'o-pop-to-buffer)

(o-remap-alt 'consult #'imenu            #'consult-imenu)
(o-remap-alt 'consult #'switch-to-buffer #'consult-buffer)
(o-remap-alt 'consult #'yank-pop         #'consult-yank-pop)
(o-remap-alt 'consult #'apropos          #'consult-apropos)
(o-remap-alt 'consult #'man              #'consult-man)

;; (o-opt consult-project-function #'projectile-project-root)
(o-opt consult-project-function #'consult--default-project-function)
;; Do not load bookmarks as buffer sources.  It is expensive and unnecessary.
(o-opt consult-buffer-sources (remove 'consult--source-bookmark consult-buffer-sources))
;;; provide
(provide 'init-pkg-consult)
;;; init-pkg-consult.el ends here
