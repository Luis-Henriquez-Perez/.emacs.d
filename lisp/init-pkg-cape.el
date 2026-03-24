;;; init-pkg-cape.el --- Initialize cape -*- lexical-binding: t; -*-
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
;; Initialize cape.
;;
;;; Code:
(o-package-declare 'cape)

(autoload 'cape-dabbrev "cape" nil nil 'function)
(autoload 'cape-file "cape" nil nil 'function)

(o-local-add-hook org-mode completion-at-point-functions #'cape-dabbrev)
(o-local-add-hook org-mode completion-at-point-functions #'cape-file)

(o-local-add-hook prog-mode completion-at-point-functions #'cape-dabbrev)
(o-local-add-hook prog-mode completion-at-point-functions #'cape-file)
;;; provide
(provide 'init-pkg-cape)
;;; init-pkg-cape.el ends here
