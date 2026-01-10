;;; init-highlight-indent-guides.el --- initialize highlight-indent-guides -*- lexical-binding: t; -*-
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
;; Initialize highlight-indent-guides.
;;
;;; Code:
(require 'init-core)

(autoload 'highlight-indent-guides-mode "highlight-indent-guides-mode" nil nil 'function)
(add-hook 'mhtml-mode-hook #'highlight-indent-guides-mode)

(o-opt highlight-indent-guides-method 'character)
;;; provide
(provide 'init-highlight-indent-guides)
;;; init-highlight-indent-guides.el ends here
