;;; init-consult.el --- initialize consult -*- lexical-binding: t; -*-
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
(require 'base)

(opt! consult-preview-key nil)
(opt! consult-fontify-preserve nil)

(alt! display-buffer oo-pop-to-buffer consult)

(autoload #'oo-pop-to-buffer "oo-commands" nil t 'function)

(alt! imenu consult-imenu consult)
(alt! pop-to-buffer oo-pop-to-buffer consult)
(alt! switch-to-buffer consult-buffer consult)
(alt! yank-pop consult-yank-pop consult)
(alt! apropos consult-apropos consult)
(alt! man consult-man consult)

;; https://andreyor.st/posts/2022-07-16-project-el-enhancements/
;; The function `consult-grep' is not detecting my emacs project.  It defers to
;; `project.el' and.
(defun project-find-root (path)
  "Search up the PATH for `project-root-markers'."
  (awhen! (vc-root-dir)
    (cons 'transient (expand-file-name root))))

(add-to-list 'project-find-functions #'project-find-root)
;;; provide
(provide 'init-consult)
;;; init-consult.el ends here
