;;; init-project.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
(require 'base)

(opt! project-vc-include-untracked nil)
;; https://andreyor.st/posts/2022-07-16-project-el-enhancements/
;; The function `consult-grep' is not detecting my emacs project.  It defers to
;; `project.el' and.
(defun oo-project-find-root (path)
  "Search up the PATH for `project-root-markers'."
  (set! default-directory path)
  (awhen! (vc-root-dir)
    (cons 'transient (expand-file-name it))))

(add-to-list 'project-find-functions #'oo-project-find-root)
;;; provide
(provide 'init-project)
;;; init-project.el ends here
