;;; init-pkg-smart-mark.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; This pkg makes it so that exiting marking a thing will restore point to where
;; it was before marking.  As things stand if you mark something and exit (with
;; C-g) point will be.  For me this means if I ever changed my mind about
;; marking or if I just wanted to apply some operation to a thing and then
;; continue with what I was doing, I was jarred by my point having moved and
;; inconvenienced by having to return it to where it was.
;;
;;; Code:
(o-declare-package 'smart-mark)

(o-opt smart-mark-mark-functions (cl-adjoin #'meep-region-mark-word smart-mark-mark-functions))
(o-opt smart-mark-mark-functions (cl-adjoin #'meep-region-mark-symbol smart-mark-mark-functions))
(o-opt smart-mark-mark-functions (cl-adjoin #'o-mark-delim-inner smart-mark-mark-functions))
(o-opt smart-mark-mark-functions (cl-adjoin #'o-mark-delim-outer smart-mark-mark-functions))
(o-opt smart-mark-mark-functions (cl-adjoin #'meep-region-mark-paragraph-inner smart-mark-mark-functions))
(o-opt smart-mark-mark-functions (cl-adjoin #'meep-region-mark-paragraph-outer smart-mark-mark-functions))
(o-opt smart-mark-mark-functions (cl-adjoin #'meep-region-mark-sentence-inner smart-mark-mark-functions))
(o-opt smart-mark-mark-functions (cl-adjoin #'meep-region-mark-sentence-outer smart-mark-mark-functions))
(o-opt smart-mark-mark-functions (cl-adjoin #'meep-region-mark-defun-inner smart-mark-mark-functions))
(o-opt smart-mark-mark-functions (cl-adjoin #'meep-region-mark-defun-outer smart-mark-mark-functions))
(o-opt smart-mark-mark-functions (cl-adjoin #'meep-region-mark-string-inner smart-mark-mark-functions))
(o-opt smart-mark-mark-functions (cl-adjoin #'meep-region-mark-string-outer smart-mark-mark-functions))
(o-opt smart-mark-mark-functions (cl-adjoin #'meep-region-mark-comment-inner smart-mark-mark-functions))
(o-opt smart-mark-mark-functions (cl-adjoin #'meep-region-mark-comment-outer smart-mark-mark-functions))
(o-opt smart-mark-mark-functions (cl-adjoin #'meep-region-mark-line-inner smart-mark-mark-functions))
(o-opt smart-mark-mark-functions (cl-adjoin #'meep-region-mark-line-outer smart-mark-mark-functions))
(o-opt smart-mark-mark-functions (cl-adjoin #'meep-region-mark-visual-line-inner smart-mark-mark-functions))
(o-opt smart-mark-mark-functions (cl-adjoin #'meep-region-mark-visual-line-outer smart-mark-mark-functions))

(add-hook 'o-first-input-hook #'smart-mark-mode)
;;; provide
(provide 'init-pkg-smart-mark)
;;; init-pkg-smart-mark.el ends here
