;;; init-w3m.el --- Initialize nil -*- lexical-binding: t; -*-
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
;; Initialize nil.
;;
;;; Code:
(opt! w3m-profile-directory (expand-file-name "w3m" oo-var-dir))

;; Do not make new tabs.  Instead make new windows.
(opt! w3m-display-mode 'plain)

;; This is in the same spirit as `w3m-display-mode'.  The stock `w3m-find-file'
;; reuses an existing w3m buffer instead of creating a new one--this is not want
;; I want.
(defun +w3m-find-file-new-session (file)
  (interactive "fFilename: ")
  (w3m-goto-url-new-session (w3m-expand-file-name-as-url file)
		                    nil coding-system-for-read
		                    nil nil nil))
;;; provide
(provide 'init-w3m)
;;; init-w3m.el ends here
