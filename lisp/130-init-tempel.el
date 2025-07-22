;;; 130-init-tempel.el --- initialize tempel -*- lexical-binding: t; -*-
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
;; Initialize tempel.
;;
;;; Code:
;;;; requirements
(require '050-base)
;;;; autoloads
(autoload! tempel-complete "tempel")
(autoload! tempel-insert "tempel")
(autoload! tempel-expand "tempel")

(imap tempel-map "C-l" #'tempel-abort)
(imap tempel-map "C-j" #'tempel-next)
(imap tempel-map "C-k" #'tempel-previous)
(imap tempel-map "TAB" #'tempel-next)
(imap tempel-map [backtab] #'tempel-previous)
;;; provide
(provide '130-init-tempel)
;;; 130-init-tempel.el ends here
