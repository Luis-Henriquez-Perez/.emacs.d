;;; init-pkg-tempel.el --- initialize tempel -*- lexical-binding: t; -*-
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
(require 'init-core)
;;;; autoloads
(autoload 'tempel-complete "tempel" nil nil 'function)
(autoload 'tempel-insert "tempel" nil nil 'function)
(autoload 'tempel-expand "tempel" nil nil 'function)

(autoload 'o-tempel-expand-elisp-defun "init-after-tempel" nil nil 'function)
(autoload 'o-tempel-expand-elisp-defhook "init-after-tempel" nil nil 'function)
(autoload 'o-tempel-expand-elisp-defvar "init-after-tempel" nil nil 'function)
(autoload 'o-tempel-expand-elisp-message "init-after-tempel" nil nil 'function)
(autoload 'o-tempel-expand-elisp-message-var "init-after-tempel" nil nil 'function)
;;; provide
(provide 'init-pkg-tempel)
;;; init-pkg-tempel.el ends here
