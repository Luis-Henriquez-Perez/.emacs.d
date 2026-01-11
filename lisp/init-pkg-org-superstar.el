;;; init-pkg-org-superstar.el --- Initialize `org-superstar` -*- lexical-binding: t; -*-
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
;; Initialize `org-superstar`.
;;
;;; Code:
(require 'init-core)

(autoload 'org-superstar-mode "org-superstar" nil nil 'function)
(add-hook 'org-mode-hook #'org-superstar-mode)

(o-opt org-superstar-leading-bullet ?\s)
(o-opt org-superstar-special-todo-items t)
;; (o-opt org-superstar-special-todo-items 'hide)
;; (o-opt org-superstar-todo-bullet-alist
;;       `(("TODO" . ,(seq-first "☐"))
;;         ("CANCELLED" . ,(seq-first "✘"))
;;         ("DONE" . ,(seq-first "✔"))))
(o-opt org-superstar-todo-bullet-alist `(("TODO" . ,(seq-first (format "%s" (nerd-icons-mdicon "nf-md-checkbox_blank_outline"))))
                                        ("DONE" . ,(seq-first (format "%s" (nerd-icons-mdicon "nf-md-checkbox_outline"))))
                                        ("CANCELLED" . ,(seq-first (format "%s" (nerd-icons-mdicon "nf-md-cancel"))))))
;;; provide
(provide 'init-pkg-org-superstar)
;;; init-pkg-org-superstar.el ends here
