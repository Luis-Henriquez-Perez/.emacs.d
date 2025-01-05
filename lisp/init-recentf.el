;;; init-recentf.el --- initialize recentf -*- lexical-binding: t; -*-
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
;; Initialize `recentf'.
;;
;;; Code:
(require 'base)
(require 'recentf)

(hook! emacs-startup-hook recentf-mode)

(advice-add #'recentf-save-list :before #'recentf-cleanup)
(advice-add #'recentf-save-list :around #'oo-funcall-quietly)
(advice-add #'recentf-cleanup   :around #'oo-funcall-quietly)
(advice-add #'recentf-mode      :around #'oo-funcall-quietly)

(adjoin! recentf-filename-handlers #'file-truename)
(adjoin! recentf-filename-handlers #'abbreviate-file-name)
(adjoin! recentf-filename-handlers #'substring-no-properties)

(adjoin! recentf-exclude (regexp-quote (recentf-expand-file-name oo-etc-dir)))
(adjoin! recentf-exclude (regexp-quote (recentf-expand-file-name oo-var-dir)))
(adjoin! recentf-exclude (lambda (file) (not (file-exists-p file))))

(setq recentf-max-saved-items nil)
;;;; always keep important files in recentf-list
(recentf-push (recentf-expand-file-name "~/.xinitrc"))
(each! (directory-files (expand-file-name "lisp/" user-emacs-directory) :full)
  (recentf-push it))
(recentf-push (recentf-expand-file-name "~/.bashrc"))
(recentf-push (recentf-expand-file-name "~/.xinitrc"))
(recentf-push (recentf-expand-file-name "~/.config/init.el"))
(recentf-push (recentf-expand-file-name "~/.config/qtile/config.py"))
(recentf-push (recentf-expand-file-name "~/.local/share/qtile/qtile.log"))
(recentf-push (recentf-expand-file-name "/etc/xdg/awesome/rc.lua"))
;;; provide
(provide 'init-recentf)
;;; init-recentf.el ends here
