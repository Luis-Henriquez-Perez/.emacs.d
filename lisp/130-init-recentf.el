;;; 130-init-recentf.el --- initialize recentf -*- lexical-binding: t; -*-
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
(require! "^0[01]")
(require 'recentf)

(add-hook 'emacs-startup-hook #'recentf-mode)

(opt! recentf-save-file (expand-file-name "recentf-save.el" oo-cache-dir))

(advice-add #'recentf-save-list :before #'recentf-cleanup)
(advice-add #'recentf-save-list :around #'oo-call-quietly-a)
(advice-add #'recentf-cleanup   :around #'oo-call-quietly-a)
(advice-add #'recentf-mode      :around #'oo-call-quietly-a)

(add-to-list 'recentf-filename-handlers #'file-truename)
(add-to-list 'recentf-filename-handlers #'abbreviate-file-name)
(add-to-list 'recentf-filename-handlers #'substring-no-properties)

(add-to-list 'recentf-exclude (regexp-quote (recentf-expand-file-name oo-etc-dir)))
(add-to-list 'recentf-exclude (regexp-quote (recentf-expand-file-name oo-cache-dir)))
(add-to-list 'recentf-exclude (lambda (file) (not (file-exists-p file))))

(defun recentf|update-recentf-list-maybe ()
  "Update the recentf list just before killing a buffer."
  (awhen! (buffer-file-name)
    (recentf-add-file it)
    (run-with-idle-timer 5 nil #'recentf-save-list)))

(add-hook 'kill-buffer-hook #'recentf|update-recentf-list-maybe)

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
(provide '130-init-recentf)
;;; 130-init-recentf.el ends here
