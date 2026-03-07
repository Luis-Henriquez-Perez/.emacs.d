;;; init-pkg-recentf.el --- initialize recentf -*- lexical-binding: t; -*-
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
(require 'init-core)

(push 'recentf o-required-features)

(add-hook 'after-init-hook #'recentf-mode)

(defvar recentf-save-file)
(defvar recentf-max-saved-items)
(defvar recentf-filename-handlers)
(defvar recentf-exclude)

(setq recentf-save-file (expand-file-name "recentf-save.el" o-var-dir))
(setq recentf-max-saved-items nil)

(advice-add #'recentf-save-list :before #'recentf-cleanup)
(advice-add #'recentf-save-list :around #'o-advice--silence-output)
(advice-add #'recentf-cleanup   :around #'o-advice--silence-output)
(advice-add #'recentf-mode      :around #'o-advice--silence-output)

(o-after recentf
  (add-to-list 'recentf-filename-handlers #'file-truename)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name)
  (add-to-list 'recentf-filename-handlers #'substring-no-properties)

  (add-to-list 'recentf-exclude (regexp-quote (recentf-expand-file-name o-etc-dir)))
  (add-to-list 'recentf-exclude (regexp-quote (recentf-expand-file-name o-var-dir)))
  (add-to-list 'recentf-exclude (lambda (file) (not (file-exists-p file)))))

(defun o-recentf--update-recentf-list-maybe ()
  "Update the recentf list just before killing a buffer."
  (o-awhen (buffer-file-name)
    (recentf-add-file it)
    (run-with-idle-timer 5 nil #'recentf-save-list)))

(add-hook 'kill-buffer-hook #'o-recentf--update-recentf-list-maybe)
;;; provide
(provide 'init-pkg-recentf)
;;; init-pkg-recentf.el ends here
