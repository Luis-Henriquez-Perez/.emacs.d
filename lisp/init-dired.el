;;; init-dired.el --- initialize dired -*- lexical-binding: t; -*-
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
;; Initialize dired.
;;
;;; Code:
(require 'base)

;; This was specifically inspired by dired buffers not updating new files when I
;; switch to them.
(defhook! oo-refresh-buffer-h (buffer-list-update-hook)
  (when (and (buffer-file-name) (derived-mode-p 'dired-mode))
    (revert-buffer :ignore-auto :noconfirm :preseve-modes)))

;; Whenever
(opt! dired-deletion-confirmer #'always)
(hook! dired-mode-hook hl-line-mode)
;; This omits:
;; 1. Backup files
;; 2. Previous and current directory.
;; 3. Dotfiles
(setq dired-omit-files "\\`\\.?#\\|\\`\\.\\.?\\'\\|^\\..*$")
(hook! dired-mode-hook dired-omit-mode)
;; By default hide details.
(hook! dired-mode-hook dired-hide-details-mode)
(opt! dired-clean-confirm-killing-deleted-buffers nil)
(opt! dired-recursive-copies 'always)
(opt! dired-recursive-deletes 'always)

(bind! oo-app-map "d" #'dired-jump)
;; Dired is very picky about when these bindings happen.  It is the only package
;; I have had that is that picky.  I have noticed that unlike every other
;; package I have tried dired bindings do not work by trying to set them when
;; `dired-mode-map' is bound.  You need to use (eval-after-load 'dired ...).
;; Also, even if you have the `eval-after-load' it work work from the
;; `oo-after-load-dired' file--do not ask me why.  Again, only package I have
;; had this happen with.
(bind! (n m) dired-mode-map "h" #'dired-up-directory)
(bind! (n m) dired-mode-map "l" #'dired-find-file)
(bind! (n m) dired-mode-map "RET" #'dired-find-file)
;;; provide
(provide 'init-dired)
;;; init-dired.el ends here
