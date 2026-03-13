;;; init-pkg-bray.el --- Initialize bray -*- lexical-binding: t; -*-
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
;;
;;; Code:
(o-declare-package 'bray)

(push 'bray o-required-features)

(defvar bray-state-default)
(setq bray-state-default 'normal)
(defvar bray-state-map-enabled)
(setq bray-state-map-enabled t)

(defvar o-bray-state-normal-enter-hook nil)
(defvar o-bray-state-normal-exit-hook nil)

(defvar o-bray-state-insert-enter-hook nil)
(defvar o-bray-state-insert-exit-hook nil)

(defvar o-bray-state-visual-enter-hook nil)
(defvar o-bray-state-visual-exit-hook nil)

(defvar o-bray-state-motion-enter-hook nil)
(defvar o-bray-state-motion-exit-hook nil)

(defvar o-bray-state-insert-map o-keymap-state-insert)

(defvar o-bray-state-normal-map o-keymap-state-normal)

(defvar o-bray-state-visual-map o-keymap-state-visual)

(defvar o-bray-state-motion-map o-keymap-state-motion)

(defun o-bray-dwim ()
  (bray-mode 1)
  ;; (let ((special-modes '(special-mode gud-mode term-mode inferior-emacs-lisp-mode dired-mode)))
  ;;   (unless (derived-mode-p special-modes)
  ;;     (bray-mode 1)))
  )

(defun o-hook--enable-bray ()
  (interactive)
  (add-hook 'post-command-hook #'o-bray-update-cursor-color)
  (add-hook 'after-change-major-mode-hook #'o-bray-dwim)
  (add-hook 'activate-mark-hook #'o-hook--bray-enable-visual-state)
  (add-hook 'deactivate-mark-hook #'o-hook--bray-disable-visual-state)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (o-bray-dwim))))

(defun o-bray-disable ()
  (interactive)
  (remove-hook 'post-command-hook #'o-bray-update-cursor-color)
  (remove-hook 'after-change-major-mode-hook #'o-bray-dwim)
  (remove-hook 'activate-mark-hook #'o-hook--bray-enable-visual-state)
  (remove-hook 'deactivate-mark-hook #'o-hook--bray-disable-visual-state)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (bray-mode -1))))

(add-hook 'o-escape-hook #'bray-state-stack-pop)

(o-setq-mode-local minibuffer-mode bray-state-init 'insert)

(defun o-bray-update-cursor-color ()
  "Update the cursor color based on the current state."
  (let (fg)
    (pcase (bray-state)
      ('insert
       (setq fg (face-attribute font-lock-type-face :foreground))
       (set-cursor-color
        (if (stringp fg)
            fg
          "#228b22")))
      ('normal
       (setq fg (face-attribute font-lock-keyword-face :foreground))
       (set-cursor-color
        (if (stringp fg)
            fg
          "purple")))
      ('visual
       (setq fg (face-attribute font-lock-string-face :foreground))
       (set-cursor-color
        (if (stringp fg)
            fg
          "#8b2252")))
      ('motion
       ;; (setq fg (face-attribute font-lock-string-face :foreground))
       (set-cursor-color "blue"))
      )))

(defvar bray-state-definitions)
(setq bray-state-definitions
      '(( :id normal
          :cursor-type box
          :lighter "<N>"
          :keymaps ((t . o-bray-state-normal-map))
          :enter-hook o-bray-state-normal-enter-hook
          :exit-hook o-bray-state-normal-exit-hook)
        ( :id visual
          :cursor-type box
          :lighter "<V>"
          :keymaps ((t . o-bray-state-normal-map)
                    (t . o-bray-state-visual-map))
          :enter-hook o-bray-state-visual-enter-hook
          :exit-hook o-bray-state-visual-exit-hook)
        ( :id insert
          :cursor-type bar
          :lighter "<I>"
          :keymaps ((t . o-bray-state-insert-map))
          :enter-hook o-bray-state-insert-enter-hook
          :exit-hook o-bray-state-insert-exit-hook
          :is-input t)
        ( :id motion
          :cursor-type box
          :lighter "<M>"
          :keymaps ((t . o-bray-state-motion-map))
          :enter-hook o-bray-state-motion-enter-hook
          :exit-hook o-bray-state-motion-exit-hook
          :is-input t)
        ))

(defun o-hook--bray-enable-visual-state ()
  "Enable visual state."
  (when (bray-state-derived-p 'normal)
    (bray-state-stack-push 'visual)))

(defun o-hook--bray-disable-visual-state ()
  "Disable visual state."
  (when (bray-state-derived-p 'visual)
    (bray-state-stack-pop)))

(add-hook 'after-init-hook #'o-hook--enable-bray)

(o-require-after-load 'bray 'init-after-bray)
;;; provide
(provide 'init-pkg-bray)
;;; init-pkg-bray.el ends here
