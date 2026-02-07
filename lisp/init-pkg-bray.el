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

(o-opt bray-state-default 'normal)
(o-opt bray-state-map-enabled t)

(defvar o-bray-state-motion-enter-hook nil)
(defvar o-bray-state-motion-exit-hook nil)

(defvar o-bray-state-normal-enter-hook nil)
(defvar o-bray-state-normal-exit-hook nil)

(defvar o-bray-state-insert-enter-hook nil)
(defvar o-bray-state-insert-exit-hook nil)

(defvar o-bray-state-visual-enter-hook nil)
(defvar o-bray-state-visual-exit-hook nil)

(defvar-keymap o-bray-state-motion-map)

(defvar-keymap o-bray-state-insert-map)

(defvar-keymap o-bray-state-normal-map)

(defvar-keymap o-bray-state-visual-map)

(defun o-bray-unbound-key ()
  "Indicate that current key is unbound."
  (interactive)
  (let ((keys (this-command-keys-vector)))
    (message "Unbound Key: %s" (format-kbd-macro keys))))

(defun o-bray-dwim-escape ()
  "Exit out of whatever is happening after escape.
Enter normal state.  If in minibuffer, exit the minibuffer.  When in a
non-readonly file buffer, save the buffer."
  (interactive)
  (when (bound-and-true-p evil-mode)
    (evil-normal-state 1))
  (bray-state-stack-pop)
  (cond
   ((minibuffer-window-active-p (minibuffer-window))
    (if (or defining-kbd-macro executing-kbd-macro)
        (minibuffer-keyboard-quit)
      (abort-recursive-edit)))
   ((or defining-kbd-macro executing-kbd-macro)
    nil)
   (t
    (when (and (not buffer-read-only) (buffer-file-name) (buffer-modified-p))
      (save-buffer))
    (keyboard-quit))))

(defun o-bray-ensure ()
  "Ensure bray is enabled in all buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (unless bray-mode
        (bray-mode 1)))))

(defun o-bray-init ()
  (cond
   (bray-mode
    (add-hook 'post-command-hook #'o-bray-update-cursor-color)
    (add-hook 'buffer-list-update-hook #'o-bray-ensure)
    (add-hook 'after-change-major-mode-hook #'o-bray-dwim)
    (add-hook 'activate-mark-hook #'o-bray-mark-hook-activate)
    (add-hook 'deactivate-mark-hook #'o-bray-mark-hook-deactivate)
    ;; (add-hook 'bray-mode-hook #'o-bray-init-visual-state)
    )
   (t
    (remove-hook 'post-command-hook #'o-bray-update-cursor-color)
    (remove-hook 'buffer-list-update-hook #'o-bray-ensure)
    (remove-hook 'after-change-major-mode-hook #'o-bray-dwim)
    (remove-hook 'activate-mark-hook #'o-bray-mark-hook-activate)
    (remove-hook 'deactivate-mark-hook #'o-bray-mark-hook-deactivate))))

(defun o-bray-enter-insert-state ()
  (bray-state-set 'insert))

(add-hook 'minibuffer-setup-hook #'o-bray-enter-insert-state)

(defun o-bray-dwim ()
  (when (not (derived-mode-p 'special-mode))
    (unless bray-mode
      (bray-mode 1))))

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
          "#8b2252"))))))

(o-opt bray-state-definitions
       (list
        (list
         :id 'motion
         :cursor-type 'box
         :lighter "<M>"
         :keymaps (list (cons t 'o-bray-state-motion-map))
         :enter-hook 'o-bray-state-motion-enter-hook
         :exit-hook 'o-bray-state-motion-exit-hook)
        (list
         :id 'normal
         ;; Define.
         :cursor-type 'box
         :lighter "<N>"
         :keymaps (list (cons t 'o-bray-state-motion-map)
                        (cons t 'o-bray-state-normal-map))
         :enter-hook 'o-bray-state-normal-enter-hook
         :exit-hook 'o-bray-state-normal-exit-hook)
        (list
         :id 'visual
         :cursor-type 'box
         :lighter "<V>"
         :keymaps
         (list (cons t 'o-bray-state-normal-map) (cons t 'o-bray-state-visual-map))

         :enter-hook 'o-bray-state-visual-enter-hook
         :exit-hook 'o-bray-state-visual-exit-hook)
        (list
         :id 'insert
         :cursor-type 'bar
         :lighter "<I>"
         :keymaps (list (cons t 'o-bray-state-insert-map))

         :enter-hook 'o-bray-state-insert-enter-hook
         :exit-hook 'o-bray-state-insert-exit-hook

         ;; Optional.
         :is-input t)))

(defun o-bray-mark-hook-activate ()
  "Activate visual state."
  (when (bray-state-derived-p 'normal)
    (bray-state-stack-push 'visual)))

(defun o-bray-mark-hook-deactivate ()
  "Activate visual state."
  (when (bray-state-derived-p 'visual)
    (bray-state-stack-pop)))

(add-hook 'bray-mode-hook #'o-bray-init)
(add-hook 'emacs-startup-hook #'o-bray-ensure 80)

(o-require-after-load 'bray 'init-after-bray)
;;; provide
(provide 'init-pkg-bray)
;;; init-pkg-bray.el ends here
