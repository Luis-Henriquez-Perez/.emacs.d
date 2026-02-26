;;; init-pkg-modaled.el --- Initialize modaled -*- lexical-binding: t; -*-
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
;; This is.
;;
;;; Code:
(require 'modaled)
(require 'meep)

(defvar modaled-normal-state-keymap o-state-normal-keymap)
(defvar modaled-visual-state-keymap o-state-visual-keymap)
(defvar modaled-insert-state-keymap o-state-insert-keymap)

(defun o-advice--modaled-set-state (state)
  (modaled-set-state (symbol-name state)))

(setq meep-state-insert 'insert)
(advice-add 'bray-state-stack-push :override #'o-advice--modaled-set-state)
(advice-add 'bray-state-set :override #'o-advice--modaled-set-state)
(advice-add 'bray-state-set :override #'o-advice--modaled-set-state)

(modaled-define-state "normal"
  :lighter "[NOR]"
  :cursor-type 'box)

(defun o-dwim-escape ()
  "Exit out of whatever is happening after escape.
Enter normal state.  If in minibuffer, exit the minibuffer.  When in a
non-readonly file buffer, save the buffer."
  (interactive)
  (when (require 'modaled)
    (modaled-set-state "normal"))
  (cond ((minibuffer-window-active-p (minibuffer-window))
		 (if (or defining-kbd-macro executing-kbd-macro)
			 (minibuffer-keyboard-quit)
           (abort-recursive-edit)))
		((or defining-kbd-macro executing-kbd-macro) nil)
        (t
         (when (and (not buffer-read-only)
                    (buffer-file-name)
                    (buffer-modified-p))
           (save-buffer))
		 (keyboard-quit)))
  (when (require 'modaled)
    (modaled-set-state "normal")))

;; (keymap-set modaled-normal-state-keymap o-key-leader-normal #'o-leader-map)

;; (defvar-keymap modaled-normal-state-keymap
;;   "<escape>" #'o-dwim-escape)

;; (add-hook 'modaled-normal-state-mode-hook (apply-partially #'set-cursor-color "orange"))
;; (add-hook 'modaled-insert-state-mode-hook (apply-partially #'set-cursor-color "green"))
(modaled-define-state "insert"
  :sparse t
  ;; insert state must be no-suppress to support inserting char
  :no-suppress t
  :cursor-type 'bar
  :lighter "[INS]")

(keymap-set modaled-insert-state-keymap "<escape>" #'o-dwim-escape)

;; set init state using a function
(setq modaled-init-state-fn (lambda () "normal"))

(modaled-define-substate "vertico"
  :sparse t
  :no-suppress t)

(modaled-enable-substate-on-state-change "vertico"
  :states '("insert")
  :pred #'minibufferp)

(modaled-get-substate-mode "vertico")
;; => modaled-vertico-substate-mode
(defvar-keymap modaled-org-substate-keymap
  "C-n" #'vertico-scroll-up
  "C-p" #'vertico-scroll-down
  "TAB" #'vertico-next
  "C-k" #'vertico-previous
  "C-j" #'vertico-next
  ";" #'vertico-quick-exit
  "C-;" #'vertico-quick-exit
  "<backtab>" #'vertico-previous
  "C-o" #'embark-act)

(modaled-define-keys
  :substates '("vertico")
  :bind
  '(("C-n" . vertico-scroll-up)
    ("TAB" . vertico-next)
    (";" . vertico-quick-exit)
    ("C-;" . vertico-quick-exit)))

(modaled-define-substate "org")
(modaled-define-keys
  :substates '("org")
  :bind
  '(("j" . org-next-visible-heading)
    ("k" . org-previous-visible-heading)))

;; (defun o-state-changed-p ()
;;   ""
;;   (message "State changed...")
;;   t)
(modaled-enable-substate-on-state-change
  "org"
  :states '("normal")
  :major '(org-mode)
  ;; :pred #'o-state-changed-p
  )

;; update after major mode changes
(add-hook 'after-change-major-mode-hook
          (lambda ()
            (setq modaled--initialized nil)
            (if (minibufferp)
                (modaled-set-state "insert")
              (modaled-initialize))))
;; update on creation (no major mode change yet)
(add-hook 'buffer-list-update-hook #'modaled-initialize-all-buffers)
;; enable it for all existing buffers
(modaled-initialize-all-buffers)
;; manually switch to it
(modaled-set-init-state)
;;; provide
(provide 'init-pkg-modaled)
;;; init-pkg-modaled.el ends here
