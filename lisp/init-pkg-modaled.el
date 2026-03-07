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

(defvar modaled-normal-state-keymap o-keymap-state-normal)
(defvar modaled-visual-state-keymap o-keymap-state-visual)
(defvar modaled-insert-state-keymap o-keymap-state-insert)

;; set init state using a function
(setq modaled-init-state-fn (lambda () (cond ((minibufferp) "insert")
                                             (t "normal"))))

(defun o-advice--modaled-set-state (state)
  (modaled-set-state (symbol-name state)))

;; (advice-add 'bray-state-stack-push :override #'o-advice--modaled-set-state)
;; (advice-add 'bray-state-set :override #'o-advice--modaled-set-state)
;; (advice-add 'bray-state-set :override #'o-advice--modaled-set-state)

(modaled-define-state "normal"
  :lighter "[NOR]"
  :cursor-type 'box)

(modaled-define-state "insert"
  :sparse t
  ;; insert state must be no-suppress to support inserting char
  :no-suppress t
  :cursor-type 'bar
  :lighter "[INS]")

(modaled-define-state "visual"
  :sparse t
  :no-suppress t
  :cursor-type 'box
  :lighter "[VIS]")

(defun o-hook--modaled-enable-visual-state ()
  "Activate visual state."
  (modaled-set-state "visual"))

(defun o-hook--modaled-disable-visual-state ()
  "Deactivate visual state."
  (modaled-set-state "normal"))

(add-hook 'activate-mark-hook #'o-hook--modaled-enable-visual-state)
(add-hook 'deactivate-mark-hook #'o-hook--modaled-disable-visual-state)

(modaled-define-substate "vertico"
  :sparse t
  :no-suppress t)

(add-hook 'minibuffer-mode-hook #'modaled-insert-state-mode)
(add-hook 'o-escape-hook #'modaled-set-init-state)

(modaled-enable-substate-on-state-change "vertico"
  :pred #'minibufferp)

(defvar-keymap modaled-vertico-mode-substate-keymap
  "C-n" #'vertico-scroll-up
  "C-p" #'vertico-scroll-down
  "TAB" #'vertico-next
  "C-k" #'vertico-previous
  "C-j" #'vertico-next
  ";" #'vertico-quick-exit
  "C-;" #'vertico-quick-exit
  "<backtab>" #'vertico-previous
  "C-o" #'embark-act)

;; update after major mode changes
(defun o-hook--setup-modaled ()
  "Setup modal-editing."
  (setq modaled--initialized nil)
  (modaled-initialize))

(add-hook 'after-change-major-mode-hook #'o-hook--setup-modaled)

(add-hook 'buffer-list-update-hook #'modaled-initialize-all-buffers)
(add-hook 'after-init-hook #'modaled-initialize-all-buffers)
;;; provide
(provide 'init-pkg-modaled)
;;; init-pkg-modaled.el ends here
