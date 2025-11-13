;;; init-modaled.el --- Initialize modaled -*- lexical-binding: t; -*-
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
(require 'mark-thing-at)

(mark-thing-at-make-functions)

(defvar-keymap modaled-normal-state-keymap
  "+" #'text-scale-increase
  "-" #'text-scale-decrease

  "1" #'meep-digit-argument-repeat
  "2" #'meep-digit-argument-repeat
  "3" #'meep-digit-argument-repeat
  "4" #'meep-digit-argument-repeat
  "5" #'meep-digit-argument-repeat
  "6" #'meep-digit-argument-repeat
  "7" #'meep-digit-argument-repeat
  "8" #'meep-digit-argument-repeat
  "9" #'meep-digit-argument-repeat
  "0" #'meep-digit-argument-repeat

  "q" #'meep-register-kmacro-start-or-end
  "Q" #'repeat-fu-execute

  "w" #'meep-move-word-next
  "W" #'meep-move-symbol-next

  "e" #'meep-move-word-next-end
  "E" #'meep-move-symbol-next-end

  "r" #'meep-clipboard-killring-yank
  "R" #'meep-clipboard-only-yank

  "t" #'meep-move-matching-syntax-inner
  "T" #'meep-move-matching-syntax-outer

  "a a" #'meep-insert
  "a h" #'meep-insert-line-beginning
  "a s" #'meep-insert-append
  "a l" #'meep-insert-line-end
  "a k" #'meep-insert-open-above
  "a j" #'meep-insert-open-below

  "s w" #'mark-word
  "s a" #'mark-word
  "s l" #'mark-line
  "s n" #'mark-line-this
  "s h" #'mark-line-this
  "s o" #'mark-symbol
  "s d" #'mark-defun
  "s j" #'mark-symbol
  "s s" #'mark-sentence
  "s p" #'mark-paragraph
  "s k" #'mark-list

  "d" #'meep-clipboard-only-cut
  "D" #'meep-clipboard-killring-cut

  "f" #'meep-insert-change
  "F" #'meep-insert-change-lines

  "g" #'meep-exchange-point-and-mark
  "G" #'meep-exchange-point-and-mark-motion

  ;; Right Hand: Row 2.
  "h" #'meep-move-char-prev
  "H" #'meep-move-line-beginning

  "j" #'meep-move-line-next
  "J" #'undo-only

  "k" #'meep-move-line-prev
  "K" #'undo-redo

  "l" #'meep-move-char-next
  "L" #'meep-move-line-end

  ";" #'execute-extended-command
  ":" #'meep-move-matching-bracket-outer

  "y" #'meep-clipboard-killring-copy
  "Y" #'meep-clipboard-only-copy

  "u" #'meep-clipboard-killring-yank
  "U" #'meep-clipboard-only-yank

  "i" #'meep-clipboard-killring-cut
  "I" #'meep-clipboard-only-cut

  "z" #'meep-region-toggle
  "Z" #'meep-clipboard-killring-copy

  "x" #'meep-move-matching-syntax-inner
  "X" #'meep-clipboard-killring-copy

  "c" #'meep-move-matching-syntax-inner
  "C" #'meep-clipboard-killring-copy

  "v" #'meep-region-toggle
  "V" #'meep-move-matching-syntax-outer

  "b" #'meep-insert-change
  "B" #'meep-insert-change-lines

  "o" #'meep-insert-open-below
  "O" #'meep-insert-open-above

  "n" #'meep-isearch-at-point-next
  "N" #'meep-isearch-at-point-prev

  "m" #'meep-isearch-at-point-next
  "M" #'meep-isearch-at-point-prev

  "<escape>" #'oo-dwim-escape
  oo-normal-leader-key #'oo-leader-map)

;; (defun meep-kbd (def)
;;   "Command that converts current key."
;;   `(lambda ()
;;      ,(format "Execute the command which is bound to %s."
;;               (buttonize def 'describe-key (kbd def)))
;;      (interactive)
;;      (cond
;;       ((string-suffix-p "-" ,def)
;;        (let ((ch (char-to-string(read-event ,def))))
;;          (setq unread-command-events (listify-key-sequence (kbd (concat ,def ch))))))
;;       (t
;;        (setq unread-command-events (listify-key-sequence (kbd ,def)))))))

;; (keymap-set global-map "C-c x" (meep-kbd "C-x"))
                                        ;this space+x = C-c x
;; (keymap-set global-map "C-c m" (meep-kbd "M-"))
                                        ;this space+m = M-

;; (defvar-keymap meep-state-keymap-motion
;;   "<SPC>"  (meep-kbd "C-c"))

(defun oo-set-state-with-modaled-a (state)
  (modaled-set-state (symbol-name state)))

(setq meep-state-insert 'insert)
(advice-add 'bray-state-stack-push :override #'oo-set-state-with-modaled-a)
(advice-add 'bray-state-set :override #'oo-set-state-with-modaled-a)
(advice-add 'bray-state-set :override #'oo-set-state-with-modaled-a)

(modaled-define-state "normal"
  :lighter "[NOR]"
  :cursor-type 'box)

(defun oo-dwim-escape ()
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

;; (keymap-set modaled-normal-state-keymap oo-normal-leader-key #'oo-leader-map)

;; (defvar-keymap modaled-normal-state-keymap
;;   "<escape>" #'oo-dwim-escape)

;; (add-hook 'modaled-normal-state-mode-hook (apply-partially #'set-cursor-color "orange"))
;; (add-hook 'modaled-insert-state-mode-hook (apply-partially #'set-cursor-color "green"))
(modaled-define-state "insert"
  :sparse t
  ;; insert state must be no-suppress to support inserting char
  :no-suppress t
  :cursor-type 'bar
  :lighter "[INS]")

(keymap-set modaled-insert-state-keymap "<escape>" #'oo-dwim-escape)

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

;; (defun oo-state-changed-p ()
;;   ""
;;   (message "State changed...")
;;   t)
(modaled-enable-substate-on-state-change
  "org"
  :states '("normal")
  :major '(org-mode)
  ;; :pred #'oo-state-changed-p
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
(provide 'init-modaled)
;;; init-modaled.el ends here
