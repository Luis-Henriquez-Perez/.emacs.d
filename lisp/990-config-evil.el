;;; 990-config-evil.el --- evil configuration -*- lexical-binding: t; -*-
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
;; This is my configuration for evil.
;;
;;; Code:
(require! "^0[01]")
(require 'evil)
;;;; SETTINGS
;; To ensure that =oo-override-mode-map= takes priority over evil states, we need
;; to make it an intercept map for all evil states.  In evil, intercept maps are
;; maps that take priority (intercept) evil bindings when they have a different
;; binding for the same key (this is opposed to =overriding-maps=, which completely
;; override an evil keymap).
;; By default =evil= displays the current state in the echo area.  I think some
;; indicator for the current state is necessary but I don't want to do it via
;; echoing.  Instead I plan to do it primarily via cursor colors; and possibly the
;; modeline as well.
(setq evil-echo-state nil)
(setq evil-move-cursor-back nil)
(setq evil-move-beyond-eol nil)
(setq evil-search-wrap nil)

;; Disable starting any mode in motion state.
(setq evil-normal-state-modes (append evil-emacs-state-modes
									  ;; evil-motion-state-modes
									  evil-normal-state-modes))
(setq evil-emacs-state-modes nil)
;; (setq evil-motion-state-modes nil)
;;;; CURSOR COLOR
;;;;; STATE FACES
(defface evil|state-face
  '((t (:weight bold)))
  "Meta-face used for property inheritance on all evil state faces.")

(defface evil|emacs-state-face
  '((t (:inherit evil|state-face :background "#483d8b")))
  "Face for the Emacs state tag in evil indicator.")

(setf (alist-get 'evil|emacs-state-face oo-custom-faces-alist) 'font-lock-builtin-face)

(defface evil|insert-state-face
  '((t (:inherit evil|state-face :background "#228b22")))
  "Face for the insert state tag in evil indicator.")

(setf (alist-get 'evil|insert-state-face oo-custom-faces-alist) 'font-lock-type-face)

(defface evil|motion-state-face
  '((t (:inherit evil|state-face :background "#a0522d")))
  "Face for the motion state tag in evil indicator.")

(setf (alist-get 'evil|motion-state-face oo-custom-faces-alist) 'font-lock-variable-name-face)

(defface evil|normal-state-face
  '((t (:inherit evil|state-face :background "purple")))
  "Face for the normal state tag in evil indicator.")

(setf (alist-get 'evil|normal-state-face oo-custom-faces-alist) 'font-lock-keyword-face)

(defface evil|operator-state-face
  '((t (:inherit evil|state-face :background "#0000ff")))
  "Face for the operator state tag in evil indicator.")

(setf (alist-get 'evil|operator-state-face oo-custom-faces-alist) 'font-lock-function-name-face)

(defface evil|visual-state-face
  '((t (:inherit evil|state-face :background "#8b2252")))
  "Face for the visual state tag in evil indicator.")

(setf (alist-get 'evil|visual-state-face oo-custom-faces-alist) 'font-lock-string-face)

(defface evil|replace-state-face
  '((t (:inherit evil|state-face :background "#008b8b")))
  "Face for the replace state tag in evil indicator.")

(setf (alist-get 'evil|replace-state-face oo-custom-faces-alist) 'font-lock-constant-face)
;;;;; CHANGE CURSOR COLOR AND SHAPE ACCORDING TO CURRENT EVIL STATE
;; Did not realize for the longest time that evil cursor can be a function that
;; changes the cursor.  With this in mind, the best way to set the cursor size
;; and shape dynamically is to set the corresponding cursor symbols to functions.
(defun evil|state-face ()
  "Return the cursor color for state as a string."
  (intern (format "evil|%s-state-face" evil-state)))

(defun evil|state-background ()
  "Return the background of the current evil state face."
  (aand! (evil|state-face) (face-attribute it :background)))

(defun evil|set-default-cursor ()
  "Set cursor for normal state."
  (evil-set-cursor (list t (evil|state-background))))

(defun evil|set-insert-state-cursor ()
  "Set cursor for insert state."
  (evil-set-cursor (list '(bar . 2) (evil|state-background))))

(defun evil|set-operator-state-cursor ()
  "Set cursor for operator state."
  (evil-set-cursor (list '(hbar . 9) (evil|state-background))))

(defalias 'evil|set-normal-state-cursor 'evil|set-default-cursor)
(defalias 'evil|set-motion-state-cursor 'evil|set-default-cursor)
(defalias 'evil|set-replace-state-cursor 'evil|set-default-cursor)
(defalias 'evil|set-emacs-state-cursor 'evil|set-default-cursor)
(defalias 'evil|set-visual-state-cursor 'evil|set-default-cursor)
;;;;; CURSOR COLORS
(setq evil-default-cursor        #'evil|set-default-cursor)
(setq evil-normal-state-cursor   #'evil|set-normal-state-cursor)
(setq evil-insert-state-cursor   #'evil|set-insert-state-cursor)
(setq evil-visual-state-cursor   #'evil|set-visual-state-cursor)
(setq evil-motion-state-cursor   #'evil|set-motion-state-cursor)
(setq evil-replace-state-cursor  #'evil|set-replace-state-cursor)
(setq evil-operator-state-cursor #'evil|set-operator-state-cursor)
(setq evil-emacs-state-cursor    #'evil|set-emacs-state-cursor)
;;;; MINIBUFFER
(defvar evil|state-before-minibuffer nil
  "Store the evil state before entering the minibuffer.")

;; It is easier to make all the hooks and functions "safe" than to remember all
;; the hooks and remove them when evil-mode is disabled.
(defun evil|save-prior-evil-state-h ()
  "Save state before entering the minibuffer and enter insert state."
  (when (bound-and-true-p evil-mode)
    (setq evil|state-before-minibuffer evil-state)
    (evil-insert-state)))

(defun evil|restore-prior-evil-state-h ()
  "Restore state after minibuffer."
  (when (bound-and-true-p evil-mode)
    (when evil|state-before-minibuffer
      (evil-change-state evil|state-before-minibuffer))
    (setq evil|state-before-minibuffer nil)))

(add-hook 'minibuffer-setup-hook #'evil|save-prior-evil-state-h)
(add-hook 'minibuffer-exit-hook #'evil|restore-prior-evil-state-h)
;;;; THEME
(defun evil|refresh-cursor-ignore-args (&rest _)
  (when (bound-and-true-p evil-mode)
    (evil-refresh-cursor)))

(add-hook 'enable-theme-functions #'evil|refresh-cursor-ignore-args)
;;;; BETTER ESCAPE
(defun evil|dwim-escape ()
  "Exit out of whatever is happening after escape.
Enter normal state.  If in minibuffer, exit the minibuffer.  When in a
non-readonly file buffer, save the buffer."
  (interactive)
  (when (bound-and-true-p evil-mode)
    (evil-normal-state 1))
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
		 (keyboard-quit))))
;;;; OPERATORS
;;;;; EVALUATING
;; This is shamelessly copied from `evil-extra-operator'.
(evil-define-operator evil|eval-operator (beg end)
  "Evil operator for evaluating code."
  :move-point nil
  (interactive "<r>")
  (eval-region beg end t))

;; This is also shamelessly copied with the difference that the format string is
;; "%S" instead of "%s".  Honestly, I think not having it that way was a bug.
(evil-define-operator evil|eval-replace-operator (beg end)
  "Evil operator for replacing contents with result from eval."
  :move-point nil
  (interactive "<r>")
  (let* ((text (buffer-substring-no-properties beg end))
         (result (format "%S" (eval (read text)))))
    (delete-region beg end)
    (insert result)))

(evil-define-operator evil|eval-print-operator (beg end)
  "Evil operator for printing the results of contents below."
  :move-point nil
  (interactive "<r>")
  (let* ((text (buffer-substring-no-properties beg end))
         (result (format "\n=> %S" (eval (read text)))))
    (goto-char end)
    (alet! (point)
      (insert result)
      (comment-region it (point)))))
;;;;; HUNGRY DELETE (EXPERIMENTAL AND IN PROGRESS)
;; This needs some more fine-tuning.
(defun evil|consume-ws-a (orig-fn &rest args)
  (prog1 (apply orig-fn args)
    ;; TODO: this should happen as well for lines behind.
    (cond ((looking-at (rx (>= 2 "\n")))
           (delete-blank-lines))
          ((looking-at (rx (>= 2 "\s")))
           (just-one-space)))))

(advice-add 'evil-delete :around #'evil|consume-ws-a)
(advice-add 'lispyville-delete :around #'evil|consume-ws-a)
(advice-add 'lispyville-delete-char-or-splice :around #'evil|consume-ws-a)
;;;; TEXT-OBJECTS
(evil-define-text-object evil|outer-buffer (_ &optional _ _ type)
  "Select the entire buffer as a text object."
  (list (point-min) (point-max) type))

(evil-define-text-object evil|inner-buffer (_ &optional _ _ type)
  "Select the inner buffer (same as outer in this case)."
  (list (point-min) (point-max) type))
;;;; INSERT STATE HOOK
(defun evil|enter-insert-state-ignore-args (&rest _)
  "Enter insert state if `evil-mode' is enabled."
  (when (bound-and-true-p evil-mode)
    (evil-insert-state 1)))

(defun oo-evil-normalize-keymaps-ignore-args (&rest _)
  (when (bound-and-true-p evil-mode)
    (evil-normalize-keymaps)))
;;;; CROSS-CONFIGURATION
;;;;; ORG-CAPTURE
(add-hook 'org-capture-mode-hook #'evil-insert-state)
;;;;; GIT-COMMIT
;; Note that I cannot use `evil-set-initial-state' for this because
;; `git-commit-mode' is a minor-mode.
(add-hook 'git-commit-mode-hook #'evil-insert-state)
;;;;; DENOTE
(add-hook 'denote-after-new-note-hook #'evil-insert-state)
;;;;; CORFU
;; When using evil, neither `corfu-map' nor `tempel-map' bindings will work
;; because the maps are overridden by evil.  In order for them to work, we need
;; to boost give the maps greater precedence.
(defafter! oo-make-corfu-kbds-work-with-evil (corfu)
  (evil-make-overriding-map corfu-map)
  (advice-add 'corfu--setup :after #'oo-evil-normalize-keymaps-ignore-args)
  (advice-add 'corfu--teardown :after #'oo-evil-normalize-keymaps-ignore-args))
;;;;; TEMPEL
(defafter! oo-make-tempel-kbds-work-with-evil (tempel)
  (evil-make-overriding-map tempel-map))

(advice-add 'tempel-insert :after #'evil-insert-state)
;;;;; MAGIT
;; Note that I cannot use `evil-set-initial-state' for this because
;; `git-commit-mode' is a minor-mode.
(add-hook 'git-commit-mode-hook #'evil-insert-state)

(add-hook 'vc-git-log-edit-mode-hook #'evil-insert-state 0)
;;;;; ORG
(add-hook 'org-log-buffer-setup-hook #'evil-insert-state)
;;;; PREVENT CURSOR COLOR FROM CHANGING WITH ELDOC
;; For some reason the cursor color changes with eldoc.  Here I tell.  This also
;; fixes the cursor color change when expanding a tempel snippet.
(advice-add 'elisp-eldoc-funcall :around #'+elisp-eldoc-funcall@preserve-cursor-color)
(defun! +elisp-eldoc-funcall@preserve-cursor-color (orig &rest args)
  (set! bg (face-attribute 'cursor :background))
  (prog1 (apply orig args)
    (unless (equal bg (face-attribute 'cursor :background))
      (set-cursor-color bg))))
;;; provide
(provide '990-config-evil)
;;; 990-config-evil.el ends here
