;;; config-evil.el --- evil configuration -*- lexical-binding: t; -*-
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
(require 'base)
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
(defface oo-evil-state-face
  '((t (:weight bold)))
  "Meta-face used for property inheritance on all evil state faces.")

(defface oo-evil-emacs-state-face
  '((t (:inherit oo-evil-state-face :background "#483d8b")))
  "Face for the Emacs state tag in evil indicator.")

(setf (alist-get 'oo-evil-emacs-state-face oo-custom-faces-alist) 'font-lock-builtin-face)

(defface oo-evil-insert-state-face
  '((t (:inherit oo-evil-state-face :background "#228b22")))
  "Face for the insert state tag in evil indicator.")

(setf (alist-get 'oo-evil-insert-state-face oo-custom-faces-alist) 'font-lock-type-face)

(defface oo-evil-motion-state-face
  '((t (:inherit oo-evil-state-face :background "#a0522d")))
  "Face for the motion state tag in evil indicator.")

(setf (alist-get 'oo-evil-motion-state-face oo-custom-faces-alist) 'font-lock-variable-name-face)

(defface oo-evil-normal-state-face
  '((t (:inherit oo-evil-state-face :background "purple")))
  "Face for the normal state tag in evil indicator.")

(setf (alist-get 'oo-evil-normal-state-face oo-custom-faces-alist) 'font-lock-keyword-face)

(defface oo-evil-operator-state-face
  '((t (:inherit oo-evil-state-face :background "#0000ff")))
  "Face for the operator state tag in evil indicator.")

(setf (alist-get 'oo-evil-operator-state-face oo-custom-faces-alist) 'font-lock-function-name-face)

(defface oo-evil-visual-state-face
  '((t (:inherit oo-evil-state-face :background "#8b2252")))
  "Face for the visual state tag in evil indicator.")

(setf (alist-get 'oo-evil-visual-state-face oo-custom-faces-alist) 'font-lock-string-face)

(defface oo-evil-replace-state-face
  '((t (:inherit oo-evil-state-face :background "#008b8b")))
  "Face for the replace state tag in evil indicator.")

(setf (alist-get 'oo-evil-replace-state-face oo-custom-faces-alist) 'font-lock-constant-face)
;;;;; CHANGE CURSOR COLOR AND SHAPE ACCORDING TO CURRENT EVIL STATE
;; Did not realize for the longest time that evil cursor can be a function that
;; changes the cursor.  With this in mind, the best way to set the cursor size
;; and shape dynamically is to set the corresponding cursor symbols to functions.
(defun oo-evil-state-face ()
  "Return the cursor color for state as a string."
  (intern (format "oo-evil-%s-state-face" evil-state)))

(defun oo-evil-state-background ()
  "Return the background of the current evil state face."
  (aand! (oo-evil-state-face) (face-attribute it :background)))

(defun oo-evil-set-default-cursor ()
  "Set cursor for normal state."
  (evil-set-cursor (list t (oo-evil-state-background))))

(defun oo-evil-set-insert-state-cursor ()
  "Set cursor for insert state."
  (evil-set-cursor (list '(bar . 2) (oo-evil-state-background))))

(defun oo-evil-set-operator-state-cursor ()
  "Set cursor for operator state."
  (evil-set-cursor (list '(hbar . 9) (oo-evil-state-background))))

(defalias 'oo-evil-set-normal-state-cursor 'oo-evil-set-default-cursor)
(defalias 'oo-evil-set-motion-state-cursor 'oo-evil-set-default-cursor)
(defalias 'oo-evil-set-replace-state-cursor 'oo-evil-set-default-cursor)
(defalias 'oo-evil-set-emacs-state-cursor 'oo-evil-set-default-cursor)
(defalias 'oo-evil-set-visual-state-cursor 'oo-evil-set-default-cursor)
;;;;; CURSOR COLORS
(setq evil-default-cursor        #'oo-evil-set-default-cursor)
(setq evil-normal-state-cursor   #'oo-evil-set-normal-state-cursor)
(setq evil-insert-state-cursor   #'oo-evil-set-insert-state-cursor)
(setq evil-visual-state-cursor   #'oo-evil-set-visual-state-cursor)
(setq evil-motion-state-cursor   #'oo-evil-set-motion-state-cursor)
(setq evil-replace-state-cursor  #'oo-evil-set-replace-state-cursor)
(setq evil-operator-state-cursor #'oo-evil-set-operator-state-cursor)
(setq evil-emacs-state-cursor    #'oo-evil-set-emacs-state-cursor)
;;;; MINIBUFFER
(defvar oo-evil-state-before-minibuffer nil
  "Store the evil state before entering the minibuffer.")

;; It is easier to make all the hooks and functions "safe" than to remember all
;; the hooks and remove them when evil-mode is disabled.
(defun oo-evil-save-prior-evil-state-h ()
  "Save state before entering the minibuffer and enter insert state."
  (when (bound-and-true-p evil-mode)
    (setq oo-evil-state-before-minibuffer evil-state)
    (evil-insert-state)))

(defun oo-evil-restore-prior-evil-state-h ()
  "Restore state after minibuffer."
  (when (bound-and-true-p evil-mode)
    (when oo-evil-state-before-minibuffer
      (evil-change-state oo-evil-state-before-minibuffer))
    (setq oo-evil-state-before-minibuffer nil)))

(add-hook 'minibuffer-setup-hook #'oo-evil-save-prior-evil-state-h)
(add-hook 'minibuffer-exit-hook #'oo-evil-restore-prior-evil-state-h)
;;;; THEME
(defun oo-evil-refresh-cursor-ignore-args (&rest _)
  (when (bound-and-true-p evil-mode)
    (evil-refresh-cursor)))

(add-hook 'enable-theme-functions #'oo-evil-refresh-cursor-ignore-args)
;;;; BETTER ESCAPE
(defun oo-evil-dwim-escape ()
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
(evil-define-operator oo-evil-eval-operator (beg end)
  "Evil operator for evaluating code."
  :move-point nil
  (interactive "<r>")
  (eval-region beg end t))

;; This is also shamelessly copied with the difference that the format string is
;; "%S" instead of "%s".  Honestly, I think not having it that way was a bug.
(evil-define-operator oo-evil-eval-replace-operator (beg end)
  "Evil operator for replacing contents with result from eval."
  :move-point nil
  (interactive "<r>")
  (let* ((text (buffer-substring-no-properties beg end))
         (result (format "%S" (eval (read text)))))
    (delete-region beg end)
    (insert result)))

(evil-define-operator oo-evil-eval-print-operator (beg end)
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
(defun oo-evil-consume-ws-a (orig-fn &rest args)
  (prog1 (apply orig-fn args)
    ;; TODO: this should happen as well for lines behind.
    (cond ((looking-at (rx (>= 3 "\n")))
           (delete-blank-lines))
          ((looking-at (rx (>= 2 "\s")))
           (just-one-space)))))

(advice-add 'evil-delete :around #'oo-evil-consume-ws-a)
(advice-add 'lispyville-delete :around #'oo-evil-consume-ws-a)
(advice-add 'lispyville-delete-char-or-splice :around #'oo-evil-consume-ws-a)
;;;; TEXT-OBJECTS
(evil-define-text-object oo-evil-outer-buffer (_ &optional _ _ type)
  "Select the entire buffer as a text object."
  (list (point-min) (point-max) type))

(evil-define-text-object oo-evil-inner-buffer (_ &optional _ _ type)
  "Select the inner buffer (same as outer in this case)."
  (list (point-min) (point-max) type))
;;;; INSERT STATE HOOK
(defun oo-evil-enter-insert-state-ignore-args (&rest _)
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
;;;; KEYBINDINGS
(nmap! "+" #'text-scale-increase)
(nmap! "-" #'text-scale-decrease)
(nmap! "H" #'evil-first-non-blank)
(nmap! "L" #'evil-last-non-blank)
(nmap! "J" #'evil-scroll-page-down)
(nmap! "K" #'evil-scroll-page-up)
(nmap! [escape] #'oo-evil-dwim-escape)
;; Finding a place for motion commands.
;; (nmap! "ff" #'evil-find-char)
;; (nmap! "fw" #'oo-evilem-motion-beginning-of-word)
;; (nmap! "fe" #'oo-evilem-motion-end-of-word)
;; (nmap! "fl" #'oo-evilem-motion-beginning-of-line)
;; (nmap! "fj" #'oo-evilem-motion-char)

;; Hello world!
;; (nmap! "sj" #'evil-open-below)
;; (nmap! "sk" #'evil-open-above)
;; Invert bindings for downcase and upcase because I am more often going from
;; down to up than from up to down.
(nmap! "gu" #'evil-upcase)
(nmap! "gU" #'evil-downcase)
;; Evil operators that are by default on the main keyboard.  Consider whether I
;; really need them there or can put them in "g" keybinding.
;; c - change
;; y - yank
(imap! "A-x" #'execute-extended-command)
(imap! "M-x" #'execute-extended-command)
(imap! "C-c h" #'grugru)
(imap! [escape] #'oo-evil-dwim-escape)
(imap! "TAB" #'completion-preview-insert)

;; Lump open line above and below into the same binding.

(nimap! "C-c j" #'abbrev/inverse-add)
(nimap! "C-c k" #'unexpand-abbrev)

(vmap! "V" #'expreg-contract)
(vmap! "v" #'expreg-expand)
;; Ensure that ";" is always available as `execute-extended-command'.  Modes
;; like dired bind it themselves and would otherwise override it.
(nvmap! override-global-map ";" #'execute-extended-command)
;; The problem is I feel like the default evil motions are not that useful
;; beyond moving to one forward unit.  So I have made the controversial decision
;; to rebind.

;; (nvmap! "w" #'oo-evilem-motion-beginning-of-word)
;; (nvmap! "e" #'oo-evilem-motion-end-of-word)
;; (nvmap! "W" #'oo-evilem-motion-beginning-of-WORD)
;; (nvmap! "E" #'oo-evilem-motion-end-of-WORD)
;; (nvmap! "f" #'oo-evilem-motion-char)
;; (nvmap! "H" #'oo-evilem-motion-beginning-of-line)

(nvmap! "g b" #'oo-evil-eval-print-operator)
(nvmap! "g p" #'oo-evil-eval-print-operator)
(nvmap! "g c" #'evilnc-comment-operator)
(each! '(cider-repl-mode-map clojure-mode-map clojurec-mode-map clojurescript-mode-map clojurex-mode-map clojure-ts-mode-map clojurescript-ts-mode-map clojurec-ts-mode-map common-lisp-mode-map emacs-lisp-mode-map eshell-mode-map fennel-mode-map fennel-repl-mode-map geiser-repl-mode-map gerbil-mode-map inf-clojure-mode-map inferior-emacs-lisp-mode-map inferior-lisp-mode-map inferior-scheme-mode-map lisp-interaction-mode-map lisp-mode-map monroe-mode-map racket-mode-map racket-repl-mode-map scheme-interaction-mode-map scheme-mode-map slime-repl-mode-map sly-mrepl-mode-map stumpwm-mode-map)
  (oo-bind-key it "g c" #'lispyville-comment-or-uncomment '(normal visual)))
(nvmap! emacs-lisp-mode-map [remap evilnc-comment-operator] #'lispyville-comment-or-uncomment)
(nvmap! "g e" #'oo-evil-eval-operator)
(nvmap! "g h" #'oo-evil-eval-operator)
(nvmap! "g l" #'oo-evil-eval-replace-operator)
(nvmap! "g r" #'oo-evil-eval-replace-operator)
(nvmap! "g s" #'evil-exchange)
(nvmap! "g S" #'evil-exchange-cancel)
(nvmap! "g x" #'evil-exchange)
(nvmap! "g X" #'evil-exchange-cancel)
;;;; TEXT-OBJECTS
(autoload 'evilnc-inner-comment "evil-nerd-commenter" nil nil 'function)
(autoload 'evilnc-outer-comment "evil-nerd-commenter" nil nil 'function)

(iotmap! "c" #'evilnc-inner-comment #'evilnc-outer-comment)
;; TODO: In "lispy" modes use lispyville-outer-comment instead.
(iotmap! "a" #'lispyville-inner-comment #'lispyville-outer-comment)
(iotmap! "h" #'evil-i-syntax #'evil-a-syntax)
(iotmap! "l" #'evil-inner-line #'evil-a-line)
(iotmap! "f" #'evil-cp-inner-form #'evil-cp-a-form)
;; (iotmap! "b" #'evil-textobj-anyblock-inner-block #'evil-textobj-anyblock-a-block)
(iotmap! "b" #'oo-evil-inner-buffer #'oo-evil-outer-buffer)
;;; provide
(provide 'config-evil)
;;; config-evil.el ends here
