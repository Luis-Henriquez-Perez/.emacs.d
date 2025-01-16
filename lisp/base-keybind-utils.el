;;; base-keybind-utils.el --- keybinding functions -*- lexical-binding: t; -*-
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
;; Provide a function for binding keys.
;;
;; 1. Record which keys have been bound and I can optionally display them.
;; 2. Let me undo binding keys.
;; 3. Provide a macro on top of it with flexible syntax.
;; 4. Defer keybindings appropriately.
;; 5. Log the binding of keys.
;; The macro variant should declare the functions that are being bound so I do
;; not have to do that.
;; 6. Gracefully handle errors with binding keys.
;; 7. autoo
;; 8. some way to specify conditional bindings
;;
;; I have written a binding function in the past.  And I do not know I think a
;; function would be better than a macro.  I do not know at the very least a
;; function backend.  Problem with a macro is that it is harder to test.  And
;; it is not callable.
;;
;;; Code:
;; What data structure should I use to record bindings?
(bind! i "A-x" #'execute-extended-command)

(defun oo--bind (fn &rest args)
  (lef! ()
    (apply fn args)))

(defun oo-bind (&rest args)
  (cond ((symbolp keymap)
         (info! "is not bound, deferring...")
         (oo-call-after-bound keymap #'apply #'oo-bind args))
        (states
         (oo-call-after-load 'evil #'oo--bind #'evil-define-key* arglist))
        (t
         (oo--bind #'define-key arglist))))

(defmacro bind! (&rest args)
  (if states
      `(oo-bind :states ',states ',keymap ,key ,def)
    '(oo-bind ',states ',keymap ,key ,def)))

(defun! oo-localleader-bind (keymap key def)
  "Convenience function for defining localleader bindings."
  (flet! leader (leader)
    (kbd (concat leader "\s" key)))
  (define-key keymap (leader oo-emacs-localleader-key) def)
  (with-eval-after-load 'evil
    (evil-define-key* 'emacs keymap (leader oo-emacs-localleader-key) def)
    (evil-define-key* 'normal keymap (leader oo-normal-localleader-key) def)
    (evil-define-key* 'normal keymap (leader oo-normal-localleader-short-key) def)
    (evil-define-key* 'insert keymap (leader oo-insert-localleader-key) def)
    (evil-define-key* 'insert keymap (leader oo-insert-localleader-short-key) def)))
;;;; alternate bindings
;; https://stackoverflow.com/questions/1609oo17/elisp-conditionally-change-keybinding
(defvar oo-alternate-commands (make-hash-table)
  "A hash-table mapping command symbols to a list of command symbols.")

(defun! oo-alternate-command-choose-fn (command)
  "Return an alternate command that should be called instead of COMMAND."
  (or (dolist (it (gethash command oo-alternate-commands))
        (aand! (funcall it) (break! it)))
      command))

;; (defun! oo-alt-bind (map orig alt &optional condition)
;;   "Remap keys bound to ORIG so ALT is called if CONDITION returns non-nil.
;; ORIG and ALT are command symbols.  CONDITION is a function that returns non-nil
;; when ALT should be invoked instead of ORIG."
;;   (flet! oo-when-fn (condition fn)
;;     `(lambda (&rest _) (when (funcall #',condition) #',alt)))
;;   (push (oo-when-fn (or condition #'always) alt) (gethash orig oo-alternate-commands))
;;   (define-key map `[remap ,orig] `(menu-item "" ,orig :filter oo-alternate-command-choose-fn)))

;; (defun oo-alt-bind (orig def)
;;   (let ((,orig ,key)
;;         (,alt ,def))
;;     (setq ,key (vconcat (list 'remap ,key)))
;;     (setq ,def (list 'menu-item "" ,alt :filter #'oo-alternate-command-choose-fn))
;;     (push ,(oo--lambda-form alt '(&rest ) `(when ,condition ,alt)) (gethash ,orig oo-alternate-commands))
;;     ,@(oo--bind-generate-body metadata steps)))
;;;; alt!
(defmacro alt! (old new feature)
  `(progn (push (lambda (&rest _) (when (or (featurep ',feature) (require ',feature nil t)) ',new))
                (gethash ',old oo-alternate-commands))
          (define-key global-map [remap ,old] '(menu-item "" ,old :filter oo-alternate-command-choose-fn))))
;;; provide
(provide 'base-keybind-utils)
;;; base-keybind-utils.el ends here
