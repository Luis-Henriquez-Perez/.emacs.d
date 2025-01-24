;;; base.el --- everything needed for startup -*- lexical-binding: t; -*-
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
;; This contains the core settings and functionality of my configuration.
;;
;;; Code:
;;;; requirements
(require 'base-vars)
(require 'base-settings)
(require 'base-lib)
(eval-when-compile (require 'base-macros))
(require 'base-packages)
;;;; logger
(defvar oo-log-buffer "*log*"
  "Name of the log buffer.")

;; This is important because it prevents the buffer from growing indefinitely and causing performance problems.
(defvar oo-log-buffer-max 500
  "Maximum number of lines in log buffer.")

;; After reaching `oo-log-buffer-max' lines, delete oldest line.
(defun! oo-log (type message &rest meta)
  "Log MESSAGE."
  (set! log (apply #'format message meta))
  (set! buffer (get-buffer-create oo-log-buffer))
  (set! output (format "[%s] %s" (upcase (symbol-name type)) log))
  (with-current-buffer buffer
    (unless view-mode (view-mode t))
    ;; Add to buffer without constantly moving focus to the end.
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-max))
        (or (save-excursion
              (and (zerop (forward-line -1))
                   (looking-at (rx bol (literal output)))
                   (goto-char (match-end 0))
                   (or (and (looking-at (rx space "(" space (group (1+ digit)) space ")" eol))
                            (alet! (match-string 1)
                              (replace-match (number-to-string (+ 1 (string-to-number it))) nil nil nil 1)
                              t))
                       (progn (insert " ( 2 )") t))))
            (progn (insert output)
                   (insert "\n")))
        (aand! (- (line-number-at-pos (point-max)) (1+ oo-log-buffer-max))
               (goto-char (point-min))
               (dotimes (_ it) (delete-line)))))))

(defmacro info! (msg &rest meta)
  `(oo-log 'info ,msg ,@meta))

(defmacro error! (msg &rest meta)
  `(oo-log 'info ,msg ,@meta))

(defmacro warn! (msg &rest meta)
  `(oo-log 'warn ,msg ,@meta))

(defmacro fatal! (msg &rest meta)
  `(oo-log 'fatal ,msg ,@meta))

(defmacro trace! (msg &rest meta)
  `(oo-log 'trace ,msg ,@meta))

(defmacro debug! (msg &rest meta)
  `(oo-log 'debug ,msg ,@meta))
;;;; hooks
(defun! oo--hook-docstring (hook function)
  "Generate a docstring for hook function."
  ;; This is taken directly from the `s' library.  Right now, it is the only
  ;; function from there I use.  Not wanting to require s for just one short
  ;; function, I copied it is body here.
  (flet! word-wrap (len s)
    (save-match-data
      (with-temp-buffer
        (insert s)
        (let ((fill-column len))
          (fill-region (point-min) (point-max)))
        (buffer-substring (point-min) (point-max)))))
  (flet! docstring (&rest lines)
    (cond ((null lines)
           "")
          ((cdr lines)
           (concat (car lines) "\n" (word-wrap 80 (string-join (cdr lines) "\s\s"))))
          ((word-wrap 80 (car lines)))))
  (docstring (format "Call `%s' from `%s'." function hook)
             (format "Log call to `%s'." function)
             (format "If `oo-debug-p' is non-nil suppress and log any error raised by `%s'." function)))

(defun! oo-add-hook (hook function &rest args)
  "Generate a function that calls FUNCTION and add it to HOOK.
Generated function call FUNCTION and logs any errors.  If IGNORE-ARGS, then do
generated function does not pass in any of its given arguments to FUNCTION."
  (set! fname (intern (format "oo--%s--%s" hook function)))
  (set! depth (plist-get args :depth))
  (set! local (plist-get args :local))
  (set! ignore-args (plist-get args :ignore-args))
  (set! funcall-form (if ignore-args `(,function) `(apply #',function arglist)))
  (unless (fboundp fname)
    (fset fname `(lambda (&rest arglist)
                   (ignore arglist)
                   ,(oo--hook-docstring hook function)
                   (info! "HOOK: %s -> %s" ',hook ',function)
                   (condition-case err
                       ,funcall-form
                     (error
                      (if oo-debug-p
                          (signal (car err) (cdr err))
                        (error! "%s : %s : %s -> %s"
                                #',function
                                ',hook
                                (car err)
                                (cdr err))))))))

  (add-hook hook fname depth local))
;;;; oo-call-after-load
(defun oo--call-after-load (expr fn)
  "Call FN after EXPR is met."
  (pcase expr
    ((pred null)
     (funcall fn nil))
    (`(:or . ,exprs)
     (dolist (expr exprs)
       (oo-call-after-load expr fn)))
    (`(:and . ,exprs)
     (oo--call-after-load exprs fn))
    ((or `(,(and feature (pred symbolp))) (and feature (pred symbolp)))
     (if (featurep feature)
         (funcall fn feature)
       (eval-after-load feature (apply-partially #'oo--call-after-load feature fn))))
    (`(,expr . ,exprs)
     (oo--call-after-load expr `(lambda (_) (oo--call-after-load ',exprs #',fn))))
    (_
     (error "invalid expression `%S'" expr))))

;; This macro is designed with the following goals in mind.
;; 1 - use one generic macro for most binding needs
;; 2 - log the variables I set and when they are being set
;; You'll get a warning when trying to bind a symbol that hasn't been defined yet.
;; So it's best to bind a package symbol only after the package has been loaded.
;; 3 - stop worrying about variables that haven't been bound
;; 4 - stop worrying about whether a variable is a custom variable or not
;; Some variables are custom variables.  Meaning they have some function that.
(defun oo-call-after-load (expr fn)
  "Call FN with ARGS after EXPR resolves.
EXPR can be a feature (symbol), a list of CONDITIONS, a list whose CAR is
either `:or' or `:and' and whose CDR is a list of EXPRS.  If CONDITION is a
feature, call FN with ARGS if feature has already been provided; otherwise,
behave similarly to `eval-after-load'.  If EXPR is a list of
EXPRS, call FN with ARGS only after all CONDITIONS have been met.  If
EXPR is a list whose CAR is `:and' behave the same way as (CDR CONDITION).
If EXPR is a list whose CAR is `:or', call FN with ARGS after any of
EXPRS in (CDR CONDITION) is met."
  (alet! (eval `(let ((first-call-p t))
                  (lambda (&optional feature)
                    (when first-call-p
                      (setq first-call-p nil)
                      (info! "AFTER-LOAD: %s -> %s" feature #',fn)
                      (condition-case err
                          (funcall #',fn)
                        (error
                         (if oo-debug-p
                             (signal (car err) (cdr err))
                           (error! "`%s` : %s -> %s"
                                   ',fn
                                   (car err)
                                   (cdr err))))))))
               t)
    (oo--call-after-load expr it)))
;;; provide
(provide 'base)
;;; base.el ends here
