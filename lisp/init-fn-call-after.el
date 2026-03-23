;;; init-fn-call-after.el --- Define functions for deferred loading -*- lexical-binding: t; -*-
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
;; Define functions for deferred loading
;;
;;; Code:
(require 'subr-x)
(eval-when-compile (require 'init-mac-base))
(require 'init-core-log)

(defvar o-defer-bound-after-fns nil
  "An alist whose elements are (SYMBOL . FNS).
SYMBOL is a variable symbol.  FNS are a list of functions that should be
called in reverse order when symbol is bound.")

(defvar o-defer-load-after-fns (make-hash-table :size 100)
  "A hash table whose elements are (FEATURE . FNS).
FEATURE is a feature symbol.  FNS are a list of functions to be called
after FEATURE is loaded.")

(defun o-defer-bound-call-fns (&rest _)
  "Call functions of any bound symbols in `o-defer-bound-after-fns'."
  (let (updated symbol fns)
    (dolist (elt (reverse o-defer-bound-after-fns))
      (setq symbol (car elt))
      (setq fns (cdr elt))
      (if (boundp symbol)
          (dolist (fn (nreverse fns))
            (funcall fn))
        (push elt updated)))
    (setq o-defer-bound-after-fns updated)))

(defun o-defer-bound-after (symbol fn)
  "Call FN after SYMBOL is bound.
If SYMBOL is already bound FN is called immediately."
  (if (boundp symbol)
      (funcall fn)
    (push fn (alist-get symbol o-defer-bound-after-fns))))

(defun o-defer-load-call-fns (feature)
  "Call each load function for FEATURE."
  (when-let (fns (gethash feature o-defer-load-after-fns))
    (dolist (fn (nreverse fns))
      (funcall fn))
    (remhash feature o-defer-load-after-fns)))

(defun o--defer-load-after (feature fn)
  "Call FN after FEATURE is loaded."
  (if (featurep feature)
      (funcall fn)
    ;; Do not add this to the `after-load-alist' more than once.
    (unless (gethash feature o-defer-load-after-fns)
      (eval-after-load feature (apply-partially #'o-defer-load-call-fns feature)))
    (push fn (gethash feature o-defer-load-after-fns))))

(defun o--defer-call-fn (fn)
  "Call FN and log time elapsed during call.
Suppress any error raised by FN, instead logging its occurrence."
  (condition-case e
      (let ((seconds (o-time-elapsed (funcall fn))))
        (o-log 'success "Called %s in %0.2f seconds" fn seconds))
    (error
     (o-log 'failure "Failed to call %S %s %s" fn (car e) (cdr e)))))

(defun o-defer-load-after (feature fn)
  "Same as `o-defer-load-after' but"
  (o-defer-load-after feature (apply-partially #'o--defer-call-fn fn)))

(defun o--defer-load-feature (feature)
  "Load FEATURE and log the time elapsed in loading.
Suppress any error raised while loading, instead logging its occurrence."
  (condition-case err
      (let ((seconds (o-time-elapsed (require feature))))
        (o-log 'success "Loaded %s in %0.2f seconds" feature seconds))
    (error
     (o-log 'failure "Failed to load %s : %S -> %S" feature (car err) (cdr err)))))

(defun o-defer-load-require (feature1 feature2)
  "Load FEATURE2 after FEATURE1 has been loaded."
  (o--defer-load-after feature1
                       (apply-partially #'o--defer-load-feature feature2)))
;;; provide
(provide 'init-fn-call-after)
;;; init-fn-call-after.el ends here
