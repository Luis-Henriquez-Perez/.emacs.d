;;; 030-base-utils.el -*- lexical-binding: t; -*-
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
;; This file contains utility functions.
;;
;;; Code:
;;;; requirements
(require 'cl-lib)
(require 'pcase)
;;;; predicates
(defsubst oo-negative-p (number)
  "Return non-nil if NUMBER is less than zero."
  (declare (pure t) (side-effect-free error-free))
  (< number 0))

(defsubst oo-positive-p (number)
  "Return non-nil if NUMBER is greater than zero."
  (declare (pure t) (side-effect-free error-free))
  (> number 0))
;;;; destructuring
;; This function of course is not only for destructuring but now its what I am
;; using it for.
(defun oo-tree-map-nodes (pred fn tree)
  "Recursively map FN over tree nodes satisfying PRED.

PRED is a predicate function applied to each node in TREE.  TREE can be a nested
list, vector or improper list.  Return a new tree with FN applied to the nodes
matching PRED."
  (cond ((funcall pred tree)
         (funcall fn tree))
        ((consp tree)
         (cons (oo-tree-map-nodes pred fn (car tree))
               (oo-tree-map-nodes pred fn (cdr tree))))
        ((vectorp tree)
         `[,@(mapcar (apply-partially #'oo-tree-map-nodes pred fn)
                     (append tree nil))])
        (t
         tree)))

(defun oo-into-pcase-pattern (match-form)
  "Convert MATCH-FORM into a `pcase` pattern.

MATCH-FORM is a potentially nested structure containing lists, vectors, or
symbols.  This function transforms symbols in MATCH-FORM into pcase-compatible
patterns using backquote and comma syntax.

Return a pcase-compatible pattern."
  (if (symbolp match-form)
      match-form
    (cl-flet ((true-symbolp (o) (and o (symbolp o)))
              (add-comma (o) (list '\, o)))
      (list '\` (oo-tree-map-nodes #'true-symbolp #'add-comma match-form)))))

(defun oo-destructure-special-match-form (match-form value)
  "Generate `let*` bindings for handling special match forms.

MATCH-FORM is a destructuring pattern to be matched.  A special match-form
constitutes one of the following structures.

(&as WHOLE PARTS) Bind the value of current expression to WHOLE.

(&key KEY . KEYS) Bind each symbol in KEYS to (plist-get MATCH-FORM KEY)

(&map KEY . KEYS) Bind each symbol in key to (map-elt MATCH-FORM . KEY).

VALUE is the value being destructured.

If MATCH-FORM is not a special form, return nil."
  (pcase match-form
    (`(,(or '&as '&whole) ,(and whole (pred symbolp)) ,parts)
     (let ((it (cl-gensym "special-&as-match-form")))
       `((,it ,value)
         (,whole ,it)
         (,parts ,it))))
    (`(&key ,(and symbol (pred symbolp)) . ,(and symbols (guard t)))
     (let ((it (cl-gensym "special-&key-match-form"))
           (bindings nil))
       (dolist (s (cons symbol symbols))
         (push `(,s (plist-get ,it ,(intern (concat ":" (symbol-name s))))) bindings))
       (push `(,it ,value) bindings)
       (nreverse bindings)))
    (`(&map ,(and symbol (pred symbolp)) . ,(and symbols (guard t)))
     (let ((it (cl-gensym "special-&map-match-form"))
           (bindings nil))
       (dolist (s (cons symbol symbols))
         (push `(,s (map-elt ,it ,(intern (concat ":" (symbol-name s))))) bindings))
       (push `(,it ,value) bindings)
       (nreverse bindings)))
    (_
     nil)))

(defun oo-generate-special-match-form-bindings (match-form value)
  "Generate bindings for special forms in MATCH-FORM relative to VALUE.

Process MATCH-FORM to identify and replace any special forms, returning a list
where the first element is a transformed match-form with special forms replaced
and subsequent elements are additional bindings required to handle the special
forms.

MATCH-FORM is a destructuring pattern that may include special forms (see
`oo-destructure-special-match-form').  VALUE is the value to be matched and
destructured."
  (let (bindings match-form-value)
    (setq match-form-value (gensym "match-form-value"))
    (cl-flet ((special-mf-p (mf)
                (let ((it (oo-destructure-special-match-form mf match-form-value)))
                  (when it
                    (setq bindings (append bindings it))
                    (setq match-form-value (gensym "match-form-value")))
                  it))
              (replace-with-value (lambda (_) match-form-value)))
      `((,(oo-tree-map-nodes #'special-mf-p #'replace-with-value match-form) ,value)
        ,@bindings))))

(defun oo-pcase-bindings (match-form value)
  "Generate pcase-compatible bindings from MATCH-FORM and VALUE.

MATCH-FORM is the destructuring pattern that specifies how VALUE should be
decomposed.  VALUE is the data to be matched and destructured.

Return a list of bindings compatible with `pcase`."
  (mapcar (pcase-lambda (`(,mf ,val)) (list (oo-into-pcase-pattern mf) val))
          (oo-generate-special-match-form-bindings match-form value)))

(defun oo-flatten-pcase-match-form (match-form)
  "Flatten MATCH-FORM into a list of components.

MATCH-FORM can contain nested lists or vectors. This function extracts all
symbols and other components, ensuring no duplicates.

Return a flat list of unique components in MATCH-FORM."
  (cl-flet ((flatten-pattern (match-form)
              (let ((stack (list (if (vectorp match-form) (append match-form nil) match-form)))
                    (symbols nil)
                    (node nil))
                (while stack
                  (cond ((null (car stack))
                         (pop stack))
                        ((listp (car stack))
                         (setq node (pop (car stack)))
                         (cond ((symbolp node)
                                (cl-pushnew node symbols))
                               ((nlistp (cdr-safe node))
                                (push (list (car node) (cdr node)) stack))
                               ((listp node)
                                (push node stack))
                               ((vectorp node)
                                (push (append node nil) stack))))
                        (t
                         (cl-pushnew (pop stack) symbols))))
                symbols)))
    (cl-set-difference (flatten-pattern match-form) '(\, \`))))
;;;; hook
(defun oo-add-hook (hook function &rest args)
  "Generate a function that calls FUNCTION and add it to HOOK.
Generated function call FUNCTION and logs any errors.  If IGNORE-ARGS, then do
generated function does not pass in any of its given arguments to FUNCTION."
  (let* ((fname (intern (format "oo--%s--%s" hook function)))
         (depth (plist-get args :depth))
         (local (plist-get args :local))
         (ignore-args (plist-get args :ignore-args))
         (funcall-form (if ignore-args `(,function) `(apply #',function arglist))))
    (unless (fboundp fname)
      (fset fname `(lambda (&rest arglist)
                     (ignore arglist)
                     (oo-log 'info "HOOK: %s -> %s" ',hook ',function)
                     (condition-case err
                         ,funcall-form
                       (error
                        (if oo-debug-p
                            (signal (car err) (cdr err))
                          (oo-log 'error "%s : %s : %s -> %s"
                                  #',function
                                  ',hook
                                  (car err)
                                  (cdr err))))))))
    (add-hook hook fname depth local)))
;;;; uncategorized
;; This function is used by captain and abbrev.
(defun oo-in-string-or-comment-p ()
  "Return non-nil if point is in a string or comment.
Specifically, return the symbol `string' if point is in a string, the symbol
`comment' if in a comment and nil otherwise."
  (declare (pure t) (side-effect-free t))
  (let ((ppss (syntax-ppss)))
    (cond ((nth 3 ppss) 'string)
          ((nth 4 ppss) 'comment)
          (t nil))))
;;; provide
(provide '030-base-utils)
;;; 030-base-utils.el ends here
