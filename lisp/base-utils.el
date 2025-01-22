;;; base-utils.el -*- lexical-binding: t; -*-
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
(defsubst oo-true-symbol-p (object)
  "Return non-nil if OBJECT is a non-keyword symbol."
  (declare (pure t) (side-effect-free error-free))
  (and object (symbolp object) (not (keywordp object))))

(defsubst oo-cons-cell-p (object)
  "Return non-nil if OBJECT is a cons-cell but not a proper list."
  (declare (pure t) (side-effect-free error-free))
  (and (listp object) (not (listp (cdr-safe object)))))

(defsubst oo-true-list-p (object)
  "Return non-nil if OBJECT is a non-nil proper-list.
This means it is non-nil."
  (declare (pure t) (side-effect-free error-free))
  (and object (listp object) (listp (cdr-safe object))))

(defsubst oo-negative-p (number)
  "Return non-nil if NUMBER is less than zero."
  (declare (pure t) (side-effect-free error-free))
  (< number 0))

(defun oo-float-divide (&rest args)
  "Perform division with ARGS, ensuring the first argument is a float.
This behaves like `/`, but the result is always a floating-point number."
  (declare (pure t) (side-effect-free error-free))
  (apply #'/ (float (car args)) (cdr args)))

(defsubst oo-positive-p (number)
  "Return non-nil if NUMBER is greater than zero."
  (declare (pure t) (side-effect-free error-free))
  (> number 0))

(defsubst oo-contains-all-p (list1 list2)
  "Return non-nil if"
  (declare (pure t) (side-effect-free error-free))
  (null (cl-set-difference list1 list2)))

(defsubst oo-same-items-as-p (list1 list2)
  "Return non-nil if LIST1 has the same items as LIST2"
  (declare (pure t) (side-effect-free error-free))
  (and (null (cl-set-difference list1 list2))
       (null (cl-set-difference list2 list1))))
(defun oo-cycle (list)
  "Return an infinite circular copy of LIST.
The returned list cycles through the elements of LIST and repeats
from the beginning."
  (declare (pure t) (side-effect-free t))
  ;; Also works with sequences that aren't lists.
  (let ((newlist (append list ())))
    (nconc newlist newlist)))
;;;; type conversion
(defun oo-into-string (&rest args)
  "Return ARGS as a string."
  (declare (pure t) (side-effect-free t))
  (with-output-to-string (mapc #'princ args)))

(defun oo-into-symbol (&rest args)
  "Return an interned symbol from ARGS."
  (declare (pure t) (side-effect-free t))
  (intern (apply #'oo-into-string args)))

(defun oo-into-keyword (&rest args)
  "Return ARGS as a keyword."
  (declare (pure t) (side-effect-free t))
  (apply #'oo-into-symbol ":" args))
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
;;;; uncategorized
(defun oo-wrap-forms (wrappers forms)
  "Return FORMS wrapped by WRAPPERS.
FORMS is a list of forms to be wrapped.  WRAPPERS are a list of forms
representing the wrappers to apply.  If WRAPPERS is empty, `progn' is added to
ensure the result is syntactically valid."
  (declare (pure t) (side-effect-free t))
  (unless wrappers (push '(progn) wrappers))
  (setq wrappers (reverse wrappers))
  (setq forms (append (pop wrappers) forms))
  (dolist (wrapper wrappers)
    (setq forms (append wrapper (list forms))))
  forms)

(defun oo-quoted-p (form)
  "Return non-nil if FORM is quoted."
  (declare (pure t) (side-effect-free t))
  (equal (car-safe form) 'quote))

(defun oo-sharpquoted-p (form)
  "Return non-nil if form is sharpquoted."
  (declare (pure t) (side-effect-free t))
  (equal (car-safe form) 'function))

(defun oo-ensure-quote (form)
  "Return quoted form unquoted, otherwise return form."
  (declare (pure t) (side-effect-free t))
  (if (oo-quoted-p form) form (macroexp-quote form)))

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
;;;; logging
(defun oo-log (type message &rest args)
  "Log to *Messages* buffer."
  (when oo-debug-p
    (apply #'message "[%s] %s" (upcase (symbol-name type)) message args)))
;;;; hook
(cl-defun oo-add-hook (hook function &key depth local ignore-args)
  "Generate a function that calls FUNCTION and add it to HOOK.
Generated function call FUNCTION and logs any errors.  If IGNORE-ARGS, then do
generated function does not pass in any of its given arguments to FUNCTION."
  (let ((fname (intern (format "oo--%s--%s" hook function)))
        (funcall-form (if ignore-args `(,function) `(apply #',function arglist))))
    (unless (fboundp fname)
      (fset fname `(lambda (&rest arglist)
                     (ignore arglist)
                     ,(oo--hook-docstring hook function)
                     (info "HOOK: %s -> %s" ',hook ',function)
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
    (add-hook hook fname depth local)))
;;; provide
(provide 'base-utils)
;;; base-utils.el ends here
