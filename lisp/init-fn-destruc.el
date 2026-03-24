;;; init-fn-destruc.el --- Destructuring utilities -*- lexical-binding: t; -*-
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
;; Destructuring utilities.
;;
;;; Code:
;; This function of course is not only for destructuring but now its what I am
;; using it for.
(defun o--destruc-map-nodes (pred fn tree)
  "Recursively map FN over tree nodes satisfying PRED.

PRED is a predicate function applied to each node in TREE.  TREE can be a nested
list, vector or improper list.  Return a new tree with FN applied to the nodes
matching PRED."
  (cond ((funcall pred tree)
         (funcall fn tree))
        ((consp tree)
         (cons (o-destruct--map-nodes pred fn (car tree))
               (o-destruct--map-nodes pred fn (cdr tree))))
        ((vectorp tree)
         `[,@(mapcar (apply-partially #'o-destruct--map-nodes pred fn)
                     (append tree nil))])
        (t
         tree)))

(defun o--destruc-convert-to-pcase (match-form)
  "Return a pcase-style pattern from MATCH-FORM.

MATCH-FORM is a potentially nested structure containing lists, vectors, and/or
symbols."
  (if (symbolp match-form)
      match-form
    (cl-flet ((true-symbolp (o) (and o (symbolp o)))
              (add-comma (o) (list '\, o)))
      (list '\` (o-destruct--map-nodes #'true-symbolp #'add-comma match-form)))))

(defun o--destruc-get-special-match-form-let-bindings (match-form value)
  "Return a list of `let*` bindings for.

MATCH-FORM is a destructuring pattern to be matched.  A MATCH-FORM
constitutes one of the following structures:

(&butlast ALLBUTLAST LAST)

This binds the value of current expression to WHOLE.

(&as WHOLE PARTS)

Bind the value of current expression to WHOLE.

(&key KEY . KEYS)

Bind each symbol in KEYS to (plist-get MATCH-FORM KEY)

(&map KEY . KEYS)

Bind each symbol in key to (map-elt MATCH-FORM . KEY).

VALUE is the value being destructured.

If MATCH-FORM is not a special form, return nil."
  (pcase match-form
    (`(&butlast ,(and butlast (pred symbolp)) ,(and last (pred symbolp)))
     (let ((it (make-symbol "--O-DESTRUC-VALUE--BUTLAST--")))
       `((,it ,value)
         (,butlast (cl-loop while (nthcdr 1 ,it) collect (pop ,it)))
         (,last (car ,it)))))
    (`(,(or '&as '&whole) ,(and whole (pred symbolp)) ,parts)
     (let ((it (make-symbol "--O-DESTRUC-VALUE--AS--")))
       `((,it ,value)
         (,whole ,it)
         (,parts ,it))))
    (`(&key ,(and symbol (pred symbolp)) . ,(and symbols (guard t)))
     (let ((plist (make-symbol "--O-DESTRUC-VALUE--KEY--"))
           (bindings nil)
           (key nil))
       (dolist (sym (cons symbol symbols))
         (setq key (intern (concat ":" (symbol-name sym))))
         (push `(,sym (plist-get ,plist ,key)) bindings))
       (cons `(,plist ,value) (nreverse bindings))))
    (`(&map ,(and symbol (pred symbolp)) . ,(and symbols (guard t)))
     (let ((it (make-symbol "--O-DESTRUC-VALUE--MAP--"))
           (bindings nil))
       (dolist (s (cons symbol symbols))
         (push `(,s (map-elt ,it ,(intern (concat ":" (symbol-name s))))) bindings))
       (push `(,it ,value) bindings)
       (nreverse bindings)))
    (_
     nil)))

(defun o-destruc-inject-special-let-bindings (match-form value)
  "Generate bindings for special forms in MATCH-FORM relative to VALUE.

Process MATCH-FORM to identify and replace any special forms, returning a list
where the first element is a transformed match-form with special forms replaced
and subsequent elements are additional bindings required to handle the special
forms.

MATCH-FORM is a destructuring pattern that may include special forms (see
`o--destruc-get-special-match-form-let-bindings').  VALUE is the value to be matched and
destructured."
  (let ((special-let-binds nil)
        (mf-value (make-symbol "--DESTRUC-MF-VALUE--")))
    (cl-flet ((is-special-mf (mf)
                (when-let ((binds (o--destruc-get-special-match-form-let-bindings mf mf-value)))
                  (setq special-let-binds (append special-let-binds binds))))
              (replace-with-value (lambda (_) mf-value)))
      `((,(o-destruct--map-nodes #'is-special-mf #'replace-with-value match-form) ,value)
        ,@special-let-binds))))

(defun o-destruc-pcase-bindings (match-form value)
  "Generate pcase-compatible bindings from MATCH-FORM and VALUE.

MATCH-FORM is the destructuring pattern that specifies how VALUE should be
decomposed.  VALUE is the data to be matched and destructured.

Return a list of bindings compatible with `pcase`."
  (mapcar (pcase-lambda (`(,mf ,val)) (list (o--destruc-convert-to-pcase mf) val))
          (o-destruc-inject-special-let-bindings match-form value)))

(defun o--destruc-flatten-match-form (match-form)
  "Start refactoring."
  (let ((stack (list (if (vectorp match-form) (append match-form nil) match-form)))
        (symbols nil)
        (node nil))
    (while stack
      (cond ((null (car stack))
             (pop stack))
            ((listp (car stack))
             (push (pop (car stack)) stack))
            ((vectorp (car stack))
             (push (append (pop stack) nil) stack))
            ((nlistp (cdr-safe (car stack)))
             (push (list (caar stack) (cdaar stack)) stack))
            ((symbolp (car stack))
             (push (pop stack) symbols))
            (t
             (pop stack))))
    (nreverse (delete-dups symbols))))

(defun o-flatten-pcase-match-form (match-form)
  "Flatten MATCH-FORM into a list of components.

MATCH-FORM can contain nested lists or vectors. This function extracts all
symbols and other components, ensuring no duplicates.

Return a flat list of unique components in MATCH-FORM."
  (cl-set-difference (flatten-pattern match-form) '(\, \`)))

(defun o-destructure-defun-args (args)
  "Destructure the arguments of a \"defun-like\" thing.
Return a list of."
  (let ((name (pop args))
        (arglist (pop args))
        (doc (and (stringp (car args)) (pop args)))
        (decl (and (equal 'declare (car-safe (car args))) (pop args)))
        (inte (and (equal 'interactive (car-safe (car args))) (pop args))))
    (list name arglist (remove nil (list doc decl inte)) args)))

(defun o-arglist-symbols (arglist)
  "Return a list of argument symbols."
  (let (symbols)
    (dolist (arg (flatten-list arglist))
      (when (and (symbolp arg)
                 (not (equal arg '_))
                 (not (string-match "^&" (symbol-name arg))))
        (push arg symbols)))
    (nreverse symbols)))
;;; provide
(provide 'init-fn-destruc)
;;; init-fn-destruc.el ends here
