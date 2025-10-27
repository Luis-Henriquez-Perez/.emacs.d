;;; 001-base-functions.el -*- lexical-binding: t; -*-
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
;;;; miscellaneous
;; I don't yet know where to put this function.  So for now, here it goes.
(defun oo-popup-at-bottom (regexp)
  "Open buffers at bottom that match regexp."
  (push `(,regexp
          (display-buffer-at-bottom)
          (side bottom)
          (slot 1)
          (window-height 0.5)
          (window-parameters ((no-other-window t))))
        display-buffer-alist))

(defun oo-eval-and-replace-region (beg end)
  "Evaluate the region between BEG and END as Elisp, and replace it with the result.
If there's an error during evaluation, restore the original region and display the error message."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties beg end))
         (result (condition-case err
                     (eval (read text))
                   (error (progn
                            (message "Eval error: %s" (error-message-string err))
                            nil)))))
    (when result
      (delete-region beg end)
      (prin1 result (current-buffer)))))
;;;; predicates
(defun oo-in-string-or-comment-p ()
  "Return non-nil if point is in a string or comment.
Specifically, return the symbol `string' if point is in a string, the symbol
`comment' if in a comment and nil otherwise."
  (declare (side-effect-free error-free))
  (let ((ppss (syntax-ppss)))
    (cond ((nth 3 ppss) 'string)
          ((nth 4 ppss) 'comment)
          (t nil))))
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

(&butlast ALLBUTLAST LAST) Bind the value of current expression to WHOLE.

(&as WHOLE PARTS) Bind the value of current expression to WHOLE.

(&key KEY . KEYS) Bind each symbol in KEYS to (plist-get MATCH-FORM KEY)

(&map KEY . KEYS) Bind each symbol in key to (map-elt MATCH-FORM . KEY).

VALUE is the value being destructured.

If MATCH-FORM is not a special form, return nil."
  (pcase match-form
    (`(&butlast ,(and butlast (pred symbolp)) ,(and last (pred symbolp)))
     (let ((it (make-symbol "--butlast--")))
       `((,it ,value)
         (,butlast (cl-loop while (nthcdr 1 ,it) collect (pop ,it)))
         (,last (car ,it)))))
    (`(,(or '&as '&whole) ,(and whole (pred symbolp)) ,parts)
     (let ((it (make-symbol "--asmf--")))
       `((,it ,value)
         (,whole ,it)
         (,parts ,it))))
    (`(&key ,(and symbol (pred symbolp)) . ,(and symbols (guard t)))
     (let ((plist (make-symbol "--keymf--"))
           (bindings nil)
           (key nil))
       (dolist (sym (cons symbol symbols))
         (setq key (intern (concat ":" (symbol-name sym))))
         (push `(,sym (plist-get ,plist ,key)) bindings))
       (cons `(,plist ,value) (nreverse bindings))))
    (`(&map ,(and symbol (pred symbolp)) . ,(and symbols (guard t)))
     (let ((it (make-symbol "--mapmf--"))
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
    (setq match-form-value (gensym "mfvalue-"))
    (cl-flet ((special-mf-p (mf)
                (let ((it (oo-destructure-special-match-form mf match-form-value)))
                  (when it
                    (setq bindings (append bindings it)))
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

(defun oo-destructure-defun (args)
  "Destructure the arguments of a \"defun-like\" thing.
Return a list of."
  (let ((name (pop args))
        (arglist (pop args))
        (doc (and (stringp (car args)) (pop args)))
        (decl (and (equal 'declare (car-safe (car args))) (pop args)))
        (inte (and (equal 'interactive (car-safe (car args))) (pop args))))
    (list name arglist (remove nil (list doc decl inte)) args)))
;;; provide
(provide '001-base-functions)
;;; 001-base-functions.el ends here
