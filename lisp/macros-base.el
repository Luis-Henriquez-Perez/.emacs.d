;;; macros-base.el --- Define base macros. -*- lexical-binding: t; -*-
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
;; Define base macros.
;;
;;; Code:
;;;; generic
(defmacro o-time-elapsed (&rest forms)
  "Eval forms and return the time elapsed."
  (let ((start (make-symbol "start")))
    `(let ((,start (float-time)))
       ,(macroexp-progn forms)
       (/ (fround (* (- (float-time) ,start) 100)) 100.0))))

(defmacro o-require (feature)
  "Require feature in lisp directory.
If FEATURE is a regexp, require all features in lisp directory that match
FEATURE."
  (pcase feature
    ((pred stringp)
     (let (forms filename (regexp feature))
       (dolist (file (directory-files (expand-file-name "lisp/" user-emacs-directory) 'full ".+\\.el$"))
         (setq filename (file-name-sans-extension (file-name-nondirectory (directory-file-name file))))
         (when (string-match-p regexp filename)
           (setq feature (intern filename))
           (push `(o-require ,feature) forms)))
       (macroexp-progn (reverse forms))))
    ((pred symbolp)
     (let (forms)
       (setq forms `((require ',feature)))
       (setq forms (let ((err (gensym "error")))
                     `((if o-init-noerrors
                           (condition-case ,err
                               ,(macroexp-progn forms)
                             (error
                              (o-log 'failure "Failed to require %S: %s -> %s." ',feature (car ,err) (cdr ,err))))
                         ,(macroexp-progn forms)))))
       (setq forms `((if o-init-profile
                         (o-aprog1 (o-time-elapsed ,(macroexp-progn forms))
                           (o-log 'success "Required %s in %.2f seconds" ',feature it)
                           (push (list ',feature it) (get-register :require-times)))
                       ,(macroexp-progn forms))))
       ;; Ensure main forms are not evaluated more than once.
       (setq forms `((unless (featurep ',feature)
                       ,(macroexp-progn forms))))
       (when (string-match-p "macros$" (symbol-name feature))
         (setq forms `((eval-when-compile ,(macroexp-progn forms)))))
       (macroexp-progn forms)))
    (_
     (signal 'wrong-type-argument `(or stringp symbolp ,feature)))))

(defmacro o-lef (bindings &rest body)
  "Bind each symbol in BINDINGS to its corresponding function during BODY.
BINDINGS is a list of either (SYMBOL FUNCTION), where symbol is the symbol to be
bound and FUNCTION is the function to bind it to; or (SYMBOL ARGS BODY).  In
each of BINDINGS if the symbol is an existing function symbol let-bind the
original function to `this-fn', otherwise bind `this-fn' to nil."
  (declare (indent 1))
  (let (binds orig-fn)
    (pcase-dolist (`(,sym . ,rest) bindings)
      (setq orig-fn (gensym "this-fn"))
      (push `(,orig-fn (when (fboundp ',sym) (symbol-function ',sym))) binds)
      (push (list `(symbol-function ',sym)
                  (pcase rest
                    (`(,fn . nil)
                     `(lambda (&rest args)
                        (let ((this-fn ,orig-fn)
                              (this-function ,orig-fn))
                          (ignore this-fn this-function)
                          (apply ,fn args))))
                    (`(,args . ,function-body)
                     `(lambda ,args
                        (let ((this-fn ,orig-fn)
                              (this-function ,orig-fn))
                          (ignore this-fn this-function)
                          ,@function-body)))))
            binds))
    `(cl-letf* ,(nreverse binds) ,@body)))

(defmacro o-quiet (&rest body)
  "Run BODY without generating any output.
Silence calls to `message', `load', `write-region' and anything that
writes to `standard-output'."
  `(let ((inhibit-message t)
         (save-silently t)
         (standard-output #'ignore))
     (o-lef ((message #'ignore)
            (load
             (lambda (file &optional noerror nomessage nosuffix must-suffix)
               (ignore nomessage)
               (funcall this-fn file noerror t nosuffix must-suffix)))
            (write-region
             (lambda (start end filename &optional append visit lockname mustbenew)
               (unless visit (setq visit 'no-message))
               (funcall this-fn start end filename append visit lockname
                        mustbenew))))
       ,@body)))

(defmacro o-stripplist (list)
  "Strip and return plist from the front of LIST.
LIST is a list symbol."
  (let ((plist (gensym "plist")))
    `(let (,plist)
       (while (keywordp (car ,list))
         (setq ,plist (append (list (pop ,list) (pop ,list)) ,plist)))
       ,plist)))
;;;; anaphora
(defmacro o-alet (form &rest body)
  "Bind the result FORM to `it' for the duration of BODY."
  (declare (debug let) (indent 1))
  `(let ((it ,form))
     ,@body))

(defmacro o-aand (&rest conditions)
  "Like `and' but bind the result of first condition to `it'."
  `(o-alet ,(car conditions)
     (and it ,@(cdr conditions))))

(defmacro o-and (&rest conditions)
  "Like `o-aand' but bind the result of each condition to `it'."
  `(let (it) (and ,@(mapcar (lambda (c) `(setq it ,c)) conditions))))

(defmacro o-aif (cond then &rest else)
  "Like `if' but bind the result of COND to `it' for duration of THEN and ELSE."
  (declare (debug t) (indent 2))
  `(o-alet ,cond (if it ,then ,@else)))

(defmacro o-awhen (cond &rest body)
  "Like `when' but the result of COND is bound to `it'."
  (declare (debug when) (indent 1))
  `(o-aif ,cond (progn ,@body) nil))

(defmacro o-aprog1 (form &rest body)
  "Like `prog1' but bind first form to `it'."
  (declare (debug when) (indent 1))
  `(o-alet ,form (prog1 it ,@body)))

(defmacro o-each (list &rest body)
  "Evaluate BODY for each element of LIST and return nil.
Each element of LIST is bound to `it'."
  (declare (debug (form body)) (indent 1))
  `(dolist (it ,list) ,@body))

(defmacro o-alet2 (form1 form2 &rest body)
  "Bind FORM1 and FORM2 to `it' and `other' and evaluate BODY."
  (declare (debug let) (indent 2))
  `(let ((it ,form1)
         (other ,form2))
     ,@body))

(defmacro o-aremf (list pred)
  "Remove the first element that satisfies PRED and return PRED.
-PRED should be a form that evaluates with `it` bound to each element."
  (declare (indent 1))
  (let ((glist (gensym "list"))
        (gpred (gensym "pred"))
        (grest (gensym "rest")))
    `(let* ((,glist ,list)
            (,grest nil)
            (,gpred nil)
            (it nil))
       (while ,glist
         (setq it (car ,glist))
         (setq ,gpred ,pred)
         (if ,gpred
             (progn
               (setq ,list (nconc (nreverse ,grest) (cdr ,glist)))
               (setq ,glist nil)) ; exit loop
           (push it ,grest)
           (setq ,glist (cdr ,glist))))
       ,gpred)))
;;;; place macros
(defmacro o-appending (place list)
  "Append LIST to the end of PLACE.
SETTER is the symbol of the macro or function used to do the setting."
  `(setf ,place (append ,place ,list)))

;; Important to note that this macro is not as efficient as pushing because it's
;; adding to the end of the list.  So this macro should be used only in
;; non-performance-intensive code.  In performance-intensive code we need the
;; =push-nreverse= idiom.
(defmacro o-collecting (place item)
  "Affix ITEM to the end of PLACE.
SETTER is the same as in `o-appending'."
  `(setf ,place (append ,place (list ,item))))

(defmacro o-prepending (place list)
  "Prepend LIST to beginning of PLACE.
SETTER is the same as in `o-appending'."
  `(setf ,place (append ,list ,place)))

;; I know =push= already exists.  But I want a variant of push that can be used
;; with the =o-autolet= macro.
(defmacro o-pushing (place item)
  "Cons ITEM to PLACE.
SETTER is the same as in `o-appending'."
  `(setf ,place (cons ,item ,place)))

(defmacro o-summing (place num)
  `(setf ,place (+ ,place ,num)))
;;; provide
(provide 'macros-base)
;;; macros-base.el ends here
