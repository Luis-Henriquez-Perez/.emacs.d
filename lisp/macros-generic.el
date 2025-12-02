;;; macros-generic.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; TODO: add commentary
;;
;;; Code:
;;;; generic
(defmacro time-elapsed! (&rest forms)
  "Eval forms and return the time elapsed."
  (let ((start (make-symbol "start")))
    `(let ((,start (float-time)))
       ,(macroexp-progn forms)
       (/ (fround (* (- (float-time) ,start) 100)) 100.0))))

(defmacro lef! (bindings &rest body)
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

(defmacro quiet! (&rest body)
  "Run BODY without generating any output.
Silence calls to `message', `load', `write-region' and anything that
writes to `standard-output'."
  `(let ((inhibit-message t)
         (save-silently t)
         (standard-output #'ignore))
     (lef! ((message #'ignore)
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

(defmacro stripplist! (list)
  "Strip and return plist from the front of LIST.
LIST is a list symbol."
  (let ((plist (gensym "plist")))
    `(let (,plist)
       (while (keywordp (car ,list))
         (setq ,plist (append (list (pop ,list) (pop ,list)) ,plist)))
       ,plist)))
;;;; anaphora
;;; provide
(provide 'macros-generic)
;;; macros-generic.el ends here
