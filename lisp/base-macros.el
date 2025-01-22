;;; base-macros.el --- Initialize base-macros -*- lexical-binding: t; -*-
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
;; Initialize base-macros.
;;
;;; Code:
;;;; requirements
(require 'base-utils)
;;;; macros
;;;;; nif!
;; More often than not when I am using `if', the default else clause is simpler than
;; the then clause.  And in that case I end up having to wrap the then clause in
;; a `progn'. I want to invert the else clause and the if clause so I do not
;; need to include the extra `progn' in that case.  I also considered just
;; writing a macro that expands to an `if' with the then and else reversed, but
;; I think it might be confusing.
(defmacro nif! (cond then &rest else)
  (declare (indent 2))
  `(if (not ,cond) ,then ,@else))
;;;;; anaphoric macros
(defmacro alet! (form &rest body)
  "Bind the result FORM to `it' for the duration of BODY."
  (declare (debug let) (indent 1))
  `(let ((it ,form))
     ,@body))

(defmacro aand! (&rest conditions)
  "Like `and' but bind the result of first condition to `it'."
  `(alet! ,(car conditions)
     (and it ,@(cdr conditions))))

(defmacro aif! (cond then &rest else)
  "Like `if' but bind the result of COND to `it' for duration of THEN and ELSE."
  (declare (debug t) (indent 2))
  `(alet! ,cond (if it ,then ,@else)))

(defmacro awhen! (cond &rest body)
  "Like `when' but the result of COND is bound to `it'."
  (declare (debug when) (indent 1))
  `(aif! ,cond (progn ,@body) nil))

(defmacro aprog1! (form &rest body)
  "Like `prog1' but bind first form to `it'."
  (declare (debug when) (indent 1))
  `(alet! ,form (prog1 it ,@body)))

(defmacro each! (list &rest body)
  "Like `prog1' but bind first form to `it'."
  (declare (debug (form body)) (indent 1))
  `(dolist (it ,list) ,@body))

(defmacro alet2! (form1 form2 &rest body)
  "Bind FORM1 and FORM2 to `it' and `other' and evaluate BODY."
  (declare (debug let) (indent 2))
  `(let ((it ,form1)
         (other ,form2))
     ,@body))
;;;;; lef!
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
;;;;; quietly!
(defmacro quietly! (&rest body)
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
;;;;; with-map!
(defun oo--generate-with-map-body (map body &optional use-keywords-p)
  "Return a list of let-bindings for `with-map!'.
Collect symbols matching REGEXP in BODY into an alist."
  (cl-flet* ((into-symbol (&rest args)
               (intern (with-output-to-string (mapc #'princ args))))
             (into-keyword (&rest args)
               (apply #'into-symbol ":" args)))
    (let* ((mapsym (gensym "map"))
           (let-binds `((,mapsym ,map)))
           (name nil)
           (symbol nil)
           (key nil)
           (key-fn (if use-keywords-p #'into-keyword #'into-symbol)))
      (dolist (obj (flatten-tree body))
        (when (and obj
                   (symbolp obj)
                   (string-match "\\(!\\{1,2\\}\\)\\([^[:space:]]+\\)"
                                 (symbol-name obj))
                   (not (assoc obj let-binds)))
          (setq symbol obj)
          (setq name (symbol-name symbol))
          (setq key (funcall key-fn (match-string 2 name)))
          (if (= 1 (length (match-string 1 name)))
              (push `(,symbol (map-elt ,mapsym ',key)) let-binds)
            (push `(,symbol (map-contains-key ,mapsym ',key)) let-binds))))
      (nreverse let-binds))))

(defmacro with-map-keywords! (map &rest body)
  "Let-bind bang symbols in BODY corresponding to keywords in MAP."
  (declare (indent 1))
  `(let* ,(oo--generate-with-map-body map body :use-keywords)
     ,@body))

(defmacro with-map! (map &rest body)
  "Let-bind bang symbols in BODY to corresponding keys in MAP.
Occurrences of !SYMBOL are let-bound to the result of evaluating (map-elt MAP
SYMBOL).  Occurrences of !!SYMBOL is let-bound to the result of evaluating
(map-contains-key MAP SYMBOL)."
  (declare (indent 1))
  `(let* ,(oo--generate-with-map-body map body)
     ,@body))
;;;;; opt!
(defmacro opt! (symbol value)
  "Set SYMBOL to VALUE when parent feature of SYMBOL is loaded.
This is like `setq' but it is meant for configuring variables."
  (let ((value-var (gensym "value")))
    `(if (not (boundp ',symbol))
         ;; This quote on he lambda is needed to avoid infinite recursion.
         (push '(lambda () (opt! ,symbol ,value))
               (gethash ',symbol oo-after-load-hash-table))
       (let ((,value-var (with-demoted-errors "Error: %S" (with-no-warnings ,value))))
         (aif! (get ',symbol 'custom-set)
             (funcall it ',symbol ,value-var)
           (with-no-warnings (setq ,symbol ,value-var)))))))
;;;;; destructive modification macros
(cl-defmacro appending! (place list &key (setter 'setf))
  "Append LIST to the end of PLACE.
SETTER is the symbol of the macro or function used to do the setting."
  `(,setter ,place (append ,place ,list)))

;; Important to note that this macro is not as efficient as pushing because it's
;; adding to the end of the list.  So this macro should be used only in
;; non-performance-intensive code.  In performance-intensive code we need the
;; =push-nreverse= idiom.
(cl-defmacro collecting! (place item &key (setter 'setf))
  "Affix ITEM to the end of PLACE.
SETTER is the same as in `appending!'."
  `(,setter ,place (append ,place (list ,item))))

(defalias 'snocing! 'collecting!)
(defalias 'affixing! 'collecting!)

;; You might be wondering why I didn't create a macro with a setter for these.
;; Well I haven't had a case yet when I've wanted to increment or decrement the
;; value symbol used in customization.
(defalias 'incrementing! 'cl-incf)
(defalias 'counting! 'cl-incf)
(defalias 'decrementing! 'cl-decf)

(cl-defmacro prepending! (place list &key (setter 'setf))
  "Prepend LIST to beginning of PLACE.
SETTER is the same as in `appending!'."
  `(,setter ,place (append ,list ,place)))

(cl-defmacro maxing! (place form &key (setter 'setf) (comparator '>))
  "Set PLACE to the greater of PLACE and FORM.
SETTER is the same as in `appending!'.  COMPARATOR is the comparison function
to determine the greater value."
  (cl-with-gensyms (value1 value2)
    `(,setter ,place (let ((,value1 ,form)
                           (,value2 ,place))
                       (if (,comparator ,value1 ,value2) ,value1 ,value2)))))

(cl-defmacro minning! (place form &key (setter 'setf) (comparator '<))
  "Set PLACE to the lesser of PLACE and FORM.
SETTER is the same as in `appending!'.  COMPARATOR is used to determine the
lesser value."
  `(maxing! ,place ,form :setter ,setter :comparator ,comparator))

(cl-defmacro concating! (place string &key (setter 'setf) separator)
  "Concat PLACE and STRING with SEPARATOR.
SETTER is the same as in `appending!'"
  `(,setter ,place (string-join (list ,place ,string) ,separator)))

(cl-defmacro adjoining! (place item &key test test-not key (setter 'setf))
  "Set PLACE to the value of `(cl-adjoin ITEM PLACE)'.
SETTER is the same as in `appending!'.  KEY, TEST, TEST-NOT are the same as in
`cl-adjoin'."
  `(,setter ,place (cl-adjoin ,item ,place :test ,test :test-not ,test-not :key ,key)))

(defalias 'adjoin! 'adjoining!)

;; I know =push= already exists.  But I want a variant of push that can be used
;; with the =autolet!= macro.
(cl-defmacro pushing! (place item &key (setter 'setf))
  "Cons ITEM to PLACE.
SETTER is the same as in `appending!'."
  `(,setter ,place (cons ,item ,place)))

;; To configure variables I don't use the standard =setq=--at least not
;; directly.  Instead, I use =set!=.  Adjoining is one of the most common
;; operations done to lisp symbols when configuring Emacs.

;; Something I was always confused about was why adjoin instead of just using
;; =push=.  The latter is more performant; however I don't think that's.  The
;; best reason I could think of is that sometimes you want to re-evaluate parts
;; of your configuration and in that case it is more convenient to have =adjoin=
;; over =push=.
(cl-defmacro unioning! (place list &key test test-not key (setter 'setf))
  "Set PLACE to the union of PLACE and LIST.
SETTER, KEY, TEST, TEST-NOT are the same as in `adjoining!'."
  `(,setter ,place (cl-union ,place ,list :test ,test :test-not ,test-not :key ,key)))

(defalias 'adding! 'cl-incf)
(defalias 'summing! 'cl-incf)
(defalias 'subtracting! 'cl-decf)
(defalias 'minusing! 'cl-decf)
;;;;; autolet!
;;;;;; control flow macros
(defmacro return! (&optional value)
  "Exit `autolet!' and return VALUE.
Inside an `autolet!' form, throw a `return!' signal, immediately terminating the
evaluation of the `autolet!' form and return VALUE."
  `(throw 'return! ,value))

(defmacro done! ()
  "This is a shorthand for `(return! nil)'."
  `(return! nil))

(defmacro break! (&optional value)
  "Exit the current loop and return VALUE.
Inside an `autolet!' form, exit the current loop and return VALUE."
  `(throw 'break! ,value))

(defmacro continue! ()
  "Skip the current iteration of a loop.
Inside an `autolet!' form, throw a `continue!' signal to end the current
iteration and move to the next."
  `(throw 'continue! nil))

(defalias 'skip! 'continue!)
;;;;;; let-binding stubs
(defmacro stub! (name args &rest body)
  "Indicator for defining local functions via `cl-flet' in `autolet!' forms."
  (declare (indent defun))
  (ignore name args body))
(defalias 'macrolet! 'stub! "Indicator for defining local macros via
`cl-macrolet' in `autolet!' forms.")
(defalias 'mlet! 'macrolet!)
(defalias 'flet! 'stub! "Same as `stub!'.")
(defalias 'noflet! 'stub! "Indicator for temporary overriding function
definitions via `lef!'.")
(defalias 'nflet! 'stub! "Same as `noflet!'")
;;;;;; helpers
(defmacro oo--autolet-inits (bodysym)
  "Extract let-bindings and excluded let-binding symbols from BODYSYM.
BODYSYM is the symbol whose value is the body."
  (let ((inits (gensym "inits"))
        (noinits (gensym "noinits"))
        (letbind (gensym "letbind")))
    `(let (,inits ,noinits)
       (while (pcase ,bodysym
                (`(:init ,(pred listp) . ,(guard t))
                 (pop ,bodysym)
                 (dolist (,letbind (pop ,bodysym))
                   (pcase ,letbind
                     ((pred symbolp)
                      (push (list ,letbind nil) ,inits))
                     (`(,_)
                      (push (append ,letbind (list nil)) ,inits))
                     (`(,_ ,_)
                      (push ,letbind ,inits))))
                 t)
                (`(:noinit ,(pred listp) . ,(guard t))
                 (pop ,bodysym)
                 (setq ,noinits (append ,noinits (pop ,bodysym)))
                 t)))
       (list ,inits ,noinits))))

(defun oo--autolet-data (body)
  "Return let-bindings and processed forms used in `autolet!'.
Identify and collect symbols needed for let bindings and return forms modified."
  (pcase-let ((`(,init ,noinit) (oo--autolet-inits body))
              (bindings nil)
              (lets '((mlet! . cl-macrolet)
                      (macrolet! . cl-macrolet)
                      (nflet! . lef!)
                      (noflet! . lef!)
                      (flet! . cl-flet)
                      (stub! . cl-flet)
                      (label! . cl-labels)
                      (labels! . cl-labels))))
    (cl-flet ((quote-symbol-p (x) (memq x '(quote function backquote cl-function)))
              (loop-symbol-p (x) (memq x '(while dolist dotimes for!)))
              (ing-symbol-p (x) (and (symbolp x) (string-match-p "ing!$" (symbol-name x))))
              (letbind-symbol-p (x) (assoc x lets))
              (should-remove-p (x) (or (member (car x) noinit) (assoc (car x) init))))
      (cl-labels ((process-form (form)
                    (pcase form
                      ;; Leave quoted forms as-is.
                      (`(,(pred quote-symbol-p) . ,_)
                       form)
                      ;; Match `(set! VAR VALUE)` and collect VARIABLE.
                      (`(set! ,pattern ,_ . ,(guard t))
                       (if (symbolp pattern)
                           (cl-pushnew (list pattern nil) bindings :key #'car)
                         (dolist (symbol (reverse (oo-flatten-pcase-match-form pattern)))
                           (cl-pushnew (list symbol nil) bindings :key #'car)))
                       form)
                      ;; Surround loops with a catch.
                      (`(,(and loop (pred loop-symbol-p)) ,pred . ,(and body (guard t)))
                       `(catch 'break! (,loop ,pred (catch 'continue! ,@(process-form body)))))
                      ;; Properly initialize variables in ingmacro declarations.
                      ;; Just for brevity I use string-match to check instead
                      ;; of listing all my ing macros but there has been a clash
                      ;; with org-ml that uses some macros that end in "ing!".
                      (`(,(and name (pred ing-symbol-p)) ,symbol . ,(guard t))
                       (cl-case name
                         ((maxing! maximizing!)
                          (cl-pushnew `(,symbol most-negative-fixnum) bindings))
                         ((minning! minimizing!)
                          (cl-pushnew `(,symbol most-positive-fixnum) bindings))
                         ((summing! adding! counting!)
                          (cl-pushnew `(,symbol 0) bindings))
                         (t
                          (cl-pushnew `(,symbol nil) bindings :key #'car)))
                       form)
                      ;; Handle special let shortcuts.
                      (`((,(and macro (pred letbind-symbol-p)) . ,args) . ,(and rest (guard t)))
                       `((,(alist-get macro lets) ((,@args)) ,@(process-form rest))))
                      ((pred null)
                       form)
                      ;; Recurse into lists.
                      ((pred listp)
                       (cons (process-form (car form)) (process-form (cdr form))))
                      ;; Leave other forms untouched.
                      (_
                       form))))
        (setq body (process-form body)))
      (setq bindings (append init (cl-remove-if #'should-remove-p bindings))))
    (list bindings body)))
;;;;;; main macro
;; Sometimes you do not want symbol to be auto let-bound to nil, you actually
;; want to just modify the original symbol without let-binding it at all.  In
;; that case use `:noinit' which tells `autolet!' not to bind specified symbols
;; at all.  Other times you want a symbol to be bound to something else than the
;; default.  For example, counting! starts at 0 by default but maybe you want to
;; start at 10, in that case you can do `:init' ((count 10)).  I suppose init
;; can be used as a single-line alternative to `let*'.
(defmacro autolet! (&rest body)
  "Dynamically let-bind symbols and modify forms in BODY.

Process BODY by recognizing special forms and keywords for dynamically
let-binding symbols, automatically wrapping forms and enhancing the control flow
of loops.

Keywords:
:init BINDINGS    Predefine symbol bindings.  Follow the same format as `let*'.
This takes precedence over other let-binding indicators.
:noinit SYMS      Do not let bind any symbols in SYMS, regardless of whether
those symbols where specified by dynamic let-binding indicators.

Dynamic let-binding:
(set! SYM _)      Let bind SYM to nil.
(maxing! SYM _)   Let bind SYM to `most-negative-fixnum'.
(minning! SYM _)  Let bind SYM to `most-positive-fixnum'.
(counting! SYM _) Let bind SYM to 0.
(...ing! SYM VAL) Let bind SYM to nil.

Wrapping forms:
(mlet!|macrolet! NAME ARGS . BODY) Wrap subsequent forms with
`(cl-macrolet ((NAME ARGS . BODY)))'.
(stub!|flet! NAME ARGS . BODY)     Same as macrolet but use `cl-letf'.
(nflet!|noflet! NAME ARGS . BODY)  Same as `stub!' but use `lef!'.
(label!|labels! NAME ARGS . BODY)  Same as `stub!' but use `cl-labels'.

Enhanced looping control flow:
(while|dotimes|dolist CONDITION . BODY) Replace with
`(catch \='return! (LOOP CONDITION (catch \='break! BODY)))'."
  (pcase-let ((`(,bindings ,body) (oo--autolet-data body)))
    `(let ,bindings (catch 'return! ,@body))))
;;;;; defun! and defmacro!
(defun oo--arglist-symbols (arglist)
  "Return a list of argument symbols."
  (let (symbols)
    (dolist (item (flatten-list arglist))
      (when (and (symbolp item) (not (string-match "^&" (symbol-name item))))
        (push item symbols)))
    (nreverse symbols)))

(defun oo--definer-components (args)
  (let ((name (pop args))
        (arglist (pop args))
        (doc (and (stringp (car args)) (pop args)))
        (decl (and (equal 'declare (car-safe (car args))) (pop args)))
        (inte (and (equal 'interactive (car-safe (car args))) (pop args))))
    (list name arglist (cl-remove-if #'null (list doc decl inte)) args)))

(defmacro defmacro! (&rest args)
  "Same as `defmacro!' but wrap body with `autolet!'.
NAME, ARGLIST and BODY are the same as `defmacro!'.

\(fn NAME ARGLIST [DOCSTRING] BODY...)"
  (declare (indent defun) (doc-string 3))
  (cl-destructuring-bind (name arglist metadata body) (oo--definer-components args)
    `(defmacro ,name ,arglist
       ,@metadata
       (autolet! :noinit ,(oo--arglist-symbols arglist)
                 ,@body))))

(defmacro defun! (&rest args)
  "Same as `defun' but wrap body with `autolet!'.
NAME, ARGS and BODY are the same as in `defun'.

\(fn NAME ARGLIST [DOCSTRING] [DECL] [INTERACTIVE] BODY...)"
  (declare (indent defun) (doc-string 3))
  (cl-destructuring-bind (name arglist metadata body) (oo--definer-components args)
    `(defun ,name ,arglist
       ,@metadata
       (autolet! :noinit ,(oo--arglist-symbols arglist)
                 ,@body))))
;;;;; for!
(defmacro for! (loop-struct &rest body)
  "A generic looping macro and drop-in replacement for `dolist'.
BODY is the body of the loop.  LOOP-STRUCT determines how `for!' loops and can
take the following forms:

(VAR NUMBER) Same as `dotimes'.

(MATCH-FORM SEQUENCE) Evaluate BODY for every element in sequence.

(reverse MATCH-FORM LIST) Evaluate body for each element of LIST in reversed order.

(repeat n) Evaluate BODY N times where (> n 0)."
  (declare (indent 1))
  (pcase loop-struct
    ((or (and (pred integerp) n) `(repeat ,n))
     `(dotimes (_ ,n) ,@body))
    (`(reverse ,match-form ,list)
     (let ((v (make-symbol "vector"))
           (i (make-symbol "i")))
       `(let* ((,v (vconcat ,list))
               (,i (length ,v)))
          (while (> ,i 0)
            (setq ,i (1- ,i))
            (pcase-let* ,(oo-pcase-bindings match-form `(aref ,v ,i))
              ,@body)))))
    (`(,(and match-form (or (pred listp) (pred vectorp))) ,list)
     (cl-with-gensyms (elt)
       `(for! (,elt ,list)
          (pcase-let* ,(oo-pcase-bindings match-form elt)
            ,@body))))
    (`(,(and elt (pred symbolp)) ,list)
     (cl-once-only (list)
       `(cond ((listp ,list)
               (dolist (,elt ,list) ,@body))
              ((sequencep ,list)
               (seq-doseq (,elt ,list) ,@body))
              ((integerp ,list)
               (dotimes (,elt ,list) ,@body))
              (t
               (error "Unknown list predicate: %S" ',loop-struct)))))))
;;;;; after!
;; I made the decision to add a hook function to a hook regardless of whether
;; the hook has already has been run.  But if the hook has been run the hook
;; function is called individually.  The idea is that I do not want to just
;; evaluate the body and have no record of it being evaluated other than it is
;; side-effects.
(defmacro defafter! (name expr &rest body)
  "Evaluate BODY after EXPR is satisfied."
  (declare (indent defun))
  `(progn
     (defun! ,name nil (with-no-warnings ,@body))
     (oo-call-after-load ',expr #',name)))
;;;;; defhook!
(defmacro hook! (hook function &rest args)
  "Configuration wrapper around `oo-add-hook'."
  `(progn (declare-function ,function nil)
          (oo-add-hook ',hook #',function ,@args)))

(defmacro! defhook! (name args &rest body)
  "Add function to hook as specified by NAME."
  (declare (indent defun))
  (while (aand! (car args) (symbolp it) (not (keywordp it)))
    (collecting! hooks (pop args)))
  (dolist (hook hooks)
    (collecting! hook-forms `(oo-add-hook ',hook ',name ,@args)))
  (when (stringp (car body))
    (collecting! metadata (pop body)))
  (when (equal 'declare (car-safe (car body)))
    (collecting! metadata (pop body)))
  `(progn
     (defun! ,name nil ,@metadata ,@body)
     ,@hook-forms))
;;;;; setq-hook
(defmacro! setq-hook! (hooks symbol value)
  "Add function to hook that sets the local value of SYMBOL to VALUE."
  (dolist (hook (ensure-list hooks))
    (set! name (intern (format "oo--%s--set-local-var--%s" hook symbol)))
    (set! docstring (format "Set local variable `%S' to `%S'." ',symbol ',value))
    (set! lambda `(lambda (&rest _)
                    ,docstring
                    (info! "HOOK: %s -> %s" ',hook ',name)
                    (condition-case err
                        (setq-local ,symbol ,value)
                      (error
                       (error! "%s error in local hook %s because of %s"
                               (car err)
                               ',hook
                               (cdr err))))))
    (appending! forms `((fset ',name ,lambda) (add-hook ',hook #',name nil nil))))
  (macroexp-progn forms))
;;;;; set!
(defmacro set! (match-form value)
  "Bind symbols in PATTERN to corresponding VALUE.
If MATCH-FORM is a symbol act as `setq'."
  (if (symbolp match-form)
      `(setq ,match-form ,value)
    (cl-flet ((list-marker-p (it) (and (symbolp it) (equal ?& (aref (symbol-name it) 0)))))
      (let* ((binds (oo-pcase-bindings match-form value))
             (non-gensyms (cl-remove-if #'list-marker-p (oo-flatten-pcase-match-form match-form)))
             (all (oo-flatten-pcase-match-form (mapcar #'car binds)))
             (gensyms (cl-set-difference all non-gensyms)))
        `(let ,gensyms
           ,(macroexp-progn (mapcar (apply-partially #'cons 'pcase-setq) binds)))))))
;;;;; logging
(info! "do this %s" 4)
(defmacro info! (msg &rest meta)
  (when oo-debug-p
    `(oo-log 'info ,msg ,@meta)))

(defmacro error! (msg &rest meta)
  (when oo-debug-p
    `(oo-log 'error ,msg ,@meta)))

(defmacro warn! (msg &rest meta)
  (when oo-debug-p
    `(oo-log 'warn ,msg ,@meta)))

(defmacro fatal! (msg &rest meta)
  (when oo-debug-p
    `(oo-log 'fatal ,msg ,@meta)))

(defmacro trace! (msg &rest meta)
  (when oo-debug-p
    `(oo-log 'trace ,msg ,@meta)))

(defmacro debug! (msg &rest meta)
  (when oo-debug-p
    `(oo-log 'debug ,msg ,@meta)))
;;; provide
(provide 'base-macros)
;;; base-macros.el ends here
