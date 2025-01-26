;;; base-macros-bind.el --- binding macro -*- lexical-binding: t; -*-
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
;; This file provides a keybinding macro: `bind!'.
;;
;; This macro provides a homogeneous syntax for binding keys uniformly that
;; abstracts away several specific keybinding functions I would normally use
;; such as `define-key', `evil-define-key*' and `evil-define-minor-mode-key'.
;; Additionally, this macro handles several details.  For example, it properly
;; defers any binding if the keymap symbol provided for said binding is not yet
;; bound or if said binding evil keybinding and evil is not loaded.  Ultimately,
;; the point is to facilitate writing code that is concise and declarative
;; as opposed to code that is nested in `with-eval-after-load' forms.

;; Forms for different keybindings such as evil.  With this in mind, I decided
;; to implement the generation of forms as a series of steps taken based on the
;; existing metadata thereby providing flexibility and extensibility while
;; avoiding code duplication.  In terms of implementation, I diverged from all
;; my previous intents of writing this macro by making it iterative instead of
;; recursive for improved performance during macro-expansion.  The iterative
;; approach I use generates the main macro form by repeatedly setting.
;;
;; Once I use this macro I should ensure that every binding I bind is bound via
;; this macro because.
;;
;;; Code:
;;;; requirements
(require 'base-lib)
;;;; oo--let-bindings
;; I wanted a way to prevent select keys from being let-bound, the concrete case
;; was the character that I want to pass into
;; `oo-call-after-evil-state-character'.  I did not need to or want to save it
;; as a symbol before passing it in.  I decided to make it so that keys ending
;; in "-value" will not be let-bound.
(defun! oo--let-binds (metadata)
  (set! processed nil)
  (dolist (key (map-keys metadata))
    (set! name (symbol-name key))
    (if (string-match-p "[^[:space:]]+-value\\'" name)
        (setf (map-elt processed key) (map-elt metadata key))
      (setf (map-elt processed key) (make-symbol (seq-rest name)))
      (set! value-key (intern (concat name "-value")))
      (setf (map-elt processed value-key) (map-elt metadata key))))
  processed)
;;;; bind steps
;; These are functions that accept two arguments, METADATA and FORMS, and return
;; a list of forms.  METADATA is a mapping of keys and values.  The are used to
;; construct the list of forms `bind!' will expand into.
(defun oo--bind-define-key (metadata forms)
  "Prepend `define-key' form to FORMS as specified by METADATA."
  (with-map-keywords! metadata
    `((define-key ,!keymap ,!key ,!def)
      ,@forms)))

(defun oo--bind-evil-define-key (metadata forms)
  "Prepend `evil-define-key*' form to FORMS as specified by METADATA."
  (with-map-keywords! metadata
    `((declare-function evil-define-key* "evil")
      (evil-define-key* ,!state ,!keymap ,!key ,!def)
      (info! "KEYBINDING: %s %s %s -> %s" ,!state ,!keymap-symbol ,!key ,!def)
      ,@forms)))

(defun oo--bind-evil-define-minor-mode-key (metadata forms)
  "Prepend `evil-define-minor-key' form to FORMS as specified by METADATA."
  (with-map-keywords! metadata
    `((declare-function evil-define-minor-mode-key nil)
      (evil-define-minor-mode-key ,!state ,!mode ,!key ,!def)
      ,@forms)))

(defun oo--bind-kbd (_ forms)
  "Return new forms such that `define-key' are `kbd'.
For the duration of FORMS any key provided to the specific call of `define-key'
will be passed into `kbd' first."
  `((lef! ((define-key (lambda (keymap key def)
                         (setq key (if (stringp key) (kbd key) key))
                         (funcall this-fn keymap key def))))
      ,@forms)))

(defun! oo--bind-let-binds (metadata forms)
  "Let-bind symbols and values specified by METADATA to FORMS."
  (for! (key (map-keys metadata))
    (set! name (symbol-name key))
    (when (and (string-match "\\`\\(:[^[:space:]]+\\)-value\\'" name)
               (set! symbol (map-elt metadata (intern (match-string 1 name)))))
      (set! value (map-elt metadata key))
      (pushing! let-binds (list symbol value))))
  `((let ,let-binds ,@forms)))

;; TODO: not done.
(defun oo--bind-check-errors (_ forms)
  "Wrap FORMS with a `condition-case' block.
Evaluating resulting forms will."
  `((condition-case err
        ,(macroexp-progn forms)
      (error (if oo-debug-p
                 (signal (car err) (cdr err))
               ;; TODO: Give a better error message.
               (error! "Error %S with binding because of %S." (car err) (cdr err)))))))

(defun! oo--bind-which-key (metadata forms)
  "Wrap FORMS with an environment."
  (with-map-keywords! metadata
    (set! wk-fn #'which-key-add-keymap-based-replacements)
    `((lef! ((define-key (lambda (keymap key def)
                           (oo-call-after-load 'which-key (apply-partially #',wk-fn keymap key ,!wk))
                           (funcall this-fn keymap key def))))
        ,@forms))))

(defun! oo--bind-defer-evil-state-char (metadata forms)
  "Return a form that defers FORMS until evil state character is loaded."
  (with-map-keywords! metadata
    `((oo-call-after-evil-state-char ,!char-value (lambda (,!state-value) ,@forms)))))

(defun! oo--bind-defer-evil-state (_ forms)
  "Return a form that defers forms until evil is loaded."
  `((oo-call-after-load 'evil (lambda () ,@forms))))

(defun! oo--bind-defer-keymap (metadata forms)
  "Defer the evaluation of body until keymap is loaded.
If METADATA has no keymap return."
  (with-map-keywords! metadata
    (cond ((or (not !!keymap) (equal !keymap-value 'global-map) (not (symbolp !keymap)))
           forms)
          ;; Dired is the only package that I have encountered where using
          ;; `oo-call-after-bound' on its keymap does not work.  No idea why it
          ;; does not.  I assume that it is something about what happens between
          ;; the time the keymap is bound and the time where dired is provided.
          ;; In general dired is extremely sensitive as to when the bindings
          ;; as even this does not work in `oo-after-load-dired'.
          (t
           `((progn (defvar ,!keymap-value)
                    (if (bound-and-true-p ,!keymap-value)
                        (progn ,@forms)
                      (oo-call-after-bound ',!keymap-value (lambda () ,@forms)))))))))
;;;; standardize metadata
(defun! oo--bind-metadata (args)
  "Standardize ARGS into proper metadata."
  (flet! letter-to-char (lambda (obj) (string-to-char (symbol-name obj))))
  (flet! letterp (obj)
    (and (symbolp obj) (= 1 (length (symbol-name obj)))))
  (flet! keymap-symbol-p (obj)
    (and (symbolp obj) (string-match-p "[^[:space:]]+-map\\'" (symbol-name obj))))
  (flet! state-p (state)
    (and (symbolp state)
         (not (keywordp state))
         (not (keymap-symbol-p state))
         (not (letterp state))))
  (flet! letter-list-p (obj)
    (and (listp obj) (cl-every #'letterp obj)))
  (flet! symbol-list-p (obj)
    (and (listp obj) (cl-every #'symbolp obj)))
  (flet! non-keyword-symbol-p (obj)
    (and (symbolp obj) (not (keywordp obj))))
  (flet! not-keyword-p (lambda (obj) (not (keywordp obj))))
  (flet! plist-p (map)
    (or (null map)
        (and (listp map) (symbolp (car map)))))
  (pcase args
    ;; (bind! global-map "d" #'foo)
    (`(,(and (pred keymap-symbol-p) keymap) ,(and (pred not-keyword-p) key)
       ,(and (pred not-keyword-p) def) . ,(and (pred plist-p) plist))
     `((:keymap-symbol ',keymap :keymap ,keymap :key ,key :def ,def . ,plist)))
    ;; (bind! insert "d" #'foo)
    (`(,(and (pred state-p) state) ,(and (pred not-keyword-p) key)
       ,(and (pred not-keyword-p) def) . ,(and (pred plist-p) plist))
     `((:state ',state :keymap-symbol 'global-map :keymap global-map :key ,key :def ,def . ,plist)))
    ;; (bind! i org-mode-map "d" #'foo)
    (`(,(and (pred letterp) letter) ,(and (pred keymap-symbol-p) keymap)
       ,(and (pred not-keyword-p) key) ,(and (pred not-keyword-p) def)
       . ,(and (pred plist-p) plist))
     (set! char (string-to-char (symbol-name letter)))
     (set! state (make-symbol "state-value"))
     `((:char-value ,char :state ,state :keymap-symbol ',keymap :keymap ,keymap :key ,key :def ,def . ,plist)))
    ;; (bind! org-mode-map insert "d" #'foo)
    ;; (bind! insert org-mode-map "d" #'foo)
    (`(,(and (pred state-p) state) ,(and (pred keymap-symbol-p) keymap)
       ,(and (pred not-keyword-p) key) ,(and (pred not-keyword-p) def)
       . ,(and (pred plist-p) plist))
     `((:state ',state :keymap-symbol ',keymap :keymap ,keymap :key ,key :def ,def . ,plist)))
    ;; (bind! i "d" #'foo)
    (`(,(and (pred letterp) letter) ,(and (pred not-keyword-p) key)
       ,(and (pred not-keyword-p) def) . ,(and (pred plist-p) plist))
     (set! char (string-to-char (symbol-name letter)))
     (set! state (make-symbol "state-value"))
     `((:char-value ,char :state ,state :keymap-symbol 'global-map :keymap global-map :key ,key :def ,def . ,plist)))
    (`(,(and (pred keymap-symbol-p) keymap) ,(and (pred state-p) state)
       ,(and (pred not-keyword-p) key) ,(and (pred not-keyword-p) def)
       . ,(and (pred plist-p) plist))
     `((:state ',state :keymap-symbol ',keymap :keymap ,keymap :key ,key :def ,def . ,plist)))
    ;; (bind! org-mode-map i "d" #'foo)
    (`(,(and (pred keymap-symbol-p) keymap) ,(and (pred letterp) letter)
       ,(and (pred not-keyword-p) key) ,(and (pred not-keyword-p) def)
       . ,(and (pred plist-p) plist))
     (set! char (letter-to-char letter))
     (set! state (make-symbol "state-value"))
     `((:char-value ,char :state ,state :keymap-symbol ',keymap :keymap ,keymap :key ,key :def ,def . ,plist)))
    ;; (bind! (n m v) "d" #'foo)
    (`(,(and (pred letter-list-p) letters) ,(and (pred not-keyword-p) key)
       ,(and (pred not-keyword-p) def)
       . ,(and (pred plist-p) plist))
     (set! metadata `(:keymap-symbol 'global-map :keymap global-map :key ,key :def ,def . ,plist))
     (mapcar (lambda (it) (map-insert (map-insert metadata :char-value it) :state (make-symbol "state-value")))
             (mapcar #'letter-to-char letters)))
    ;; (bind! (normal insert visual) "d" #'foo)
    (`(,(and (pred symbol-list-p) states) ,(and (pred not-keyword-p) key)
       ,(and (pred not-keyword-p) def)
       . ,(and (pred plist-p) plist))
     (set! metadata `(:keymap-symbol 'global-map :keymap global-map :key ,key :def ,def . ,plist))
     (mapcar (lambda (it) (map-insert metadata :state (macroexp-quote it))) states))
    ;; (bind! org-mode-map (n m v) "d" #'foo)
    (`(,(and (pred keymap-symbol-p) keymap) ,(and (pred letter-list-p) letters)
       ,(and (pred not-keyword-p) key) ,(and (pred not-keyword-p) def)
       . ,(and (pred plist-p) plist))
     (set! metadata `(:keymap-symbol ',keymap :keymap ,keymap :key ,key :def ,def . ,plist))
     (mapcar (lambda (it) (map-insert (map-insert metadata :char-value it) :state (make-symbol "state-value")))
             (mapcar #'letter-to-char letters)))
    ;; (bind! org-mode-map (normal motion visual) "d" #'foo)
    (`(,(and (pred keymap-symbol-p) keymap) ,(and (pred symbol-list-p) states)
       ,(and (pred not-keyword-p) key) ,(and (pred not-keyword-p) def)
       . ,(and (pred plist-p) plist))
     (set! metadata `(:keymap-symbol ',keymap :keymap ,keymap :key ,key :def ,def . ,plist))
     (mapcar (lambda (it) (map-insert metadata :state (macroexp-quote it))) states))
    ;; (bind! (n m v) org-mode-map "d" #'foo)
    (`(,(and (pred letter-list-p) letters) ,(and (pred keymap-symbol-p) keymap)
       ,(and (pred not-keyword-p) key) ,(and (pred not-keyword-p) def)
       . ,(and (pred plist-p) plist))
     (set! metadata `(:keymap-symbol ',keymap :keymap ,keymap :key ,key :def ,def . ,plist))
     (mapcar (lambda (it) (map-insert (map-insert metadata :char-value it) :state (make-symbol "state-value")))
             (mapcar #'letter-to-char letters)))
    ;; (bind! (normal motion visual) org-mode-map "d" #'foo)
    (`(,(and (pred symbol-list-p) states) ,(and (pred keymap-symbol-p) keymap)
       ,(and (pred not-keyword-p) key) ,(and (pred not-keyword-p) def)
       . ,(and (pred plist-p) plist))
     (setq metadata `(:keymap-symbol ',keymap :keymap ,keymap :key ,key :def ,def . ,plist))
     (mapcar (lambda (it) (map-insert metadata :state (macroexp-quote it))) states))
    ;; (bind! "d" #'foo)
    (`(,(and (pred not-keyword-p) key) ,(and (pred not-keyword-p) def)
       . ,(and (pred plist-p) plist))
     `((:keymap-symbol 'global-map :keymap global-map :key ,key :def ,def . ,plist)))
    (_
     (error "cannot parse arguments..."))))
;;;; bind steps
;; This contains the logic of which steps how the macro should deal with the
;; data specified by metadata.  Before I had this logic in the build functions
;; themselves which each of them longer and more confusing.
(defun oo--bind-declare-function (metadata forms)
  (with-map-keywords! metadata
    (pcase !def-value
      (`(function ,(and function (pred symbolp)))
       `((declare-function ,function nil) ,@forms))
      (_
       forms))))

(defun! oo--bind-steps (metadata)
  "Return the list of steps for building the `bind!' form.
Each step is a function that accepts two arguments, metadata and forms, and
returns a list of forms."
  (with-map-keywords! metadata
    (cond ((or (and (integerp !char-value) (= !char-value ?g)) (member !state '(nil global)))
           (pushing! steps 'oo--bind-define-key))
          (!mode
           (pushing! steps 'oo--bind-evil-define-minor-mode-key))
          (t
           (pushing! steps 'oo--bind-evil-define-key)))
    (pushing! steps 'oo--bind-declare-function)
    (pushing! steps 'oo--bind-check-errors)
    (pushing! steps 'oo--bind-kbd)
    (when !wk
      (pushing! steps 'oo--bind-which-key))
    (pushing! steps 'oo--bind-let-binds)
    (unless (member !state '(nil global))
      (if (not (member !char-value '(nil ?g)))
          (pushing! steps 'oo--bind-defer-evil-state-char)
        (pushing! steps 'oo--bind-defer-evil-state)))
    (pushing! steps 'oo--bind-defer-keymap)
    (nreverse steps)))
;;;; oo--bind-body
(defun! oo--bind-body (args)
  "Return the body of `bind!'."
  (dolist (metadatum (oo--bind-metadata args))
    (setq metadatum (oo--let-binds metadatum))
    (set! steps (oo--bind-steps metadatum))
    (appending! body (cl-reduce (lambda (acc it) (funcall it metadatum acc)) steps :initial-value nil)))
  (nreverse body))
;;;; bind!
(defmacro! bind! (&rest args)
  "Bind keys as specified by ARGS.
STATE can be either a symbol representing an evil state, a letter (symbol)
representing the first letter of an evil state, or a list of either symbols xor
letters.  KEYMAP is a keymap symbol.  KEY and DEF are forms that evaluate to a
key and key-definition respectively.
Below are the possibilities for ARGS.

(bind! KEY DEF)

(bind! STATE KEY DEF)

(bind! KEYMAP KEY DEF)

(bind! STATE KEYMAP KEY DEF)

(bind! KEYMAP STATE KEY DEF)

After the arguments there is a plist of key-value pairs possible.

:wk DESCRIPTION

The :wk keyword signals adds the provided description for `which-key'.

:mode MODE

The :mode description signals the use of `evil-define-minor-mode-key' and uses
that to bind the key instead of `evil-define-key*'.  It should be used in
combination with STATE.

\(fn [STATE] [KEYMAP] KEY DEF . [PLIST])"
  (macroexp-progn (oo--bind-body args)))
;;; provide
(provide 'base-macros-bind)
;;; base-macros-bind.el ends here
