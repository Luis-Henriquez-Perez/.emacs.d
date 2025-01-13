;;; base-keybind-utils.el --- keybinding functions -*- lexical-binding: t; -*-
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
;; Provide a function for binding keys.
;;
;; 1. Record which keys have been bound and I can optionally display them.
;; 2. Let me undo binding keys.
;; 3. Provide a macro on top of it with flexible syntax.
;; 4. Defer keybindings appropriately.
;; 5. Log the binding of keys.
;; The macro variant should declare the functions that are being bound so I do
;; not have to do that.
;; 6. Gracefully handle errors with binding keys.
;; 7. autoo
;; 8. some way to specify conditional bindings
;;
;; I have written a binding function in the past.  And I do not know I think a
;; function would be better than a macro.  I do not know at the very least a
;; function backend.  Problem with a macro is that it is harder to test.  And
;; it is not callable.
;;
;;; Code:
(defun oo-call-with-lef (binds fn)
  "Call function with functions."
  )

(defun oo--define-key ()
  ""
  (info! "")
  )

(defun oo--do-binding ()
  ""
  (oo-call-with-lef (())))

;; What data structure should I use to record bindings?

(defun oo-bind (keymap key def &key states)
  "Bind KEY to DEF in KEYMAP."
  (cond ((symbolp keymap)
         (info! "is not bound, deferring...")
         (oo-call-after-bound keymap #'apply #'oo-bind arguments))
        (states
         (oo--do-binding #'evil-define-key* states keymap key def)
         )))
;;; provide
(provide 'base-keybind-utils)
;;; base-keybind-utils.el ends here
