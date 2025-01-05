;;; base-macros-lef-test.el -*- lexical-binding: t; -*-
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
;; Test for `base-macros-lef'.
;;
;;; Code:
(require 'base-macros-lef)

;; (defun oo--lef-)

;; (defun oo--lef-if ()
;;   `(if (fboundp ',sym)
;;        (oo--lef-anaphoric)
;;      (oo--lef-anaphoric)))

;; (mapcan #'oo--lef-bind bindings)
;; (defun oo--lef-fn ())

;; (defun oo--contains-this-fn-p ()
;;   (member 'this-fn (flatten-list body)))

;; (ert-deftest "can use a special syntax"
;;   (should (lef! ((#'foo #'always)) (foo)) :to-be t))
(ert-deftest lef!---can-bind-a-function-symbol-to-a-different-function ()
  "can bind a function symbol to a different function"
  (should (= 5 (lef! ((+ #'-)) (+ 10 5)))))

(ert-deftest lef!---can-bind-symbols-to-anonymous-functions ()
  "can bind symbols to anonymous functions"
  (should (= 4 (lef! ((foo (lambda () 4))) (foo)))))

(ert-deftest lef!---stores-original-function-in-the-symbol-this-fn ()
  (should (= 16 (lef! ((+ (lambda (&rest args) (1+ (apply this-fn args))))) (+ 10 5))))
  (should (= 10 (lef! ((+ (x y) (funcall this-fn (* x y) 1))) (+ 3 3)))));;; provide

(provide 'base-macros-lef-test)
;;; base-macros-lef-test.el ends here
