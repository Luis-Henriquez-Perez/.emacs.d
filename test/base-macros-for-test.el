;;; base-macros-for-test.el -*- lexical-binding: t; -*-
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
;; Test `base-macros-for'.
;;
;;; Code:
(require 'base-macros-for)

(ert-deftest for!---properly-loops-with-predicate-being-repeat-N ()
  (should (= 11 (let ((n 1)) (for! (repeat 10) (cl-incf n)) n))))

(ert-deftest for!---properly-loops-with-predicate-being-repeat-N ()
  (should (= 11 (let ((n 1)) (for! (repeat 10) (cl-incf n)) n))))

(ert-deftest for!---destructures-if-predicate-is-MATCH-FORM-LIST ()
  (should (equal '(3 9)
                 (let ((list '((1 2) (4 5)))
                       (result nil))
                   (for! ((a b) list)
                     (push (+ a b) result))
                   (reverse result)))))

(ert-deftest for!---properly-loops-with-predicate-being-VAR-SEQUENCE ()
  (should (equal '(4 3 2 1) (let (nums) (for! (n '(1 2 3 4)) (push n nums)) nums)))
  (should (equal '(4 3 2 1) (let (nums) (for! (n [1 2 3 4]) (push n nums)) nums)))
  (should (equal '(111 108 108 101 104) (let (chars) (for! (char "hello") (push char chars)) chars))))

(ert-deftest for!---properly-loops-with-predicate-being-VAR-INTEGER ()
  (should (equal '(0 1 2 3) (let (n) (for! (x 4) (collecting! n x)) n)))
  (should (= 11 (let ((n 1)) (for! (x 10) (cl-incf n)) n))))

(ert-deftest for!---propertly-loops-with-predicate-being-INTEGER ()
  (should (= 11 (let ((n 1)) (for! 10 (cl-incf n)) n))))
;;; provide
(provide 'base-macros-for-test)
;;; base-macros-for-test.el ends here
