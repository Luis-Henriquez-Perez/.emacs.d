;;; 035-base-macros-test.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
(require '035-base-macros)
;;;; anaphora
(ert-deftest alet! ()
  (should (equal '(3 2) (alet! 1 (list (+ it 2) (+ it 1))))))

(ert-deftest aand! ()
  (should (equal 2 (aand! 1 (+ it 2) (+ it 1)))))

(ert-deftest aif! ()
  (should (equal '(3 2) (aif! 1 (list (+ it 2) (+ it 1))))))

(ert-deftest awhen! ()
  (should (equal '(3 2) (awhen! 1 (list (+ it 2) (+ it 1))))))

(ert-deftest aprog1! ()
  (should (equal 1 (let (a) (aprog1! 1 (push (+ it 2) a))))))

(ert-deftest each! ()
  (should (equal '(3 2) (alet! 1 (list (+ it 2) (+ it 1))))))

(ert-deftest collect! ()
  (should (equal '(2 3 4 5) (collect! '(1 2 3 4) (+ it 1)))))

(ert-deftest alet2! ()
  (should (= 3 (alet2! 1 2 (+ it other)))))

(ert-deftest as! ()
  (should (= 9 (as! 1 (+ 2 it) (* 3 it)))))
;;;; docollect!
(ert-deftest docollect! ()
  (should (e)))
;;;; lef!
(ert-deftest lef! ()
  ;; Can bind symbol to different function.
  (should (= 5 (lef! ((+ #'-)) (+ 10 5))))
  ;; Works with anonymous functions.
  (should (= 4 (lef! ((foo (lambda () 4))) (foo))))
  ;; Stores original function in symbol `this-fn'.
  (should (= 16 (lef! ((+ (lambda (&rest args) (1+ (apply this-fn args))))) (+ 10 5))))
  (should (= 10 (lef! ((+ (x y) (funcall this-fn (* x y) 1))) (+ 3 3)))))
;;;; quiet!
;; (ert-deftest quiet! ()
;;   (should ))
;;;; with-map!
(ert-deftest with-map! ()
  (should (= 3 (with-map! '((a . 1) (b . 2)) (and !!a !!b (+ !a !b)))))
  (should (= 3 (with-map! '(a 1 b 2) (and !!a !!b (+ !a !b))))))
;;;; opt!
;; (ert-deftest opt! ()
;;   ;; Sets variable if feature is loaded.
;;   )
;;;; destructive modification macros
(ert-deftest collecting! ()
  (should (equal '(1 2) (let (a) (collecting! a 1) a)))
  (should (equal '(1) (let (a) (collecting! a 1) a))))

(ert-deftest appending! ()
  (should (equal '(1 2) (let (a) (appending! a 1) a)))
  (should (equal '(1) (let (a) (appending! a 1) a))))

(ert-deftest prepending! ()
  (should (equal '(1 2) (let (a) (prepending! a 1) a)))
  (should (equal '(1) (let (a) (prepending! a 1) a))))
;;;; for!
(ert-deftest for! ()
  ;; properly-loops-with-predicate-being-repeat-N
  (should (= 11 (let ((n 1)) (for! (repeat 10) (cl-incf n)) n)))
  ;; destructures-if-predicate-is-MATCH-FORM-LIST
  (should (equal '(3 9) (let ((list '((1 2) (4 5))) (result nil)) (for! ((a b) list) (push (+ a b) result)) (reverse result)))))

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
(provide '035-base-macros-test)
;;; 035-base-macros-test.el ends here
