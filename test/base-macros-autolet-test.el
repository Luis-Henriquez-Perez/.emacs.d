;;; base-macros-block-autolet.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
(require 'base-macros-autolet)

;;;; helpers
(defun expand (form) (macroexpand-1 form))
(defun lets (form) (mapcar #'car (macroexpand-1 form)))
(defun letbind (sym form) (assoc sym (cl-second (macroexpand-1 form))))
(defun letbinds (form) (cl-second (macroexpand-1 form)))
(defun body (form) (cddr (cl-third (macroexpand-1 form))))
;;;; main
(ert-deftest autolet!---correctly-processes-keywords ()
  (should (equal '((a 10) (b nil)) (letbinds '(autolet! :init ((a 10)) (set! a 1) (set! b 1)))))
  (should (equal '((a 10)) (letbinds '(autolet! :init ((a 10)) (set! a 1)))))
  (should-not (letbinds '(autolet! :noinit (a b c) (set! a 1)))))

(ert-deftest autolet!---skips-loops-with-continue ()
  (autolet! (dotimes (n 3)
              (and (= 1 n) (continue!))
              (collecting! nums n))
            (should (equal nums '(0 2)))))

(ert-deftest autolet!---exits-body-when-return-is-invoked ()
  (should (= 2 (autolet! (when t (return! 2)) 3)))
  (should (= 2 (autolet! (dotimes (n 10) (return! 2)) 3)))
  (should (= 2 (autolet! (when t (return! 2)) 3))))

(ert-deftest autolet!---exits-loop-if-break! ()
  (should (= 2 (autolet! 2)))
  (should (= 2 (catch 'break! (break! 2) 4)))
  (should (= 2 (autolet! (dotimes (x 3) (when (= x 2) (break! x))))))
  (should (= 2 (autolet! (dotimes (n 10) (return! 2)) 3)))
  (should (= 5 (autolet! (dotimes (i 10) (when (= 5 i) (break! 5)))))))

(ert-deftest autolet!---handles-set-correctly ()
  (should (equal '((a nil) (b nil)) (letbinds '(autolet! (set! a 1) (set! b 2)))))
  (should (equal '((a nil)) (letbinds '(autolet! (set! a 1)))))
  (let ((bindings (letbinds '(autolet! (set! (a [b] [[c]] d) '(1 [2] [[3]] d))))))
    (should (and (member '(a nil) bindings)
                 (member '(b nil) bindings)
                 (member '(c nil) bindings)
                 (member '(d nil) bindings)))))

(ert-deftest autolet!---binds-symbol-specified-by-minning-to-most-positive-fixnum ()
  "Binds symbol specified by `minning!' to `most-positive-fixnum'"
  ;; (should-not (expand '(autolet! (maximizing! a 1))))
  (should (equal '((a most-negative-fixnum)) (letbinds '(autolet! (maximizing! a 1)))))
  (should (equal '((a most-positive-fixnum)) (letbinds '(autolet! (minimizing! a 1)))))
  (should (equal '((minimizing! a 1)) (body '(autolet! (minimizing! a 1)))))
  (should (equal '((a 0)) (letbinds '(autolet! (counting! a 1)))))
  (should (equal '((a nil)) (letbinds '(autolet! (collecting! a 1)))))
  (should (equal '((a nil)) (letbinds '(autolet! (appending! a 1)))))
  (should (equal '((a nil)) (letbinds '(autolet! (prepending! a 1))))))

;; (ert-deftest autolet!---ignores-quoted-forms ()
;;   (autolet! '(set! foo 1)))

(ert-deftest autolet!---stubbing-macros-work ()
  "wraps subsequent forms with lef!"
  (should (equal '((cl-flet ((plus (a b) (+ a (* 2 b)))) (plus 1)))
                 (body '(autolet! (stub! plus (a b) (+ a (* 2 b))) (plus 1)))))
  (should (equal '((lef! ((plus (a b) (+ a (* 2 b)))) (plus 1)))
                 (body '(autolet! (nflet! plus (a b) (+ a (* 2 b))) (plus 1)))))
  (should (= 10 (autolet! (stub! plus (a b) (+ a (* 2 b))) (plus 6 2))))
  (should (= 10 (autolet! (flet! plus #'+) (plus 5 5))))
  (should (= 10 (autolet! (nflet! + (a b) (funcall this-fn 1 (* a b)))
                          (+ 3 3))))
  (should (= 9 (autolet!
                (flet! four () 4)
                (flet! five () 5)
                (+ (four) (five))))))

;; (ert-deftest autolet!---gensym ()
;;   "wraps subsequent forms with lef!"
;;   (should (equal '(foo bar baz) (let-binds '((gensym! foo bar baz)))))
;;   (should (equal '((gensym! foo)) (body '((gensym! foo))))))
;;; provide
(provide 'base-macros-block-autolet)
;;; base-macros-block-autolet.el ends here
