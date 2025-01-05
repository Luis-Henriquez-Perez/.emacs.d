;;; base-macros-definers-test.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; Test `base-macro-definers'.
;;
;;; Code:
(require 'base-macros-definers)

(ert-deftest oo--arglist-symbols ()
  (should (equal '(a b c) (oo--arglist-symbols '(a b &rest c)))))

(ert-deftest oo--definer-components ()
  (should (equal '(bar (a) ("foo") ((+ 1 1)))
                 (oo--definer-components '(bar (a) "foo" (+ 1 1)))))
  (should (equal '(bar (a) ("foo" (declare (indent defun))) ((+ 1 1)))
                 (oo--definer-components '(bar (a) "foo" (declare (indent defun)) (+ 1 1)))))
  (should (equal '(bar (a) ("foo" (interactive)) ((+ 1 1)))
                 (oo--definer-components '(bar (a) "foo" (interactive) (+ 1 1)))))
  (should (equal '(bar (a) ("foo" (declare) (interactive)) ((+ 1 1)))
                 (oo--definer-components '(bar (a) "foo" (declare) (interactive) (+ 1 1))))))

(ert-deftest oo--defun ()
  (should-not (macroexpand-1 '(defun! oo--suppress-woman-warning (orig-fn &rest args)
                                (pcase args
                                  (`(woman-topic-history Man-topic-history . ,_)
                                   (advice-remove 'defvaralias #'oo--suppress-woman-warning))
                                  (_
                                   (apply orig-fn args))))))
  (should (equal '(defun foo (a) "foo" (autolet! (exclude! a) (+ 1 1)))
                 (macroexpand-1 '(defun! foo (a) "foo" (+ 1 1))))))
;;; provide
(provide 'base-macros-definers-test)
;;; base-macros-definers-test.el ends here
