;;; base-macros-hook-test.el --- Test `base-macros-hook' -*- lexical-binding: t; -*-
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
;; Test `base-macros-hook'.
;;
;;; Code:
(require 'base-macros-hook)

;; (ert-deftest hook! ()
;;   (autolet!
;;     (set! run-list nil)
;;     (defvar fake-hook nil)
;;     (should-not fake-hook)
;;     (nflet! a () (push 'a run-list))
;;     (nflet! c () (push 'c run-list))
;;     (nflet! b-error () (push 'b-error run-list) (/ 2 0))
;;     (hook! fake-hook&a)
;;     (hook! fake-hook&b-error)
;;     (hook! fake-hook&c)
;;     ;; All the hooks have been added.
;;     (should (equal fake-hook '(fake-hook&c fake-hook&b-error fake-hook&a)))
;;     (run-hooks 'fake-hook)
;;     ;; Even the hook with the error should be in the list.
;;     (should (equal run-list '(a b-error c)))))

(ert-deftest after! ()
  (autolet!
   (should-not (boundp 'oo-after-load-foo-hook))
   (set! some-list nil)
   (set! other-list nil)
   (after! do-something (foo) (push 1 other-list))
   (should (boundp 'oo-after-load-foo-hook))
   (should (fboundp 'oo-run-after-load-foo-hook))
   (should (equal oo-after-load-foo-hook '(oo-after-load-foo-hook&do-something)))
   (should-not other-list)
   (provide 'foo)
   ;; Adds 1 to other-list when `foo' is provided.
   (should (equal 1 (car other-list)))
   (after! do-thing (bar baz) (push 1 some-list))
   (provide 'bar)
   (should-not some-list)
   (provide 'baz)
   ;; Adds 1 to other-list when `foo' and `bar' are provided.
   (should (equal some-list '(1)))
   (provide 'baz)
   (provide 'baz)
   ;; Does not run hook more than once.
   (should (equal some-list '(1)))
   (after! do-another-thing (baz) (push 2 some-list))
   ;; Calls function immediately.
   (should (equal some-list '(2 1)))
   (provide 'baz)
   (should (equal some-list '(2 1)))))
;;; provide
(provide 'base-macros-hook-test)
;;; base-macros-hook-test.el ends here
