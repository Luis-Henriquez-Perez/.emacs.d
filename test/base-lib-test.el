;;; base-lib-test.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; Tests for `base-lib'.
;;
;;; Code:
(require 'base-lib)
(require 'evil)

(ert-deftest oo--evil-char-to-state ()
  (set! evil-state-properties '((normal . _) (motion . _)))
  (should-not (oo--evil-char-to-state ?t))
  (should (equal 'motion (oo--evil-char-to-state ?m)))
  (should (equal 'normal (oo--evil-char-to-state ?n))))

;; (ert-deftest oo-call-after-load-functions ()
;;   (autolet!
;;     (set! foo 1)
;;     (set! load-order nil)
;;     (nflet! load-A (_) (push 'load-A load-order))
;;     (nflet! load-B (_) (push 'load-B load-order))
;;     (nflet! load-C (_) (push 'load-C load-order))
;;     (nflet! load-D () (push 'load-D load-order))
;;     (nflet! load-E () (push 'load-E load-order))
;;     (nflet! load-F () (push 'load-F load-order))
;;     (nflet! load-G () (push 'load-G load-order))
;;     (nflet! boundp (symbol)
;;       (if (member symbol '(foo evil-mode)) t (funcall this-fn symbol)))
;;     (set! evil-mode t)
;;     (set! oo-after-load-functions-alist (list (cons 'bar '(load-G load-F))
;;                                               (cons 'foo '(load-E load-D))
;;                                               (cons ?t '(load-C))
;;                                               (cons ?n '(load-B load-A))))
;;     (should (boundp 'foo))
;;     (oo-call-after-load-functions)
;;     (should (equal '(load-E load-D) load-order))
;;     (should (equal '((bar load-G load-F)
;;                      (116 load-C)
;;                      (110 load-B load-A))
;;                    oo-after-load-functions-alist))
;;     (set! evil-state-properties '((normal)))
;;     (oo-call-after-load-functions)
;;     (should (equal '((bar load-G load-F)
;;                      (116 load-C))
;;                    oo-after-load-functions-alist))))

;; (ert-deftest oo-call-after-bound ()
;;   (autolet!
;;     (set! called nil)
;;     (nflet! fn-A () (push 'fn-A called))
;;     (nflet! boundp (symbol)
;;       (if (equal symbol 'foo) t (funcall this-fn symbol)))
;;     (should (boundp 'foo))
;;     (oo-call-after-bound 'foo #'fn-A)
;;     (should-not oo-after-load-functions-alist)
;;     (should (equal '(fn-A) called))
;;     (oo-call-after-bound 'bar #'fn-B)
;;     (should (equal '(fn-A) called))
;;     (should (equal '((bar fn-B)) oo-after-load-functions-alist))))

;; (ert-deftest oo-call-after-evil-state-char ()
;;   (autolet!
;;     (set! oo-after-load-functions-alist nil)
;;     (set! called nil)
;;     (set! evil-mode t)
;;     (nflet! fn-A (state) (push `(fn-A ,state) called))
;;     (nflet! fn-B (state) (push `(fn-B ,state) called))
;;     (nflet! boundp (symbol)
;;       (if (equal symbol 'evil-mode) t (funcall this-fn symbol)))
;;     (set! evil-state-properties '((normal)))
;;     (should-not called)
;;     (oo-call-after-evil-state-char ?n #'fn-A)
;;     (should (equal '((fn-A normal)) called))
;;     (oo-call-after-evil-state-char ?t #'fn-B)
;;     (should (equal '((fn-A normal)) called))
;;     (should (equal '((116 fn-B)) oo-after-load-functions-alist))))
;;; provide
(provide 'base-lib-test)
;;; base-lib-test.el ends here
