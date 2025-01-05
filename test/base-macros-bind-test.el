;;; base-macros-bind-test.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
(require 'base-macros-bind)

(ert-deftest oo--bind-steps ()
  (should (equal (oo--bind-steps '(:char-value 105 :state state :keymap global-map :key "d" :def #'foo))
                 '(oo--bind-evil-define-key
                   oo--bind-check-errors
                   oo--bind-kbd
                   oo--bind-let-binds
                   oo--bind-defer-evil-state-char
                   oo--bind-defer-keymap)))
  (should (equal (oo--bind-steps '(:state 'insert :keymap global-map :key "d" :def #'foo))
                 '(oo--bind-evil-define-key
                   oo--bind-check-errors
                   oo--bind-kbd
                   oo--bind-let-binds
                   oo--bind-defer-evil-state
                   oo--bind-defer-keymap)))
  (should (equal (oo--bind-steps '(:keymap global-map :key "d" :def #'foo))
                 '(oo--bind-define-key
                   oo--bind-check-errors
                   oo--bind-kbd
                   oo--bind-let-binds
                   oo--bind-defer-keymap))))

(ert-deftest oo--bind-metadata ()
  (lef! ((gensym (&optional prefix) (if (equal prefix "state-value") (intern prefix)
                                      (funcall this-fn prefix)))
         (make-symbol (name) (if (equal name "state-value") (intern name)
                               (funcall this-fn name))))
    ;; (bind! "d" #'foo)
    (should (equal (oo--bind-metadata '("d" #'foo))
                   '((:keymap global-map :key "d" :def #'foo))))
    ;; (bind! insert "d" #'foo)
    (should (equal (oo--bind-metadata '(insert "d" #'foo))
                   '((:state 'insert :keymap global-map :key "d" :def #'foo))))
    ;; (bind! i "d" #'foo)
    (should (equal (oo--bind-metadata '(i "d" #'foo))
                   '((:char-value 105 :state state-value :keymap global-map :key "d" :def #'foo))))
    ;; (bind! insert org-mode-map "d" #'foo)
    (should (equal (oo--bind-metadata '(insert org-mode-map "d" #'foo))
                   '((:state 'insert :keymap org-mode-map :key "d" :def #'foo))))
    ;; (bind! i org-mode-map "d" #'foo)
    (should (equal (oo--bind-metadata '(i org-mode-map "d" #'foo))
                   '((:char-value 105 :state state-value :keymap org-mode-map :key "d" :def #'foo))))
    ;; (bind! org-mode-map insert "d" #'foo)
    (should (equal (oo--bind-metadata '(org-mode-map insert "d" #'foo))
                   '((:state 'insert :keymap org-mode-map :key "d" :def #'foo))))
    ;; (bind! org-mode-map i "d" #'foo)
    (should (equal (oo--bind-metadata '(org-mode-map i "d" #'foo))
                   '((:char-value 105 :state state-value :keymap org-mode-map :key "d" :def #'foo))))
    ;; (bind! (normal insert visual) "d" #'foo)
    (should (equal (oo--bind-metadata '((normal insert visual) "d" #'foo))
                   '((:state 'normal :keymap global-map :key "d" :def #'foo)
                     (:state 'insert :keymap global-map :key "d" :def #'foo)
                     (:state 'visual :keymap global-map :key "d" :def #'foo))))
    ;; (bind! (n m v) "d" #'foo)
    (should (equal (oo--bind-metadata '((n m v) "d" #'foo))
                   '((:state state-value :char-value 110 :keymap global-map :key "d" :def #'foo)
                     (:state state-value :char-value 109 :keymap global-map :key "d" :def #'foo)
                     (:state state-value :char-value 118 :keymap global-map :key "d" :def
                             #'foo))))
    ;; (bind! org-mode-map (normal motion visual) "d" #'foo)
    (should (equal (oo--bind-metadata '(global-map (normal insert visual) "d" #'foo))
                   '((:state 'normal :keymap global-map :key "d" :def #'foo)
                     (:state 'insert :keymap global-map :key "d" :def #'foo)
                     (:state 'visual :keymap global-map :key "d" :def #'foo))))
    ;; (bind! org-mode-map (n m v) "d" #'foo)
    (should (equal (oo--bind-metadata '(global-map (n i v) "d" #'foo))
                   '((:state state-value :char-value 110 :keymap global-map :key "d" :def #'foo)
                     (:state state-value :char-value 105 :keymap global-map :key "d" :def #'foo)
                     (:state state-value :char-value 118 :keymap global-map :key "d" :def #'foo))))
    ;; (bind! (normal motion visual) org-mode-map "d" #'foo)
    (should (equal (oo--bind-metadata '((normal insert visual) global-map "d" #'foo))
                   '((:state 'normal :keymap global-map :key "d" :def #'foo)
                     (:state 'insert :keymap global-map :key "d" :def #'foo)
                     (:state 'visual :keymap global-map :key "d" :def #'foo))))
    ;; (bind! (n m v) org-mode-map "d" #'foo)
    (should (equal (oo--bind-metadata '((n m v) global-map "d" #'foo))
                   '((:state state-value :char-value 110 :keymap global-map :key "d" :def #'foo)
                     (:state state-value :char-value 109 :keymap global-map :key "d" :def #'foo)
                     (:state state-value :char-value 118 :keymap global-map :key "d" :def #'foo))))))

;; (should-not (oo--bind-metadata '(i oo-override-mode-map oo-insert-leader-key #'oo-leader-prefix-command)))

(ert-deftest oo--let-binds ()
  (lef! ((make-symbol (name) (intern name)))
    (should (equal (oo--let-binds '(:a 1 :b 2 :c 3 :d-value 4))
                   '((:d-value . 4)
                     (:c-value . 3)
                     (:c . c)
                     (:b-value . 2)
                     (:b . b)
                     (:a-value . 1)
                     (:a . a))))))
;;; provide
(provide 'base-macros-bind-test)
;;; base-macros-bind-test.el ends here
