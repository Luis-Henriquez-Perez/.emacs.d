;;; 990-config-evil-goggles.el --- Configure evil-goggles -*- lexical-binding: t; -*-
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
;; Configure evil-goggles.
;;
;;; Code:
(require! "^0[01]")
(require 'evil-goggles)
;;;; Remove advices
(setopt evil-goggles-duration 0.1)

;; Now that `evil-goggles' is loaded we do not need the advice.
(advice-remove 'evil-delete                                #'oo-require-evil-goggles-a)
(advice-remove 'evil-delete-line                           #'oo-require-evil-goggles-a)
(advice-remove 'evil-org-delete                            #'oo-require-evil-goggles-a)
(advice-remove 'evil-yank                                  #'oo-require-evil-goggles-a)
(advice-remove 'evil-yank-line                             #'oo-require-evil-goggles-a)
(advice-remove 'evil-change                                #'oo-require-evil-goggles-a)
(advice-remove 'evil-change-line                           #'oo-require-evil-goggles-a)
(advice-remove 'evil-change-whole-line                     #'oo-require-evil-goggles-a)
(advice-remove 'evil-indent                                #'oo-require-evil-goggles-a)
(advice-remove 'evil-join                                  #'oo-require-evil-goggles-a)
(advice-remove 'evil-join-whitespace                       #'oo-require-evil-goggles-a)
(advice-remove 'evil-fill-and-move                         #'oo-require-evil-goggles-a)
(advice-remove 'evil-shift-left                            #'oo-require-evil-goggles-a)
(advice-remove 'evil-shift-right                           #'oo-require-evil-goggles-a)
(advice-remove 'evil-org                                   #'oo-require-evil-goggles-a)
(advice-remove 'evil-org                                   #'oo-require-evil-goggles-a)
(advice-remove 'evil-surround-region                       #'oo-require-evil-goggles-a)
(advice-remove 'evil-commentary                            #'oo-require-evil-goggles-a)
(advice-remove 'evilnc-comment-operator                    #'oo-require-evil-goggles-a)
(advice-remove 'evil-replace-with-register                 #'oo-require-evil-goggles-a)
(advice-remove 'evil-set-marker                            #'oo-require-evil-goggles-a)
(advice-remove 'evil-record-macro                          #'oo-require-evil-goggles-a)
(advice-remove 'evil-paste-before                          #'oo-require-evil-goggles-a)
(advice-remove 'evil-paste-after                           #'oo-require-evil-goggles-a)
(advice-remove 'lispyville-yank                            #'oo-require-evil-goggles-a)
(advice-remove 'lispyville-delete                          #'oo-require-evil-goggles-a)
(advice-remove 'lispyville-change                          #'oo-require-evil-goggles-a)
(advice-remove 'lispyville-yank-line                       #'oo-require-evil-goggles-a)
(advice-remove 'lispyville-delete-line                     #'oo-require-evil-goggles-a)
(advice-remove 'lispyville-change-line                     #'oo-require-evil-goggles-a)
(advice-remove 'lispyville-delete-char-or-splice           #'oo-require-evil-goggles-a)
(advice-remove 'lispyville-delete-char-or-splice-backwards #'oo-require-evil-goggles-a)
(advice-remove 'lispyville-substitute                      #'oo-require-evil-goggles-a)
(advice-remove 'lispyville-change-whole-line               #'oo-require-evil-goggles-a)
(advice-remove 'lispyville-join                            #'oo-require-evil-goggles-a)
(advice-remove '+evil-eval-operator                        #'oo-require-evil-goggles-a)
(advice-remove '+evil-eval-replace-operator                #'oo-require-evil-goggles-a)
(advice-remove '+evil-eval-print-operator                  #'oo-require-evil-goggles-a)

(autolet!
 (set! list '((+evil-eval-operator evil-change)
              (+evil-eval-replace-operator evil-change)
              (+evil-eval-print-operator evil-change)
              (lispyville-delete-line evil-delete-line)
			  (lispyville-yank-line evil-yank-line)
			  (lispyville-change-line evil-change-line)
			  (lispyville-delete-char-or-splice evil-delete-char)
			  (lispyville-delete-char-or-splice-backwards evil-delete-backward-char)
			  (lispyville-substitute evil-substitute)
			  (lispyville-change-whole-line evil-change-whole-line)
			  (lispyville-join evil-join)
			  (lispyville-change evil-change)
			  (lispyville-delete evil-delete)
			  (lispyville-yank evil-yank)))
 (for! ((new old) list)
   (set! elt (cons new (cdr (assoc old evil-goggles--commands))))
   (cl-pushnew elt evil-goggles--commands :key #'car))
 (cl-assert (cl-every (lambda (it) (assoc it list)) (mapcar #'car list))))
;;;; register lispyville commands
;;; provide
(provide '990-config-evil-goggles)
;;; 990-config-evil-goggles.el ends here
