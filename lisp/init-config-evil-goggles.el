;;; init-config-evil-goggles.el --- Configure evil-goggles -*- lexical-binding: t; -*-
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
(require 'init-core)
(require 'evil-goggles)
;;;; Remove advices
(o-opt evil-goggles-duration 0.1)

;; Now that `evil-goggles' is loaded we do not need the advice.
(advice-remove 'evil-delete                                #'o-advice--load-evil-goggles)
(advice-remove 'evil-delete-line                           #'o-advice--load-evil-goggles)
(advice-remove 'evil-org-delete                            #'o-advice--load-evil-goggles)
(advice-remove 'evil-yank                                  #'o-advice--load-evil-goggles)
(advice-remove 'evil-yank-line                             #'o-advice--load-evil-goggles)
(advice-remove 'evil-change                                #'o-advice--load-evil-goggles)
(advice-remove 'evil-change-line                           #'o-advice--load-evil-goggles)
(advice-remove 'evil-change-whole-line                     #'o-advice--load-evil-goggles)
(advice-remove 'evil-indent                                #'o-advice--load-evil-goggles)
(advice-remove 'evil-join                                  #'o-advice--load-evil-goggles)
(advice-remove 'evil-join-whitespace                       #'o-advice--load-evil-goggles)
(advice-remove 'evil-fill-and-move                         #'o-advice--load-evil-goggles)
(advice-remove 'evil-shift-left                            #'o-advice--load-evil-goggles)
(advice-remove 'evil-shift-right                           #'o-advice--load-evil-goggles)
(advice-remove 'evil-org                                   #'o-advice--load-evil-goggles)
(advice-remove 'evil-org                                   #'o-advice--load-evil-goggles)
(advice-remove 'evil-surround-region                       #'o-advice--load-evil-goggles)
(advice-remove 'evil-commentary                            #'o-advice--load-evil-goggles)
(advice-remove 'evilnc-comment-operator                    #'o-advice--load-evil-goggles)
(advice-remove 'evil-replace-with-register                 #'o-advice--load-evil-goggles)
(advice-remove 'evil-set-marker                            #'o-advice--load-evil-goggles)
(advice-remove 'evil-record-macro                          #'o-advice--load-evil-goggles)
(advice-remove 'evil-paste-before                          #'o-advice--load-evil-goggles)
(advice-remove 'evil-paste-after                           #'o-advice--load-evil-goggles)
(advice-remove 'lispyville-yank                            #'o-advice--load-evil-goggles)
(advice-remove 'lispyville-delete                          #'o-advice--load-evil-goggles)
(advice-remove 'lispyville-change                          #'o-advice--load-evil-goggles)
(advice-remove 'lispyville-yank-line                       #'o-advice--load-evil-goggles)
(advice-remove 'lispyville-delete-line                     #'o-advice--load-evil-goggles)
(advice-remove 'lispyville-change-line                     #'o-advice--load-evil-goggles)
(advice-remove 'lispyville-delete-char-or-splice           #'o-advice--load-evil-goggles)
(advice-remove 'lispyville-delete-char-or-splice-backwards #'o-advice--load-evil-goggles)
(advice-remove 'lispyville-substitute                      #'o-advice--load-evil-goggles)
(advice-remove 'lispyville-change-whole-line               #'o-advice--load-evil-goggles)
(advice-remove 'lispyville-join                            #'o-advice--load-evil-goggles)
(advice-remove 'o-evil-eval-operator                        #'o-advice--load-evil-goggles)
(advice-remove 'o-evil-eval-replace-operator                #'o-advice--load-evil-goggles)
(advice-remove 'o-evil-eval-print-operator                  #'o-advice--load-evil-goggles)

(o-autolet nil
  (o-set list '((o-evil-eval-operator evil-change)
               (o-evil-eval-replace-operator evil-change)
               (o-evil-eval-print-operator evil-change)
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
  (pcase-dolist (`(,new ,old) list)
    (o-set elt (cons new (cdr (assoc old evil-goggles--commands))))
    (cl-pushnew elt evil-goggles--commands :key #'car))
  (cl-assert (cl-every (lambda (it) (assoc it list)) (mapcar #'car list))))
;;;; register lispyville commands
;;; provide
(provide 'init-config-evil-goggles)
;;; init-config-evil-goggles.el ends here
