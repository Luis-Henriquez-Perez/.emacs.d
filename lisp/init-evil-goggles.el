;;; init-evil-goggles.el --- Initialize evil-goggles -*- lexical-binding: t; -*-
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
;; Initialize evil-goggles.
;;
;;; Code:
(require 'base)
;;;; register evil commands
(declare-function evil-goggles-mode "evil-goggles")

;; Note that when `evil-goggles' is loaded in an advice and itself adds an
;; advice to the advised function.  Advising within an advice does not work at
;; least not until the next invocation of the advised function.  TLDR:
;; `evil-goggles' will not work first time one of the advised functions is
;; invoked--but it will every time afterwards.
(defun o-advice--load-evil-goggles (fn &rest args)
  (unless (or (minibufferp)
			  (bound-and-true-p evil-goggles-mode))
	(require 'evil-goggles)
	(evil-goggles-mode 1))
  (apply fn args))

(advice-add 'evil-delete								:around #'o-advice--load-evil-goggles)
(advice-add 'evil-delete-line                           :around #'o-advice--load-evil-goggles)
(advice-add 'evil-org-delete							:around #'o-advice--load-evil-goggles)
(advice-add 'evil-yank                                  :around #'o-advice--load-evil-goggles)
(advice-add 'evil-yank-line                             :around #'o-advice--load-evil-goggles)
(advice-add 'evil-change								:around #'o-advice--load-evil-goggles)
(advice-add 'evil-change-line                           :around #'o-advice--load-evil-goggles)
(advice-add 'evil-change-whole-line                     :around #'o-advice--load-evil-goggles)
(advice-add 'evil-indent								:around #'o-advice--load-evil-goggles)
(advice-add 'evil-join                                  :around #'o-advice--load-evil-goggles)
(advice-add 'evil-join-whitespace                       :around #'o-advice--load-evil-goggles)
(advice-add 'evil-fill-and-move                         :around #'o-advice--load-evil-goggles)
(advice-add 'evil-shift-left							:around #'o-advice--load-evil-goggles)
(advice-add 'evil-shift-right                           :around #'o-advice--load-evil-goggles)
(advice-add 'evil-org                                   :around #'o-advice--load-evil-goggles)
(advice-add 'evil-org                                   :around #'o-advice--load-evil-goggles)
(advice-add 'evil-surround-region                       :around #'o-advice--load-evil-goggles)
(advice-add 'evil-commentary							:around #'o-advice--load-evil-goggles)
(advice-add 'evilnc-comment-operator					:around #'o-advice--load-evil-goggles)
(advice-add 'evil-replace-with-register                 :around #'o-advice--load-evil-goggles)
(advice-add 'evil-set-marker							:around #'o-advice--load-evil-goggles)
(advice-add 'evil-record-macro                          :around #'o-advice--load-evil-goggles)
(advice-add 'evil-paste-before                          :around #'o-advice--load-evil-goggles)
(advice-add 'evil-paste-after                           :around #'o-advice--load-evil-goggles)
(advice-add 'lispyville-yank							:around #'o-advice--load-evil-goggles)
(advice-add 'lispyville-delete                          :around #'o-advice--load-evil-goggles)
(advice-add 'lispyville-change                          :around #'o-advice--load-evil-goggles)
(advice-add 'lispyville-yank-line                       :around #'o-advice--load-evil-goggles)
(advice-add 'lispyville-delete-line                     :around #'o-advice--load-evil-goggles)
(advice-add 'lispyville-change-line                     :around #'o-advice--load-evil-goggles)
(advice-add 'lispyville-delete-char-or-splice           :around #'o-advice--load-evil-goggles)
(advice-add 'lispyville-delete-char-or-splice-backwards :around #'o-advice--load-evil-goggles)
(advice-add 'lispyville-substitute                      :around #'o-advice--load-evil-goggles)
(advice-add 'lispyville-change-whole-line               :around #'o-advice--load-evil-goggles)
(advice-add 'lispyville-join							:around #'o-advice--load-evil-goggles)
(advice-add 'o-evil-eval-operator						:around #'o-advice--load-evil-goggles)
(advice-add 'o-evil-eval-replace-operator				:around #'o-advice--load-evil-goggles)
(advice-add 'o-evil-eval-print-operator                  :around #'o-advice--load-evil-goggles)

(o-require-after-load 'evil-goggles 'config-evil-goggles)
;;; provide
(provide 'init-evil-goggles)
;;; init-evil-goggles.el ends here
