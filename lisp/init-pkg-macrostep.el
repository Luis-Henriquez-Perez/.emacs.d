;;; init-pkg-macrostep.el --- Initialize macrostep -*- lexical-binding: t; -*-
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
(declare-function macrostep-expand "macrostep")
(declare-function macrostep-collapse-all "macrostep")
(declare-function macrostep-collapse "macrostep")

(o-declare-package 'macrostep)

(keymap-set emacs-lisp-mode-map (concat o-key-localleader-emacs "\s" "m") nil)
(keymap-set emacs-lisp-mode-map (concat o-key-localleader-emacs "\s" "m e") #'macrostep-expand)
(keymap-set emacs-lisp-mode-map (concat o-key-localleader-emacs "\s" "m c") #'macrostep-collapse)
(keymap-set emacs-lisp-mode-map (concat o-key-localleader-emacs "\s" "m C") #'macrostep-collapse-all)
(keymap-set emacs-lisp-mode-map (concat o-key-localleader-emacs "\s" "m a") #'macrostep-collapse-all)

(o-defafter o-after--evil-define-macrostep-binds (evil)
  (let ((prefixes (list o-key-localleader-normal o-key-localleader-normal-alt)))
    (dolist (prefix prefixes)
      (evil-define-key* 'normal emacs-lisp-mode-map (kbd (concat prefix "\s" "m")) nil)
      (evil-define-key* 'normal emacs-lisp-mode-map (kbd (concat prefix "\s" "m e")) #'macrostep-expand)
      (evil-define-key* 'normal emacs-lisp-mode-map (kbd (concat prefix "\s" "m c")) #'macrostep-collapse)
      (evil-define-key* 'normal emacs-lisp-mode-map (kbd (concat prefix "\s" "m C")) #'macrostep-collapse-all)
      (evil-define-key* 'normal emacs-lisp-mode-map (kbd (concat prefix "\s" "m a")) #'macrostep-collapse-all))))

(o-defafter o-after--bray-define-macrostep-binds (init-pkg-bray)
  (let ((prefixes (list o-key-localleader-normal o-key-localleader-normal-alt)))
    (dolist (prefix prefixes)
      (bray-state-map-set 'normal emacs-lisp-mode-map (concat prefix "\s" "m") nil)
      (bray-state-map-set 'normal emacs-lisp-mode-map (concat prefix "\s" "m e") #'macrostep-expand)
      (bray-state-map-set 'normal emacs-lisp-mode-map (concat prefix "\s" "m c") #'macrostep-collapse)
      (bray-state-map-set 'normal emacs-lisp-mode-map (concat prefix "\s" "m C") #'macrostep-collapse-all)
      (bray-state-map-set 'normal emacs-lisp-mode-map (concat prefix "\s" "m a") #'macrostep-collapse-all))))
;;; provide
(provide 'init-pkg-macrostep)
;;; init-pkg-macrostep.el ends here
