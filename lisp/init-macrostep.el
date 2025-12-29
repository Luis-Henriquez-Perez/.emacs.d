;;; init-macrostep.el --- Initialize macrostep -*- lexical-binding: t; -*-
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

;; TODO: add which-key name.
(keymap-set emacs-lisp-mode-map (concat o-key-localleader-emacs "\s" "m") nil)
(keymap-set emacs-lisp-mode-map (concat o-key-localleader-emacs "\s" "m e") #'macrostep-expand)
(keymap-set emacs-lisp-mode-map (concat o-key-localleader-emacs "\s" "m c") #'macrostep-collapse)
(keymap-set emacs-lisp-mode-map (concat o-key-localleader-emacs "\s" "m C") #'macrostep-collapse-all)
(keymap-set emacs-lisp-mode-map (concat o-key-localleader-emacs "\s" "m a") #'macrostep-collapse-all)
;;; provide
(provide 'init-macrostep)
;;; init-macrostep.el ends here
