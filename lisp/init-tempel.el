;;; init-tempel.el --- initialize tempel -*- lexical-binding: t; -*-
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
;; Initialize tempel.
;;
;;; Code:
;;;; requirements
(require 'base)
;;;; autoloads
(autoload #'tempel-complete "tempel" nil t 'function)
(autoload #'tempel-insert "tempel" nil t 'function)
(autoload #'tempel-expand "tempel" nil t 'function)
;;;; capf
;; `tempel-expand' only triggers on exact matches. Alternatively use
;; `tempel-complete' if you want to see all matches, but then you
;; should also configure `tempel-trigger-prefix', such that Tempel
;; does not trigger too often when you don't expect it. NOTE: We add
;; `tempel-expand' *before* the main programming mode Capf, such
;; that it will be tried first.
(defhook! oo-setup-tempel-completion-h (prog-mode-hook text-mode-hook)
  "Add the Tempel Capf to `completion-at-point-functions'."
  (pushing! completion-at-point-functions #'tempel-expand :setter setq-local))
;;;; keybindings
(bind! i tempel-map "C-j" #'tempel-next)
(bind! i tempel-map "C-k" #'tempel-previous)
(bind! i tempel-map "TAB" #'tempel-next)
(bind! i tempel-map [backtab] #'tempel-previous)

(bind! oo-quick-map "i" #'tempel-insert)
(bind! oo-quick-map "l" #'tempel-insert)
;;; provide
(provide 'init-tempel)
;;; init-tempel.el ends here
