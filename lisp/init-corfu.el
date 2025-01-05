;;; init-corfu.el --- initialize corfu -*- lexical-binding: t; -*-
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
;; Initialize corfu.
;;
;;; Code:
(require 'base)

;; TODO: make it so moving on a candidate if I press espace insert that candidate.
(opt! corfu-preview-current t)
(opt! corfu-preselect-first t)
(opt! corfu-quit-at-boundary nil)
(opt! corfu-auto t)
(opt! corfu-auto-delay 0.1)
(opt! corfu-auto-prefix 1)
(opt! corfu-bar-width 0)

(bind! i corfu-map "<tab>"   #'corfu-next)
(bind! i corfu-map [backtab] #'corfu-previous)
(bind! i corfu-map "S-TAB"   #'corfu-previous)
(bind! i corfu-map "C-;"     #'corfu-quick-complete)
(bind! i corfu-map "C-j"     #'corfu-next)
(bind! i corfu-map "C-k"     #'corfu-previous)
(bind! i corfu-map "C-p"     #'corfu-previous)
(bind! i corfu-map ";"       #'corfu-quick-complete)
(bind! i corfu-map "SPC"     #'corfu-insert)
;;; provide
(provide 'init-corfu)
;;; init-corfu.el ends here
