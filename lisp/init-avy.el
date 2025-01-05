;;; init-avy.el --- avy configuration -*- lexical-binding: t; -*-
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
;; This is my configuration for avy.
;;
;;; Code:
(require 'base)

(opt! avy-style 'pre)

(opt! avy-keys (eval-when-compile (string-to-list "jfkdlsaurieowncpqmxzb")))

(opt! avy-background nil)

(opt! avy-timeout-seconds 0.3)

;; Probably not the best binding, but I just want it to be bound to something
;; for now.
(bind! oo-find-map "z" #'ace-link)
;;; provide
(provide 'init-avy)
;;; init-avy.el ends here
