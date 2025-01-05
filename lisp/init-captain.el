;;; init-captain.el --- initialize captain-mode -*- lexical-binding: t; -*-
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
;; Initialize captain-mode.
;;
;;; Code:
(require 'base)

(hook! prog-mode-hook captain-mode)
(hook! text-mode-hook captain-mode)

(setq-hook! text-mode-hook captain-predicate #'always)
(setq-hook! text-mode-hook captain-sentence-start-function #'captain--default-sentence-start)

(declare-function +captain--prog-mode-sentence-start "config-captain")
(setq-hook! prog-mode-hook captain-predicate #'always)
(setq-hook! prog-mode-hook captain-sentence-start-function #'+captain--prog-mode-sentence-start)
;;; provide
(provide 'init-captain)
;;; init-captain.el ends here
