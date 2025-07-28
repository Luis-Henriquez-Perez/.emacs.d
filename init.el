;;; init.el --- My emacs configuration -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Free Software Foundation, Inc.
;;
;; Author: Luis Henriquez-Perez <luis@luishp.xyz>
;; Maintainer: Luis Henriquez-Perez <luis@luishp.xyz>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
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
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This is my personal emacs configuration.  Please refer to the
;; README for information on how to run and modify them.
;;
;;; Code:
;; https://medium.com/@danielorihuelarodriguez/optimize-emacs-start-up-time-ae314201e04f
;; https://news.ycombinator.com/item?id=39127859
;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.8)

;; Credits to irreal for sharing that keywords can be used as registers in his
;; blog post (https://irreal.org/blog/?p=12386).
(set-register :file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(push (expand-file-name "lisp/" user-emacs-directory) load-path)

(require '000-base-vars)
(require '001-init-log)
(eval-when-compile (require '002-init-loader))

;; Be more precise about startup.  What I will look at when I measure startup
;; is the time taken for my lisp files to load, the time taken to run
;; `after-init-hook', and the time taken to run `emacs-startup-hook'.  That is
;; everything I am responsible for when emacs is loaded.

(defvar oo-before-load-time (current-time))
(require! :profile t)
(defvar oo-after-load-time (current-time))
(defun oo-startup-info ()
  (interactive)
  (let ((a (float-time (time-subtract oo-before-load-time before-init-time)))
        (b (float-time (time-subtract oo-after-load-time oo-before-load-time)))
        (c (float-time (time-subtract after-init-time oo-after-load-time))))
    (message "before -> %0.2f | loading elapsed %0.2f | done %0.2f" a b c)))
;;; provide init
(provide 'init)
;;; init.el ends here
