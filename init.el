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
;;;; disable garbage collection until after startup
;; https://medium.com/@danielorihuelarodriguez/optimize-emacs-start-up-time-ae314201e04f
;; https://news.ycombinator.com/item?id=39127859
;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.8)
;;;; don't search for whenever a package is loaded
;; Credits to irreal for sharing that keywords can be used as registers in his
;; blog post (https://irreal.org/blog/?p=12386).
(set-register :file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
;;;; prevent flashing of unstyled modeline
;; Don't render the modeline on startup.  For one thing, the startup looks
;; better without flashing stuff on the screen.  Additionally, the more that's
;; saved on rendering, the faster the startup.
(set-register :mode-line-format mode-line-format)
(setq-default mode-line-format nil)
;;;; set load-path
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
;;;; require! - help me load files robustly
(defmacro require! (feature)
  "Require FEATURE, reporting errors and logging the time it takes to load."
  `(let ((start-time (current-time))
         (total-time nil)
         (success-p t))
     (condition-case err
         (require ',feature)
       (error
        (setq success-p nil)
        (message "Error loading %s: %s" ',feature (error-message-string err))))
     (setq total-time (* 1000 (float-time (time-subtract (current-time) start-time))))
     (message "Loaded %s in %.2fms" ',feature total-time)))

(defmacro init! ()
  "Initialize."
  (let ((body nil))
    (dolist ()
      (push `(require! ,feature) feature))
    (progn ,@(nreverse body))))
;;;; load requirements
(init!)
;; (require! oo-keybindings)
;; (require! oo-autoloads)
;; (require! oo-init)
;;; provide init
(provide 'init)
;;; init.el ends here
