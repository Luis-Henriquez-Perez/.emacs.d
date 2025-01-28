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
;;;; loader
(defvar oo-init-data nil)

;; Sort init info by the amount of time taken.  Then put the longest times on
;; top.  Compute the total and place the percentages.
(defmacro require! (feature &optional path)
  "Catch any errors, record and log the time taken to require FEATURE."
  `(let ((start (current-time))
         (feature ',feature)
         (time nil))
     (condition-case err
         (progn
           (require ',feature ,path)
           (setq time (string-to-number (format "%.2f" (float-time (time-subtract (current-time) start)))))
           (message "Required '%s in %f seconds" feature time))
       (error
        (message "Error requiring '%s: %s" feature err)))
     (push (list feature time) oo-init-data)))

(defmacro init! (dir)
  (let (forms feature)
    (setq dir (expand-file-name dir user-emacs-directory))
    (dolist (path (directory-files dir t (rx bol (1+ nonl) ".el" eol)))
      (setq feature (intern (file-name-sans-extension (file-name-nondirectory (directory-file-name path)))))
      (push `(require! ,feature ,path) forms))
    `(progn ,@(nreverse forms))))
;;;; set load-path
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/base/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/init/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/config/" user-emacs-directory))
;; (require 'init-loader)
;; (require 'base)
(require! base-vars)
(require! base-settings)
(require! base-utils)
(require! base-macros)
(require! base-lib)
;; (init! "lisp/base/")
(init! "lisp/init/")
;;;; load requirements
(require! oo-keybindings)
(require! oo-autoloads)
(require! oo-init)

(defun! oo-startup-time-table ()
  (interactive)
  (require 'ctable)
  (set! total (apply #'+ (mapcar #'cl-second oo-init-data)))
  (flet! add-percentage (datum) (round (* 100 (oo-float-divide (cl-second datum) total))))
  (flet! add-init-per (datum) (round (* 100 (oo-float-divide (cl-second datum) (string-to-number (emacs-init-time "%.2f"))))))
  (for! ((feature time) oo-init-data)
    (collecting! data (list feature time (total-per time) (init-per time))))
  (setq oo-init-data (mapcar #'add-percentage oo-init-data))
  (setq oo-init-data (mapcar #'add-init-per oo-init-data))
  (setq oo-init-data (sort oo-init-data (lambda (o1 o2) (> (cl-second o1) (cl-second o2)))))
  (message "total -> %s" total)
  (let* ((column-model (list (make-ctbl:cmodel :title "Feature" :align 'left)
                             (make-ctbl:cmodel :title "Time (seconds)" :align 'center)
                             (make-ctbl:cmodel :title "Percent of Total" :align 'center)
                             (make-ctbl:cmodel :title "Percent of Init" :align 'center)))
         (data oo-init-data)
         (model (make-ctbl:model :column-model column-model :data data))
         (component (ctbl:create-table-component-buffer :model model)))
    (pop-to-buffer (ctbl:cp-get-buffer component))))
;;; provide init
(provide 'init)
;;; init.el ends here
