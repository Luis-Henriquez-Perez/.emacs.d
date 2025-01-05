;;; init-custom.el --- initialize custom -*- lexical-binding: t; -*-
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
;; Initialize custom.
;;
;;; Code:
(require 'base)
;;;; don't ask me for permission to enable a theme
;; By default Emacs will ask you whether you are sure you want to enable a theme
;; as a precaution because a theme could contain malicious code.  Downloading
;; themes with elpaca is safe.  I don't make a habit of grabbing random themes
;; from wierd places online and evaluating them.  So I don't need.
(setq custom-safe-themes t)
;;;; don't create a custom file
;; I don't need it.  I'll be honest; to me it seems like the emacs's custom
;; interface is intended for people that don't know elisp.  For me it's completely
;; unnecessary.  Every variable I customize is in my emacs configuration.
(setq custom-file null-device)
;;;; disable old themes before enabling new ones
;; We end up with remants of the faces of old themes when we load a new
;; one.  For this reason, I make sure to disable any enabled themes before applying
;; a new theme.

;; When you load a theme you'll end up with quite a surprise.  And it
;; stacks as well when working on a big configuration change I didn't
;; have this code and I could literally backtrack the themes.

;; Don't know why deleting the previous theme before enabling a new
;; one isn't the default behavior.  When would anyone want to layer
;; the colors of one theme on top of an older one.
(defun! oo--disable-old-themes (orig-fn &rest args)
  "Disable old themes before loading new ones."
  (mapc #'disable-theme custom-enabled-themes)
  (apply orig-fn args))

(advice-add 'load-theme :around #'oo--disable-old-themes)
;;;; bindings
(bind! oo-toggle-map "r" #'oo-load-random-theme)
(bind! oo-toggle-map "t" #'load-theme)
;;;; make setting faces actually work
;; Surprisingly, the function `custom-theme-set-faces' and `custom-set-faces' do
;; not by default actually change any faces.  For that to happen the variable
;; `custom--inhibit-theme-enable' needs to be nil.  Furthermore, because I
;; disable existing themes before enabling new ones even after customizing a
;; theme the customization does not persist.  This function addresses both of
;; these issues ensuring that as expected the faces are set immediately if the
;; theme is loaded and that these changes persist even after theme change.
(defvar oo-custom-faces-alist nil
  "An alist of faces to be applied.
Each element is of the form (theme . faces).  THEME is the customized theme and
FACES is the list of customized faces for THEME.")

(defun oo-custom-set-faces (theme &rest faces)
  "Customize THEME with FACES.
If THEME is already enabled, also applies
faces immediately."
  (declare (indent defun))
  (when (and after-init-time
             (or (equal theme 'user) (member theme custom-enabled-themes)))
    (let ((custom--inhibit-theme-enable nil))
      (apply #'custom-theme-set-faces theme faces)))
  (setf (alist-get theme oo-custom-faces-alist)
        (cl-union faces (alist-get theme oo-custom-faces-alist) :key #'car)))

(defun oo-apply-custom-faces-h (current)
  "Apply any faces that need to be applied from `oo-custom-faces-alist'."
  (info! "Current theme -> %s" current)
  (for! ((theme . faces) oo-custom-faces-alist)
    (when (or (equal theme 'user) (member theme custom-enabled-themes))
      (info! "Applying faces for %s..." theme)
      (let ((custom--inhibit-theme-enable nil))
        (apply #'custom-theme-set-faces theme faces)))))

(add-hook 'enable-theme-functions #'oo-apply-custom-faces-h)
;;; provide
(provide 'init-custom)
;;; init-custom.el ends here
