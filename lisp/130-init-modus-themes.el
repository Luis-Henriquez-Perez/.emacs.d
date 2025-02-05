;;; 130-init-modus-themes.el --- Initialize modus-themes -*- lexical-binding: t; -*-
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
;; Initialize modus-themes.
;;
;;; Code:
;; Without this the compiler will not know how to expand `modus-themes-with-colors'.
(eval-when-compile (require 'modus-themes))

(defun oo-apply-custom-faces-for-modus-themes-h (theme)
  "Add custom faces for `modus-themes'.
This hook is meant to be added to `enabled-theme-functions'."
  (when (bound-and-true-p modus-themes-items)
    (when (and (not (assoc theme oo-custom-faces-alist)) (member theme modus-themes-items))
      ;; (info! "Specifying custom faces for theme `%s'" theme)
      (modus-themes-with-colors
        (oo-custom-set-faces
          theme
          `(+evil-normal-state-face   ((t (:background ,magenta-cooler :foreground ,bg-main))))
          `(+evil-insert-state-face   ((t (:background ,green          :foreground ,bg-main))))
          `(+evil-visual-state-face   ((t (:background ,magenta        :foreground ,bg-main))))
          `(+evil-replace-state-face  ((t (:background ,red            :foreground ,bg-main))))
          `(+evil-motion-state-face   ((t (:background ,yellow         :foreground ,bg-main))))
          `(+evil-operator-state-face ((t (:background ,yellow         :foreground ,bg-main))))
          `(+evil-emacs-state-face    ((t (:background ,blue           :foreground ,bg-main)))))))
    (unless (cl-set-difference modus-themes-items (mapcar #'car oo-custom-faces-alist))
      ;; (info! "Done setting specific modus-themes faces.")
      (remove-hook 'enable-theme-functions #'oo-apply-custom-faces-for-modus-themes-h))))
;;; Set custom faces
(hook! enable-theme-functions oo-apply-custom-faces-for-modus-themes-h)
;;; provide
(provide '130-init-modus-themes)
;;; 130-init-modus-themes.el ends here
