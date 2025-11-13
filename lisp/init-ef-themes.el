;;; init-ef-themes.el --- Initialize ef-themes -*- lexical-binding: t; -*-
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
;; Initialize ef-themes.
;;
;;; Code:
(defun +ef-themes-setup-custom-faces-h (theme)
  (when (and (bound-and-true-p ef-themes-items) (member theme ef-themes-items))
    (ef-themes-with-colors
      (oo-custom-set-faces
        theme
        ;; Evil state faces
        `(spaceline-evil-normal   ((t (:background ,magenta-cooler :foreground ,bg-main))))
        `(spaceline-evil-insert   ((t (:background ,green          :foreground ,bg-main))))
        `(spaceline-evil-visual   ((t (:background ,magenta        :foreground ,bg-main))))
        `(spaceline-evil-replace  ((t (:background ,red            :foreground ,bg-main))))
        `(spaceline-evil-motion   ((t (:background ,yellow         :foreground ,bg-main))))
        `(spaceline-evil-operator ((t (:background ,yellow         :foreground ,bg-main))))
        `(spaceline-evil-emacs    ((t (:background ,blue           :foreground ,bg-main))))))))

(+ef-themes-setup-custom-faces-h 'ef-light)
;;; provide
(provide 'init-ef-themes)
;;; init-ef-themes.el ends here
