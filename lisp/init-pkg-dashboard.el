;;; init-pkg-dashboard.el --- initialize dashboard -*- lexical-binding: t; -*-
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
;; Initialize dashboard.
;;
;;; Code:
(require 'init-core)

(declare-function dashboard-insert-startupify-lists "dashboard")
(declare-function dashboard-initialize "dashboard")
(declare-function dashboard-resize-on-hook "dashboard")

(defun o-dashboard-init-info (&rest _)
  (format "Emacs started in %.2f seconds" (string-to-number (emacs-init-time))))

(o-opt dashboard-items nil)
(o-opt dashboard-init-info #'o-dashboard-init-info)
(o-opt dashboard-banner-logo-title "Welcome!")
(o-opt dashboard-startupify-list (cl-set-difference dashboard-startupify-list '(dashboard-insert-items dashboard-insert-footer)))
(o-opt dashboard-startup-banner (seq-random-elt (if (display-graphic-p) '(official logo) '(1 2 3))))
(o-opt dashboard-center-content t)

(defun o-hook--init-dashboard ()
  (require 'dashboard)
  (add-hook 'window-size-change-functions #'dashboard-resize-on-hook)
  (add-hook 'window-setup-hook #'dashboard-resize-on-hook)
  (dashboard-insert-startupify-lists)
  (dashboard-initialize))

(add-hook 'emacs-startup-hook #'o-hook--init-dashboard)
;;; provide
(provide 'init-pkg-dashboard)
;;; init-pkg-dashboard.el ends here
