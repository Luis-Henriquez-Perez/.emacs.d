;;; init-company.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; TODO: add commentary
;;
;;; Code:
(o-opt company-minimum-prefix-length 1)
(o-opt company-dabbrev-minimum-length 1)

(defun o-hook--init-company ()
  "Initialize company for text-mode."
  (setq-local company-frontends '(company-preview-if-just-one-frontend))
  (setq-local company-backends '(company-dabbrev))
  (setq-local company-dabbrev-downcase t)
  (setq-local company-dabbrev-ignore-case t)
  ;; Search other buffers with the same major-mode.
  (setq-local company-dabbrev-other-buffers t)
  (company-mode 1))

(add-hook 'text-mode-hook #'o-hook--init-company)

(add-hook 'prog-mode-hook #'o-hook--init-company)
;;; provide
(provide 'init-company)
;;; init-company.el ends here
