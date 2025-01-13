;;; init-org-appear.el --- Initialize org-appear -*- lexical-binding: t; -*-
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
;; Initialize org-appear.
;;
;;; Code:
(opt! org-appear-autoemphasis t)
(opt! org-appear-autolink t)
(opt! org-appear-trigger 'manual)

(hook! org-mode-hook org-appear-mode)

;; (add-hook 'org-mode-hook (lambda ()
;;                            (add-hook 'evil-insert-state-entry-hook
;;                                      #'org-appear-manual-start
;;                                      nil
;;                                      t)
;;                            (add-hook 'evil-insert-state-exit-hook
;;                                      #'org-appear-manual-stop
;;                                      nil
;;                                      t)))
;;; provide
(provide 'init-org-appear)
;;; init-org-appear.el ends here
