;;; init-fancy-priorities.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
(hook! org-mode-hook org-fancy-priorities-mode)

(opt! org-fancy-priorities-list `((?A . ,(nerd-icons-faicon "nf-fa-fire"))
                                  (?B . ,(nerd-icons-mdicon "nf-md-alarm_light"))
                                  (?C . ,(nerd-icons-faicon "nf-fa-star"))
                                  (?D . ,(nerd-icons-octicon "nf-oct-pin"))
                                  (?E . ,(nerd-icons-mdicon "nf-md-sleep"))
                                  ;; (?F . ,(nerd-icons-mdicon "nf-md-exclamation_thick"))
                                  ))
;;; provide
(provide 'init-fancy-priorities)
;;; init-fancy-priorities.el ends here
