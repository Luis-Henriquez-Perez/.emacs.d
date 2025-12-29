;;; init-type-break.el --- Initialize type-break -*- lexical-binding: t; -*-
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
(o-opt type-break-interval (* 0.26 60))  ;; 25 minutes in seconds
(o-opt type-break-good-rest-interval (* 5 60))  ;; 5 minutes in seconds

(o-opt type-break-warning-repeat 3)  ;; Repeat warnings 3 times
(o-opt type-break-time-warning-intervals '(60 30 10)) ;; Warnings at 60s, 30s, 10s before break
(o-opt type-break-query-mode t) ;; Force confirmation to skip breaks

(o-opt type-break-good-rest-interval nil) ;; 5 minutes break required
;;; provide
(provide 'init-type-break)
;;; init-type-break.el ends here
