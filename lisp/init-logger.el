;;; init-logger.el --- Initialize logger -*- lexical-binding: t; -*-
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
;; Initialize logger.
;;
;;; Code:
(defvar oo-logger (lgr-get-logger "main")
  "Object used for logging.")

(defvar oo-error-logger (lgr-get-logger "error")
  "Object used for logging errors.")

;; Define a formatter.
(set! ts "%Y-%m-%d %H:%M:%S")
(set! format "%t [%L] %m")
(set! formatter (lgr-layout-format :format format :timestamp-format ts))
(set! message-format "[%L] %m")
(set! message-formatter (lgr-layout-format :format message-format))
;; Define the appenders.
(set! log-buffer-appender (lgr-appender-buffer :buffer (get-buffer-create "*log*")))
(set! message-buffer-appender (lgr-appender-buffer :buffer (get-buffer "*Messages*")))
;; Add the formatter to the appenders.
(lgr-set-layout log-buffer-appender formatter)
(lgr-set-layout message-buffer-appender message-formatter)
;; Add the appenders to the logger.
(lgr-add-appender oo-logger log-buffer-appender)
(lgr-add-appender oo-error-logger message-buffer-appender)
(lgr-add-appender oo-error-logger log-buffer-appender)

(when
    (>=
     (lgr-get-threshold oo-logger)
     lgr-level-info)
  (lgr-log oo-logger lgr-level-info "foo"))
;; I do not want to have to pass in the logger every single time.
(defmacro info! (msg &rest meta)
  `(lgr-info oo-logger ,msg ,@meta))

(defmacro error! (msg &rest meta)
  `(lgr-error oo-error-logger ,msg ,@meta))

(defmacro warn! (msg &rest meta)
  `(lgr-warn oo-logger ,msg ,@meta))

(defmacro fatal! (msg &rest meta)
  `(lgr-fatal oo-logger ,msg ,@meta))

(defmacro trace! (msg &rest meta)
  `(lgr-trace oo-logger ,msg ,@meta))

(defmacro debug! (msg &rest meta)
  `(lgr-debug oo-logger ,msg ,@meta))
;;; provide
(provide 'init-logger)
;;; init-logger.el ends here
