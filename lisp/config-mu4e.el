;;; config-mu4e.el --- Configure mu4e -*- lexical-binding: t; -*-
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
;; Configure mu4e.
;;
;;; Code:
(defmacro only-once! (&rest forms)
  "Evaluate FORMS only once."
  `(funcall ',(eval `(let (evaluated-p)
                       (lambda () (unless evaluated-p
                                    (setq evaluated-p t)
                                    ,@forms)))
                    t)))

(defun +mu4e--main-enter-message ()
  ;; (only-once! (call-process "mu --init %s"))
  (info! "Entering main account."))

(defun +mu4e--main-leave-message ()
  "Log when `luis@luishp.xyz' email account is left."
  (info! "Exiting main account."))

(defun +mu4e--legacy-enter-message ()
  ;; (only-once! (call-process "mu --init %s"))
  (info! "Entering main account."))

(defun +mu4e--legacy-leave-message ()
  "Log when `luishenriquezperez@gmail.com' is entered."
  (info! "Exiting main account."))

(defun! +mu4e-jump-to-maildir ()
  (interactive)
  (set! maildir (completing-read "Maildir: " (mu4e-get-maildirs)))
  (mu4e-headers-search (format "maildir:\"%s\"" maildir)))
;;; provide
(provide 'config-mu4e)
;;; config-mu4e.el ends here
