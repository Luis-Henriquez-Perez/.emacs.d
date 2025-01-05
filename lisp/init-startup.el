;;; init-startup.el --- initialize startup -*- lexical-binding: t; -*-
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
;; Initialize startup.
;;
;;; Code:
;;;; don't show the startup screen
;; By default Emacs displays [[][this startup screen]] at startup.  No thanks!  I
;; think these variables are all aliases for eachother.
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)
;;;; stop creating =auto-save-list= directory
;; See [[https://emacs.stackexchange.com/questions/18677/prevent-auto-save-list-directory-to-be-created][#18677]].
(setq auto-save-list-file-prefix nil)
;;;; set the initial major mode to =fundamental-mode=
;; This improve startup time because packages enabled for emacs-lisp-mode are not
;; loaded immediately.
(setq initial-major-mode 'fundamental-mode)
;;;; disable initial scratch message
;; Don't display any documentation--or any message at all--in the =*scratch*=
;; buffer.  Emacs by default displays a message in the scratch buffer.
(setq initial-scratch-message nil)
;;;; don't display message advertising gnu system
;; They made the process of disabling this more difficult.
(advice-add #'display-startup-echo-area-message :around #'ignore)
;;;;; garbage collection
(defun! oo--timer--lower-garbage-collection ()
  "Lower garbage collection until it reaches default values."
  (flet! mb (x) (/ (float x) 1024 1024))
  (if (minibuffer-window-active-p (minibuffer-window))
      (run-with-timer 5 nil #'oo--timer--lower-garbage-collection)
    (info! "Running timer for lowering garbage collection...")
    (set! reduction (/ (get-register :gc-cons-threshold) 10))
    (set! gc-floor (* 8 1024 1024))
    (set! gcp-default 0.2)
    (when (/= gc-cons-threshold gc-floor)
      (set! old gc-cons-threshold)
      (set! new (max (- old reduction) gc-floor))
      (setq gc-cons-threshold new)
      (info! "Lower `gc-cons-threshold' from %.2f to %.2f MB..." (mb old) (mb new)))
    (when (/= gc-cons-percentage gcp-default)
      (set! old (max gc-cons-percentage gcp-default))
      (set! new (max (- gc-cons-percentage 0.1) gcp-default))
      (info! "Lower `gc-cons-percentage' from %.1f to %.1f..." old new)
      (setq gc-cons-percentage new))
    (if (and (= gc-cons-threshold gc-floor)
             (= gc-cons-percentage gcp-default))
        (info! "Done with timer.")
      (run-with-timer 7 nil #'oo--timer--lower-garbage-collection))))
;;;; emacs-startup-hook
(defhook! oo-restore-startup-values-h (emacs-startup-hook :depth 90)
  "Restore the values of `file-name-handler-alist' and `gc-cons-threshold'."
  (info! "Restore the value of `file-name-handler-alist'.")
  (setq file-name-handler-alist (get-register :file-name-handler-alist))
  (setq gc-cons-threshold (* 40 1024 1024))
  (set-register :gc-cons-threshold gc-cons-threshold)
  (info! "Set the value of `gc-cons-threshold' to 40 MB.")
  (run-with-timer 5 nil #'oo--timer--lower-garbage-collection))
;;; provide
(provide 'init-startup)
;;; init-startup.el ends here
