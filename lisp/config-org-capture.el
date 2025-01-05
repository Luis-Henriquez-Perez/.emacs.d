;;; config-org-capture.el --- Configure org-capture -*- lexical-binding: t; -*-
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
;; Configure org-capture.
;;
;;; Code:
;;;; Requirements
(require 'base)
(require 'doct)
(require 'org-ml)
(require 'ts)
(require 'org-capture)
;;;; disable header-line
(setq-hook! org-capture-mode-hook header-line-format nil)
;;;; use completing-read for org-capture
(defun! +org-capture-choose-template ()
  "Choose capture template to open."
  (interactive)
  (set! templates (mapcar (lambda (it) (cons (cl-second it) (cl-first it))) org-capture-templates))
  (set! choosen (completing-read "Choose a Templates: " templates))
  (set! key (cdr (assoc choosen templates)))
  (if key
      (org-capture nil key)
    (error "No capture template corresponding to %S" choosen)))
;;;; +org-todo-file
(defvar +org-todo-file (expand-file-name (expand-file-name "todo.org" org-directory))
  "Return the file to be used for capturing TODOs.")
;;;; main capture template
(defun +org-planning ()
  "Return a timestamp."
  (let* ((now (ts-adjust 'day 5 (ts-now)))
         (time (list (ts-year now) (ts-month now) (ts-day now) (ts-hour now)
                     (ts-min now))))
    (org-ml-build-planning! :deadline time)))

(defun! +org-capture--todo-template (&optional todo-keyword)
  "Return template string."
  (require 'org-ml)
  (thread-last (org-ml-build-headline! :level 1 :todo-keyword todo-keyword :title-text "%?")
               (org-ml-headline-set-node-property "ID" (org-id-new))
               (org-ml-headline-set-node-property "Effort" "0:05")
               (org-ml-to-string)))

(defun +org-capture-plain-template ()
  "Return capture template as a string."
  (thread-last (org-ml-build-headline! :level 1 :title-text "%?")
               (org-ml-headline-set-node-property "ID" (org-id-new))
               (org-ml-to-string)))

(defun +org-capture-todo-template ()
  "Return the TODO capture template as a string."
  (+org-capture--todo-template "TODO"))

(defun +org-capture-bug-template ()
  "Return the BUG capture template as a string."
  (+org-capture--todo-template "BUG"))

(defun +org-capture-question-template ()
  "Return the QUESTION capture template as a string."
  (+org-capture--todo-template "QUESTION"))

(defun +org-capture-plain ()
  (interactive)
  (org-capture nil "p"))

(defun +org-capture-todo ()
  (interactive)
  (org-capture nil "t"))

(defun +org-capture-bug ()
  (interactive)
  (org-capture nil "b"))

(defun +org-capture-question ()
  (interactive)
  (org-capture nil "q"))

(defun +org-capture--refile ()
  (org-back-to-heading)
  (call-interactively #'org-refile))

(setq org-capture-templates
      (append (doct (list (list "todo"
                                :keys "t"
                                :file +org-todo-file
                                :template #'+org-capture-todo-template)))
              (doct (list (list "bug"
                                :keys "b"
                                :file +org-todo-file
                                :template #'+org-capture-bug-template)))
              (doct (list (list "question"
                                :keys "q"
                                :file +org-todo-file
                                :template #'+org-capture-question-template)))
              (doct (list (list "plain"
                                :keys "p"
                                :file org-default-notes-file
                                :template #'+org-capture-plain-template)))))
;;; provide
(provide 'config-org-capture)
;;; config-org-capture.el ends here
