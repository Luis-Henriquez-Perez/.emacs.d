;;; 998-mode-line-icons.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; Provide advices to add icons to segment strings.
;;
;;; Code:
(require '123-base-mode-line)
(require 'nerd-icons)

(defun oo-mode-line-nerd-icons--line-number (orig-fn &rest args)
  (awhen! (apply orig-fn args)
    (format "%s%s" (nerd-icons-powerline "nf-pl-line_number") it)))

(defun! oo-mode-line-nerd-icons--git-ahead (orig-fn &rest args)
  (awhen! (apply orig-fn args)
    (string-match "\\([[:digit:]]+\\)@" it)
    (set! count (match-string 1 it))
    (set! icon (nerd-icons-faicon "nf-fa-arrow_up_long" :v-adjust 0.01))
    (format "%s%s" count icon)))

(defun oo-mode-line-nerd-icons--buffer-name (orig-fn &rest args)
  (format "%s %s" (nerd-icons-icon-for-buffer) (apply orig-fn args)))

(defun! oo-mode-line-nerd-icons--read-only (orig-fn &rest args)
  (awhen! (apply orig-fn args)
    (nerd-icons-faicon "nf-fa-lock")))

(defun oo-mode-line-nerd-icons--kbd-macro (orig-fn &rest args)
  (when (apply orig-fn args)
    (nerd-icons-mdicon "nf-md-record_circle" :face 'error :v-adjust -0.0)))

(defun! oo-mode-line-nerd-icons--branch (orig-fn &rest args)
  (awhen! (apply orig-fn args)
    (set! icon (nerd-icons-devicon "nf-dev-git_branch" :v-adjust -0.01))
    (format "%s %s" icon it)))

(defun! oo-mode-line-nerd-icons--narrow (orig-fn &rest args)
  (awhen! (apply orig-fn args)
    (nerd-icons-codicon "nf-cod-fold" :face 'warning)))

(defun! oo-mode-line-nerd-icons--pomodoro (orig-fn &rest args)
  (awhen! (apply orig-fn args)
    (string-match "\\([bw]\\)\\([[:digit:]][[:digit:]]:[[:digit:]][[:digit:]]\\)" it)
    (set! type (match-string 1 it))
    (set! time (match-string 2 it))
    (pcase type
      ("w" (set! icon (nerd-icons-pomicon "nf-pom-pomodoro_ticking" :v-adjust 0)))
      ("b" (set! icon (nerd-icons-codicon "nf-cod-coffee" :v-adjust 0))))
    (format "%s %s" icon time)))

(defun! oo-mode-line-nerd-icons--time (orig-fn &rest args)
  (set! icon (nerd-icons-wicon (format-time-string "nf-weather-time_%-I")))
  (format "%s %s" icon (apply orig-fn args)))

(defun! oo-mode-line-nerd-icons--date (orig-fn &rest args)
  (set! icon (nerd-icons-faicon "nf-fa-calendar"))
  (format "%s %s" icon (apply orig-fn args)))

(defun oo-mode-line-nerd-icons--buffer-modified (orig-fn &rest args)
  (when (apply orig-fn args)
    (nerd-icons-faicon "nf-fa-save" :face 'error)))

(defun! oo-mode-line-nerd-icons--text-scale (orig-fn &rest args)
  (awhen! (apply orig-fn args)
    ;; TODO: Preserve the face of the original string.
    (string-match "(\\([+-]\\)\\([[:digit:]]+\\))" it)
    (set! change (match-string 1 it))
    (set! amount (match-string 2 it))
    (pcase change
      ("+" (set! icon (nerd-icons-mdicon "nf-md-magnify_plus")))
      ("-" (set! icon (nerd-icons-mdicon "nf-md-magnify_minus"))))
    (format "%s %s%s" icon change amount)))

(defun! oo-mode-line-nerd-icons--emms (orig-fn &rest args)
  "Return indicator for the current track."
  (awhen! (apply orig-fn args)
    (string-match "\\([[:upper:]]+\\) \\(.+\\)" it)
    (set! trigger (match-string 1 it))
    (set! segment (match-string 2 it))
    (pcase trigger
      ("PAUSED" (set! icon (nerd-icons-faicon "nf-fa-pause")))
      ("REPEAT" (set! icon (nerd-icons-faicon "nf-fa-repeat")))
      ("PLAYING" (set! icon (nerd-icons-mdicon "nf-md-music_note"))))
    (format "%s %s" icon segment)))

(define-minor-mode oo-mode-line-icons-mode
  "Display icons in the mode line."
  :global t
  (cond (oo-mode-line-icons-mode
         (advice-add 'oo-mode-line-segment--kbd-macro     :around 'oo-mode-line-nerd-icons--kbd-macro)
         (advice-add 'oo-mode-line-segment--buffer-name     :around 'oo-mode-line-nerd-icons--buffer-name)
         (advice-add 'oo-mode-line-segment--line-number     :around 'oo-mode-line-nerd-icons--line-number)
         (advice-add 'oo-mode-line-segment--buffer-modified :around 'oo-mode-line-nerd-icons--buffer-modified)
         (advice-add 'oo-mode-line-segment--text-scale      :around 'oo-mode-line-nerd-icons--text-scale)
         (advice-add 'oo-mode-line-segment--read-only       :around 'oo-mode-line-nerd-icons--read-only)
         (advice-add 'oo-mode-line-segment--narrow          :around 'oo-mode-line-nerd-icons--narrow)
         (advice-add 'oo-mode-line-segment--time            :around 'oo-mode-line-nerd-icons--time)
         (advice-add 'oo-mode-line-segment--date            :around 'oo-mode-line-nerd-icons--date)
         (advice-add 'oo-mode-line-segment--git-ahead       :around 'oo-mode-line-nerd-icons--git-ahead)
         (advice-add 'oo-mode-line-segment--pomodoro        :around 'oo-mode-line-nerd-icons--pomodoro)
         (advice-add 'oo-mode-line-segment--emms            :around 'oo-mode-line-nerd-icons--emms)
         (advice-add 'oo-mode-line-segment--branch          :around 'oo-mode-line-nerd-icons--branch))
        (t
         (advice-remove 'oo-mode-line-segment--kbd-macro     'oo-mode-line-nerd-icons--kbd-macro)
         (advice-remove 'oo-mode-line-segment--buffer-name     'oo-mode-line-nerd-icons--buffer-name)
         (advice-remove 'oo-mode-line-segment--line-number     'oo-mode-line-nerd-icons--line-number)
         (advice-remove 'oo-mode-line-segment--buffer-modified 'oo-mode-line-nerd-icons--buffer-modified)
         (advice-remove 'oo-mode-line-segment--text-scale      'oo-mode-line-nerd-icons--text-scale)
         (advice-remove 'oo-mode-line-segment--read-only       'oo-mode-line-nerd-icons--read-only)
         (advice-remove 'oo-mode-line-segment--narrow          'oo-mode-line-nerd-icons--narrow)
         (advice-remove 'oo-mode-line-segment--time            'oo-mode-line-nerd-icons--time)
         (advice-remove 'oo-mode-line-segment--date            'oo-mode-line-nerd-icons--date)
         (advice-remove 'oo-mode-line-segment--git-ahead       'oo-mode-line-nerd-icons--git-ahead)
         (advice-remove 'oo-mode-line-segment--pomodoro        'oo-mode-line-nerd-icons--pomodoro)
         (advice-remove 'oo-mode-line-segment--emms            'oo-mode-line-nerd-icons--emms)
         (advice-remove 'oo-mode-line-segment--branch          'oo-mode-line-nerd-icons--branch))))
;;; provide
(provide '998-mode-line-icons)
;;; 998-mode-line-icons.el ends here
