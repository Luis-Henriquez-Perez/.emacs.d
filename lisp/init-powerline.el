;;; init-powerline.el --- Initialize powerline -*- lexical-binding: t; -*-
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
;; Initialize powerline.
;;
;;; Code:
(require 'battery)
(require 'dash)
(require 'powerline)
(require 'spaceline)
;;;; powerline settings
(setq powerline-default-separator 'arrow)
(setq powerline-height 33)

(hook! enable-theme-functions powerline-reset :ignore-args t)
;;;; variables
(defvar oo-modeline-icons 'nerd-icons
  "Type of icons to use in the modeline.
Values can be `nerd-icons', `all-the-icons' and nil.  If nil, icons in the
modeline are disabled.")

(defun oo-modeline-cycle-icons (select-p)
  "Cycle the available icon modeline options in `oo-modeline-icons'.
With prefix argument, SELECT-P, prompt for specific icon type to display."
  (interactive "P")
  (if select-p
      (alet! (list 'all-the-icons 'nerd-icons 'none)
        (completing-read "Choose type of icons: " it))
    (pcase oo-modeline-icons
      ('all-the-icons
       (require 'all-the-icons)
       (setq oo-modeline-icons 'nerd-icons))
      ('nerd-icons
       (require 'nerd-icons)
       (setq oo-modeline-icons nil))
      ('nil
       (setq oo-modeline-icons 'all-the-icons)))))

(defun! oo-modeline-cycle-separators (select-p)
  "Cycle through available powerline separators.
With prefix argument, SELECT-P, select one explicitly."
  (interactive "P")
  (set! separators '(alternate arrow arrow-fade bar box brace
                               butt chamfer contour curve rounded roundstub wave
                               zigzag slant utf-8))
  (if select-p
      (awhen! (completing-read "Choose separator: " separators)
        (setq powerline-default-separator (seq-random-elt it))
        (powerline-reset))
    (setq powerline-default-separator (seq-random-elt separators))
    (powerline-reset)))
;;;; segment faces
(defface oo-modeline-segment-1 '((t (:background "white" :foreground "black")))
  "Face for the first modeline segment.")
;;;; utility functions
;; When you modify the modeline variable the modeline is not automatically
;; updated.  You only see the updated version when you open a new buffer.  To
;; actually see the updated modeline in buffers that are already open you need
;; to change their buffer-local mode-line-format variable and then call
;; `force-mode-line-update' after you make the change to the default value of
;; `mode-line-format'.
(defun oo-modeline-update ()
  "Update the mode line in all buffers to reflect the default `mode-line-format'."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (setq-local mode-line-format (default-value 'mode-line-format))))
  (force-mode-line-update))

(defun! oo-modeline-component (name)
  "Return string representation of component named NAME.
If an error is raised from component function."
  (condition-case err
      (set! return-value (funcall (intern (format "oo-modeline-component--%s" name))))
    (error
     (error! "Modeline component %s raised a %s error because of %s" name (car err) (cdr err))
     (return! "")))
  (pcase return-value
    ('nil
      "")
    ((pred stringp)
     return-value)
    (_
     (error "Modeline component %s returned a non-string %s." name return-value)))
  return-value)

(defun! oo-modeline-join-components (components)
  "Join mode line COMPONENTS into a single string."
  (dolist (c components)
    (aand! (oo-modeline-component c)
           (and (stringp it) (not (string-empty-p it)))
           (pushing! strings it)))
  (if strings
      (concat "\s" (string-join (nreverse strings) "\s") "\s")
    ""))

(defun! oo--modeline-render-lhs (segment-names faces &optional sep)
  "Render the left-hand side of the modeline."
  (set! sep (or sep (oo-modeline-left-separator)))
  (set! prev-face (pop faces))
  (alet2! (length segment-names) (length faces)
    (when (> it other)
      (set! faces (cons (car faces) (-take (1- it) (-cycle (cdr faces)))))))
  (for! (reverse (name . face) (-zip-pair segment-names faces))
    (set! segment (funcall (intern (format "oo-modeline-segment--%s" name))))
    (when (and (stringp segment) (not (string-empty-p segment)))
      (pushing! lhs (funcall sep prev-face face))
      (add-face-text-property 0 (length segment) face t segment)
      (pushing! lhs segment)
      (set! prev-face face)))
  lhs)

(defun! oo--modeline-render-rhs (segment-names faces)
  "Render the right-hand side of the modeline."
  (set! faces (cons (car faces) (reverse (cdr faces))))
  (oo--modeline-render-lhs segment-names faces (oo-modeline-right-separator)))

(defun oo-modeline-left-separator (&optional right-p)
  "Return the left separator."
  (intern (format "powerline-%s-%s"
                  (powerline-current-separator)
                  (funcall (if right-p #'cdr #'car) powerline-default-separator-dir))))

(defun oo-modeline-right-separator ()
  "Return the right separator."
  (oo-modeline-left-separator 'right))

(defun! oo-modeline-render (left right faces)
  "Render modeline."
  (alet! (oo--modeline-render-rhs right faces)
    (concat (powerline-render (oo--modeline-render-lhs left faces))
            (powerline-fill (car faces) (powerline-width it))
            (powerline-render it))))
;;;; components
(defun oo-modeline-component--untracked ()
  "Indicate if a file is apart of a project directory but is not tracked."
  (and (buffer-file-name)
       (locate-dominating-file (buffer-file-name) ".git")
       (string-empty-p (shell-command-to-string (format "git ls-files %s" buffer-file-name)))
       (pcase oo-modeline-icons
         ('nerd-icons
           (require 'nerd-icons)
           ;; (format "%s %s" (nerd-icons-powerline "nf-pl-line_number") ln)
           (all-the-icons-faicon "times" :v-adjust +0.02))
         ('all-the-icons
           (require 'all-the-icons)
           (all-the-icons-faicon "times" :v-adjust +0.02))
         (_
          "untracked"))))

(defun! oo-modeline-component--line-number ()
  "Return the line-number component for the mode line."
  (set! ln (powerline-raw "%l"))
  (pcase oo-modeline-icons
    ('nerd-icons
     (require 'nerd-icons)
     (format "%s %s" (nerd-icons-powerline "nf-pl-line_number") ln))
    ('all-the-icons
     ;; (require 'all-the-icons)
     ln)
    (_
     ln)))

(defun! oo-modeline-component--percentage-of-buffer ()
  "Return the percentage of the buffer."
  ;; I know about the mode line `%p' option, but it fails with folding.  Simply
  ;; dividing point by point-max is more accurate.
  ;; (powerline-raw "%p")
  (set! percentage (* 100 (/ (float (point)) (point-max))))
  (cond ((> percentage 95) "BOT")
        ((< percentage 5) "TOP")
        ((format "%d%%%%" percentage))))

(defun oo-modeline-component--buffer-name ()
  "Buffer name indicator for mode line."
  (pcase oo-modeline-icons
    ('nerd-icons
      (require 'nerd-icons)
      (format "%s %s" (nerd-icons-icon-for-buffer) (buffer-name)))
    ('all-the-icons
      (require 'all-the-icons)
      (format "%s %s" (all-the-icons-icon-for-buffer) (buffer-name)))
    (_
     (buffer-name))))

(defun oo-modeline-component--kbd-macro ()
  "Return an indicator for keyboard macro recording or playback."
  (or (and defining-kbd-macro
           (pcase oo-modeline-icons
             ('nerd-icons
               (require 'nerd-icons)
               (nerd-icons-mdicon "nf-md-record_circle" :face 'error :v-adjust -0.0))
             ('all-the-icons
               (require 'all-the-icons)
               (all-the-icons-material "fiber_manual_record" :face 'error :v-adjust -0.2))
             (_ "•REC")))
      (and executing-kbd-macro
           (pcase oo-modeline-icons
             ('nerd-icons
               (require 'nerd-icons)
               (nerd-icons-mdicon "nf-md-play"))
             ('all-the-icons
               (require 'all-the-icons)
               (all-the-icons-faicon "play"))
             (_ "PLAYING")))))

(defun! oo-modeline-component--branch ()
  "Return the branch name as a modeline segment."
  (set! branch (string-trim (shell-command-to-string "git rev-parse --abbrev-ref HEAD")))
  (pcase oo-modeline-icons
    ('nerd-icons
      (require 'nerd-icons)
      (set! icon (nerd-icons-devicon "nf-dev-git_branch" :v-adjust -0.01))
      (format "%s %s" icon branch))
    ('all-the-icons
      (require 'all-the-icons)
      (set! icon (all-the-icons-octicon "git-branch"  :v-adjust -0.01))
      ;; (set! remote (shell-command-to-string "git rev-parse --abbrev-ref --symbolic-full-name @{u}"))
      ;; (set! unmerged-commits (shell-command-to-string "git rev-list --count HEAD..%s" remote))
      (format "%s %s" icon branch))
    (_
     branch)))

(defun! oo-modeline-component--git-ahead ()
  "Display the number of commits ahead of origin.
If 0, do not display anything."
  (set! count (string-to-number (shell-command-to-string "git rev-list --count @{upstream}..HEAD")))
  (when (> count 0)
    (pcase oo-modeline-icons
      ('all-the-icons
       (set! long-arrow (all-the-icons-faicon "long-arrow-up" :v-adjust 0.01))
       (propertize (format "%s%s" count long-arrow) 'face 'success))
      ('nerd-icons
       (set! long-arrow (nerd-icons-faicon "nf-fa-arrow_up_long" :v-adjust 0.01))
       (propertize (format "%s%s" count long-arrow) 'face 'success))
      (_
       (propertize (format "%s@" count) 'face 'success)))))

;; (defun! oo-modeline-component--log-error ()
;;   "Notify of error appearing in my log."
;;   (with-current-buffer "*log*"
;;     (save-excursion
;;       (goto-char (point-min))
;;       (when (re-search-forward (rx "[ERROR]") nil t)
;;         ;; Find the error type.
;;         (re-search )
;;         (pcase oo-modeline-icons
;;           ('all-the-icons
;;             (set! icon (all-the-icons-material "warning" :face 'error))
;;             (format "%s %s" icon 'void-function))
;;           ('nerd-icons
;;             ;; (set! error (all-the-icons-material "warning" :face 'error))
;;             )
;;           (_))))))

(defun! oo-modeline-component--narrow ()
  "Return an indicator for a narrowed buffer in the modeline."
  (when (or (buffer-narrowed-p)
            (and (bound-and-true-p fancy-narrow-mode)
                 (fancy-narrow-active-p))
            (bound-and-true-p dired-narrow-mode))
    (pcase oo-modeline-icons
      ('all-the-icons
       (all-the-icons-material "unfold_less" :face 'warning))
      ('nerd-icons
       (nerd-icons-codicon "nf-cod-fold" :face 'warning))
      (_
       "><"))))

(defun! oo-modeline-component--pomodoro ()
  "Return indicator for remaining Pomodoro time for work or break."
  (defvar pomodoro-mode-line-string)
  (when (and (bound-and-true-p pomodoro-mode-line-string)
             (not (string-empty-p pomodoro-mode-line-string)))
    (string-match "\\([[:alpha:]]\\)\\([[:digit:]][[:digit:]]:[[:digit:]][[:digit:]]\\)" pomodoro-mode-line-string)
    (set! type (match-string 1 pomodoro-mode-line-string))
    (set! time (match-string 2 pomodoro-mode-line-string))
    (pcase oo-modeline-icons
      ('all-the-icons
       (pcase type
         ("w" (set! icon (all-the-icons-material "work")))
         ("b" (set! icon (all-the-icons-faicon "coffee" :v-adjust 0))))
       (format "%s %s" icon time))
      ('nerd-icons
       (pcase type
         ("w" (set! icon (nerd-icons-pomicon "nf-pom-pomodoro_ticking" :face 'error :v-adjust 0)))
         ("b" (set! icon (nerd-icons-codicon "nf-cod-coffee" :v-adjust 0))))
       (format "%s %s" icon time))
      (_
       (format "%s %s" type time)))))

(defun! oo-modeline-component--current-time ()
  "Display the current time."
  (set! time (format-time-string "%H:%M"))
  (set! date (format-time-string "%m-%d"))
  (pcase oo-modeline-icons
    ('nerd-icons
     (require 'nerd-icons)
     (set! dicon (nerd-icons-faicon "nf-fa-calendar"))
     (set! ticon (nerd-icons-wicon (format-time-string "nf-weather-time_%-I")))
     (format "%s %s %s %s" dicon date ticon time))
    ('all-the-icons
     (require 'all-the-icons)
     (set! dicon (all-the-icons-faicon "calendar" :v-adjust 0.01))
     (set! ticon (all-the-icons-wicon (format-time-string "time-%-I") :v-adjust 0.01))
     (format "%s %s %s %s" dicon date ticon time))
    (_
     (format "%s %s" date time))))

(defun! oo-modeline-component--read-only ()
  "Return indicator for whether file is read-only."
  (when buffer-read-only
    (pcase oo-modeline-icons
      ('all-the-icons
       (all-the-icons-material "lock" :face 'error))
      ('nerd-icons
       (set! icon (nerd-icons-faicon "nf-fa-lock" :face 'error))
       icon)
      (_
       "LOCKED"))))

(defun! oo-modeline-component--buffer-modified ()
  "Return indicator for buffer modified.
If the current buffer is modified."
  (when (and (buffer-file-name) (buffer-modified-p))
    (pcase oo-modeline-icons
      ('all-the-icons
       (all-the-icons-material "save" :face 'error))
      ('nerd-icons
       (nerd-icons-faicon "nf-fa-save" :face 'error))
      (_
       (propertize "MODIFIED" 'face 'error)))))

(defun! oo-modeline-component--evil-state ()
  "Return indicator for evil state."
  (when (bound-and-true-p evil-mode)
    (symbol-name evil-state)))

(defun! oo-modeline-component--battery ()
  "Return component."
  (set! status (funcall battery-status-function))
  (set! percentage (thread-last (battery-format "%p" status)
                                (string-to-number)
                                (round)))
  (set! charging-p (not (equal "Discharging" (battery-format "%B" status))))
  (pcase oo-modeline-icons
    ('nerd-icons
     (cond ((and (> percentage 90) charging-p)
            (set! battery (nerd-icons-faicon "nf-fa-battery_4" :face 'success))
            (set! bolt (nerd-icons-mdicon "nf-md-lightning_bolt" :face 'success))
            (format "%s %s" battery bolt))
           ((> percentage 90)
            (set! battery (nerd-icons-faicon "nf-fa-battery_4" :face 'warning))
            (set! arrow (nerd-icons-faicon "nf-fa-arrow_down" :face 'warning))
            (format "%s %s %d%%%%" arrow battery percentage))
           ((> percentage 80)
            (set! battery (nerd-icons-faicon "nf-fa-battery_3" :face 'warning))
            (set! arrow (nerd-icons-faicon "nf-fa-arrow_down" :face 'warning))
            (format "%s %s %d%%%%" arrow battery percentage))
           ((> percentage 70)
            (set! battery (nerd-icons-faicon "nf-fa-battery_2" :face 'warning))
            (set! arrow (nerd-icons-faicon "nf-fa-arrow_down" :face 'warning))
            (format "%s %d%%%%" battery percentage))
           ((> percentage 60)
            (set! battery (nerd-icons-faicon "nf-fa-battery_1" :face 'error))
            (set! arrow (nerd-icons-faicon "nf-fa-arrow_down" :face 'warning))
            (format "%s %d%%%%" battery percentage))
           ((> percentage 50)
            (set! battery (nerd-icons-faicon "nf-fa-battery_0" :face 'error))
            (set! arrow (nerd-icons-faicon "nf-fa-arrow_down" :face 'warning))
            (format "%s %d%%%%" battery percentage))))
    ('all-the-icons
     (cond ((charging-p ""))
           ((> percentage 90)
            (all-the-icons-faicon "battery-full" :v-adjust 0.01))
           ((> percentage 80)
            (format "%s %s" (all-the-icons-faicon "battery-three-quarters") percentage))
           ((> percentage 70)
            (format "%s %s" (all-the-icons-faicon "battery-three-quarters") percentage))
           ((> percentage 60)
            (format "%s %s" (all-the-icons-faicon "battery-three-quarters") percentage))
           ((> percentage 50)
            ())))))

(declare-function emms-track-description "emms")
(declare-function emms-playlist-current-selected-track "emms")
(defun! oo-modeline-component--emms ()
  "Return indicator for emms.
Returns whether current track is playing."
  (when (bound-and-true-p emms-player-playing-p)
    (set! path (emms-track-description (emms-playlist-current-selected-track)))
    (set! track (file-name-nondirectory (directory-file-name path)))
    (cond ((bound-and-true-p emms-player-paused-p)
           (pcase oo-modeline-icons
             ('nerd-icons
              (nerd-icons-codicon))
             ('all-the-icons
              (all-the-icons-faicon "pause-circle"))
             (_
              (format "PAUSED %s" track))))
          ((bound-and-true-p emms-repeat-track)
           (pcase oo-modeline-icons
             ('nerd-icons
              (nerd-icons-faicon "nf-fa-repeat"))
             ('all-the-icons
              (all-the-icons-faicon "repeat"))
             (_
              (format "REPEAT %s" track))))
          (t
           (pcase oo-modeline-icons
             ('nerd-icons
              (nerd-icons-faicon "nf-fa-play"))
             ('all-the-icons
              (all-the-icons-material "play-circle"))
             (_
              (format "PLAY %s" track)))))))
;;;; segments
(defun oo-modeline-segment--evil-state ()
  "Display segment for displaying evil state."
  (oo-modeline-join-components '(evil-state)))

(defun oo-modeline-segment--buffer-info ()
  "Display general (usually buffer-related) information."
  (oo-modeline-join-components '(narrow read-only kbd-macro buffer-modified buffer-name)))

(defun oo-modeline-segment--version-control ()
  "Display version control information."
  (when (and (buffer-file-name) vc-mode (string-match "Git" vc-mode))
    (oo-modeline-join-components '(branch git-ahead))))

(defun oo-modeline-segment--current-time ()
  "Display the current date and time."
  (oo-modeline-join-components '(current-time)))

(defun oo-modeline-segment--pomodoro ()
  "Display time elapsed for work or play."
  (oo-modeline-join-components '(pomodoro)))

(defun oo-modeline-segment--battery ()
  "Display the battery status."
  (oo-modeline-join-components '(battery)))

(defun oo-modeline-segment--buffer-location ()
  "Display the buffer location.
This means the line number and percentage."
  (oo-modeline-join-components '(line-number percentage-of-buffer)))

(defun oo-modeline-segment--log-error ()
  (oo-modeline-join-components '(log-error)))
;;;; custom modelines
(defun! oo-modeline-main ()
  "Return my main modeline."
  (set! active (powerline-selected-window-active))
  (set! face1 (if active 'oo-modeline-segment-1 'powerline-inactive1))
  (set! face2 (if active 'mode-line 'mode-line-inactive))
  (set! face3 (if active 'powerline-active2 'powerline-inactive2))
  (set! fill-face (if active 'powerline-active0 'powerline-inactive0))
  (set! evil-face (spaceline-highlight-face-evil-state))
  (oo-modeline-render '(evil-state buffer-info version-control)
                      '(pomodoro battery buffer-location current-time)
                      `(,fill-face ,evil-face ,face1 ,face2 ,face3)))
;;;; initialization
(defhook! oo-initialize-modeline-h (after-init-hook :depth 90)
  "Initialize modeline."
  (setq-default mode-line-format '("%e" (:eval (oo-modeline-main))))
  (oo-modeline-update))
;;; provide
(provide 'init-powerline)
;;; init-powerline.el ends here
