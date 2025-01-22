;;; base-mode-line.el --- Initialize powerline -*- lexical-binding: t; -*-
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
;; Initialize my modeline.
;;
;;; Code:
(eval-when-compile (require 'base-macros))
(require 'battery)
(require 'dash)
(require 'powerline)
(require 'spaceline)
;;;; powerline settings
;; Resetting the modeline after a theme change ensures separator colors are
;; updated to match the current theme.
(hook! enable-theme-functions powerline-reset :ignore-args t)

(setq powerline-default-separator 'zigzag)

;; A height of 40 also looks good, but I am fine with 33.  By the way, for this
;; to take effect you need to call `powerline-reset' after setting it.
(setq powerline-height 33)
;;;; modeline settings
(defvar oo-mode-line-icons 'nerd-icons
  "Type of icons to use in the modeline.
Values can be `nerd-icons', `all-the-icons' and nil.  If nil, icons in the
modeline are disabled.")
;;;; faces
(defface oo-mode-line-segment-1 '((t (:background "white" :foreground "black")))
  "Face for the first modeline segment.")
;;;; utility functions
;; When you modify the modeline variable the modeline is not automatically
;; updated.  You only see the updated version when you open a new buffer.  To
;; actually see the updated modeline in buffers that are already open you need
;; to change their buffer-local mode-line-format variable and then call
;; `force-mode-line-update' after you make the change to the default value of
;; `mode-line-format'.
(defun! oo-mode-line-component (name)
  "Return string representation of component named NAME.
If an error is raised from component function."
  (condition-case err
      (set! return-value (funcall (intern (format "oo-mode-line-component--%s" name))))
    (error
     (error! "MODELINE %s : %s -> %s" name (car err) (cdr err))
     (return! "")))
  (pcase return-value
    ('nil
     "")
    ((pred stringp)
     return-value)
    (_
     (error "Modeline component %s returned a non-string %s." name return-value)))
  return-value)

(defun! oo-mode-line-join-components (components)
  "Join mode line COMPONENTS into a single string."
  (dolist (c components)
    (aand! (oo-mode-line-component c)
           (and (stringp it) (not (string-empty-p it)))
           (pushing! strings it)))
  (if strings
      (concat "\s" (string-join (nreverse strings) "\s") "\s")
    ""))

(defun! oo--modeline-render-lhs (segment-names faces)
  "Render the left-hand side of the modeline."
  (set! sep (or sep (oo-mode-line-left-separator)))
  (set! prev-face (pop faces))
  (alet2! (length segment-names) (length faces)
    (when (> it other)
      (set! faces (cons (car faces) (-take (1- it) (-cycle (cdr faces)))))))
  (for! (reverse (name . face) (-zip-pair segment-names faces))
    (set! segment (funcall (intern (format "oo-mode-line-segment--%s" name))))
    (when (and (stringp segment) (not (string-empty-p segment)))
      (pushing! lhs (funcall sep face prev-face))
      (add-face-text-property 0 (length segment) face t segment)
      (pushing! lhs segment)
      (set! prev-face face)))
  lhs)

(defun! oo--modeline-render-rhs (segment-names faces)
  "Render the right-hand side of the modeline."
  (set! sep (oo-mode-line-right-separator))
  (set! prev-face (pop faces))
  (alet2! (length segment-names) (length faces)
    (when (> it other)
      (set! faces (cons (car faces) (-take (1- it) (-cycle (cdr faces)))))))
  (for! (reverse (name . face) (-zip-pair (reverse segment-names) faces))
    (set! segment (funcall (intern (format "oo-mode-line-segment--%s" name))))
    (when (and (stringp segment) (not (string-empty-p segment)))
      (collecting! rhs (funcall sep prev-face face))
      (add-face-text-property 0 (length segment) face t segment)
      (collecting! rhs segment)
      (set! prev-face face)))
  rhs)

(defun oo-mode-line-left-separator (&optional right-p)
  "Return the left separator."
  ;; (lambda (&rest _) "")
  (intern (format "powerline-%s-%s"
                  (powerline-current-separator)
                  (funcall (if right-p #'cdr #'car)
                           powerline-default-separator-dir)))
  )

(defun oo-mode-line-right-separator ()
  "Return the right separator."
  (oo-mode-line-left-separator 'right))

(defun oo-mode-line-render (left right faces)
  "Render modeline with LEFT and RIGHT segments using FACES."
  (let* ((lhs-str (powerline-render (oo--modeline-render-lhs left faces)))
         (rhs-str (powerline-render (oo--modeline-render-rhs right faces)))
         (rhs (string-pixel-width rhs-str))
         (lhs (string-pixel-width lhs-str))
         (window (window-pixel-width))
         (scroll-bar (window-scroll-bar-width))
         (right-divider (window-right-divider-width))
         (right-fringe (frame-parameter nil 'right-fringe))
         ;; I do one extra pixel to cover the 1 pixel wide space at the end.
         ;; Also it is debatable to make the modeline smaller for the window
         ;; divider, in my opinion it looks better covering it and the modeline
         ;; is not big enough to make using the scroll bar difficult.
         ;; I do not know what this 4 pixels is for but that is what it takes to
         ;; balance my modeline.
         (mid (1+ (- window lhs rhs right-divider scroll-bar right-fringe)))
         ;; Doom has this margin but I tell you it was just messing me up.  The
         ;; modeline fits perfectly.
         ;; (margin (* (or (cdr (window-margins)) 1) (frame-char-width)))
         ;; (margin (+ 40 (* (or (cdr (window-margins)) 1) (frame-char-width))))
         ;; (margin 0)
         )
    (concat lhs-str
            (propertize "\s" 'face (car faces) 'display `(space :align-to (,(+ lhs mid))))
            rhs-str)))
;;;; components
(defun oo-mode-line-component--untracked ()
  "Indicate if a file is apart of a project directory but is not tracked."
  (and (equal (vc-state (buffer-file-name)) 'unregistered)
       (pcase oo-mode-line-icons
         ('nerd-icons
          (require 'nerd-icons)
          ;; (format "%s %s" (nerd-icons-powerline "nf-pl-line_number") ln)
          (all-the-icons-faicon "times" :v-adjust +0.02))
         ('all-the-icons
          (require 'all-the-icons)
          (all-the-icons-faicon "times" :v-adjust +0.02))
         (_
          "UNTRACKED"))))

(defun! oo-mode-line-component--line-number ()
  "Return the line-number component for the mode line."
  (set! ln (format-mode-line "%l"))
  (pcase oo-mode-line-icons
    ('nerd-icons
     (require 'nerd-icons)
     (format "%s %s" (nerd-icons-powerline "nf-pl-line_number") ln))
    ('all-the-icons
     ;; (require 'all-the-icons)
     ln)
    (_
     ln)))

(defun! oo-mode-line-component--percentage-of-buffer ()
  "Return the percentage of the buffer."
  ;; I know about the mode line `%p' option, but it fails with folding.  Simply
  ;; dividing point by point-max is more accurate.
  ;; (powerline-raw "%p")
  (set! percentage (* 100 (/ (float (point)) (point-max))))
  (cond ((> percentage 95) "BOT")
        ((< percentage 5) "TOP")
        (t
         ;; (number-to-string (round percentage))
         (concat (number-to-string (round percentage)) "%")
         )))

(defun oo-mode-line-component--buffer-name ()
  "Buffer name indicator for mode line."
  (pcase oo-mode-line-icons
    ('nerd-icons
     (require 'nerd-icons)
     (format "%s %s" (nerd-icons-icon-for-buffer) (buffer-name)))
    ('all-the-icons
     (require 'all-the-icons)
     (format "%s %s" (all-the-icons-icon-for-buffer) (buffer-name)))
    (_
     (buffer-name))))

(defun oo-mode-line-component--kbd-macro ()
  "Return an indicator for keyboard macro recording or playback."
  (or (and defining-kbd-macro
           (pcase oo-mode-line-icons
             ('nerd-icons
               (require 'nerd-icons)
               (nerd-icons-mdicon "nf-md-record_circle" :face 'error :v-adjust -0.0))
             ('all-the-icons
               (require 'all-the-icons)
               (all-the-icons-material "fiber_manual_record" :face 'error :v-adjust -0.2))
             (_ "•REC")))
      (and executing-kbd-macro
           (pcase oo-mode-line-icons
             ('nerd-icons
               (require 'nerd-icons)
               (nerd-icons-mdicon "nf-md-play"))
             ('all-the-icons
               (require 'all-the-icons)
               (all-the-icons-faicon "play"))
             (_ "PLAYING")))))

(defun! oo-mode-line-component--branch ()
  "Return the branch name as a modeline segment."
  (set! branch (and vc-mode (cadr (split-string (string-trim vc-mode) "^[A-Z]+[-:]+"))))
  (pcase oo-mode-line-icons
    ('nerd-icons
     (require 'nerd-icons)
     (set! icon (nerd-icons-devicon "nf-dev-git_branch" :v-adjust -0.01))
     (format "%s %s" icon branch))
    ('all-the-icons
     (require 'all-the-icons)
     (set! icon (all-the-icons-octicon "git-branch"  :v-adjust -0.01))
     (format "%s %s" icon branch))
    (_
     branch)))

(defun! oo-mode-line-component--git-ahead ()
  "Display the number of commits ahead of origin.
If 0, do not display anything."
  (set! count (string-to-number (shell-command-to-string "git rev-list --count @{upstream}..HEAD")))
  (when (> count 0)
    (pcase oo-mode-line-icons
      ('all-the-icons
       (set! long-arrow (all-the-icons-faicon "long-arrow-up" :v-adjust 0.01))
       (propertize (format "%s%s" count long-arrow) 'face 'success))
      ('nerd-icons
       (set! long-arrow (nerd-icons-faicon "nf-fa-arrow_up_long" :v-adjust 0.01))
       (propertize (format "%s%s" count long-arrow) 'face 'success))
      (_
       (propertize (format "%s@" count) 'face 'success)))))

(defun! oo-mode-line-component--narrow ()
  "Return an indicator for a narrowed buffer in the modeline."
  (when (or (buffer-narrowed-p)
            (and (bound-and-true-p fancy-narrow-mode)
                 (fancy-narrow-active-p))
            (bound-and-true-p dired-narrow-mode))
    (pcase oo-mode-line-icons
      ('all-the-icons
       (all-the-icons-material "unfold_less" :face 'warning))
      ('nerd-icons
       (nerd-icons-codicon "nf-cod-fold" :face 'warning))
      (_
       "><"))))

(defun! oo-mode-line-component--pomodoro ()
  "Return indicator for remaining Pomodoro time for work or break."
  (defvar pomodoro-mode-line-string)
  (when (and (bound-and-true-p pomodoro-mode-line-string)
             (not (string-empty-p pomodoro-mode-line-string)))
    (string-match "\\([[:alpha:]]\\)\\([[:digit:]][[:digit:]]:[[:digit:]][[:digit:]]\\)" pomodoro-mode-line-string)
    (set! type (match-string 1 pomodoro-mode-line-string))
    (set! time (match-string 2 pomodoro-mode-line-string))
    (pcase oo-mode-line-icons
      ('all-the-icons
       (pcase type
         ("w" (set! icon (all-the-icons-material "work" :face 'error)))
         ("b" (set! icon (all-the-icons-faicon "coffee" :v-adjust 0))))
       (format "%s %s" icon time))
      ('nerd-icons
       (pcase type
         ("w" (set! icon (nerd-icons-pomicon "nf-pom-pomodoro_ticking" :face 'error :v-adjust 0)))
         ("b" (set! icon (nerd-icons-codicon "nf-cod-coffee" :v-adjust 0))))
       (format "%s %s" icon time))
      (_
       (format "%s %s" type time)))))

(defun! oo-mode-line-component--current-time ()
  "Display the current time."
  (set! time (format-time-string "%H:%M"))
  (set! date (format-time-string "%a %m-%d"))
  (pcase oo-mode-line-icons
    ('nerd-icons
     (require 'nerd-icons)
     (set! dicon (nerd-icons-faicon "nf-fa-calendar"))
     (set! ticon (nerd-icons-wicon (format-time-string "nf-weather-time_%-I")))
     (format "%s %s %s %s" ticon time dicon date))
    ('all-the-icons
     (require 'all-the-icons)
     (set! dicon (all-the-icons-faicon "calendar" :v-adjust 0.01))
     (set! ticon (all-the-icons-wicon (format-time-string "time-%-I") :v-adjust 0.01))
     (format "%s %s %s %s" ticon time dicon date))
    (_
     (format "%s %s" time date))))

(defun! oo-mode-line-component--read-only ()
  "Return indicator for whether file is read-only."
  (when buffer-read-only
    (pcase oo-mode-line-icons
      ('all-the-icons
       (all-the-icons-material "lock" :face 'error))
      ('nerd-icons
       (nerd-icons-faicon "nf-fa-lock" :face 'error))
      (_
       "LOCKED"))))

(defun! oo-mode-line-component--buffer-modified ()
  "Return indicator for buffer modified.
If the current buffer is modified."
  (when (and (buffer-file-name) (buffer-modified-p))
    (pcase oo-mode-line-icons
      ('all-the-icons
       (all-the-icons-material "save" :face 'error))
      ('nerd-icons
       (nerd-icons-faicon "nf-fa-save" :face 'error))
      (_
       (propertize "MODIFIED" 'face 'error)))))

(defun! oo-mode-line-component--evil-state ()
  "Return indicator for evil state."
  (when (bound-and-true-p evil-mode)
    (capitalize (char-to-string (seq-first (symbol-name evil-state))))))

(defun oo-mode-line-component--text-scale ()
  "Indicate whether the text is scaled and by how much."
  (and (boundp 'text-scale-mode-amount)
       (/= text-scale-mode-amount 0)
       (pcase oo-mode-line-icons
         ('all-the-icons
          (all-the-icons-material "save" :face 'error))
         ('nerd-icons
          (cond ((> text-scale-mode-amount 0)
                 (format "%s+%d" (nerd-icons-mdicon "nf-md-magnify_plus") text-scale-mode-amount))
                (t
                 (format "%s%d" (nerd-icons-mdicon "nf-md-magnify_minus") text-scale-mode-amount))))
         (_
          (alet! (if (> text-scale-mode-amount 0) "(%+d)" "(%-d)")
            (propertize (format it text-scale-mode-amount) 'face 'success))))))

(defun oo-first-words (n string)
  "Return the first N words from STRING.
Words are determined by splitting STRING on whitespace."
  (let ((words (split-string string)))
    (string-join (seq-take words n) " ")))

(defun! oo-mode-line-component--clocked-in ()
  "Display the current clocked-in task and the time elapsed since clocking in, with seconds included."
  (when (and (bound-and-true-p org-clock-hd-marker)
             (marker-buffer org-clock-hd-marker))
    (set! elapsed-time (time-subtract (current-time) org-clock-start-time))
    (set! total-seconds (float-time elapsed-time))
    (set! minutes (floor (/ total-seconds 60)))
    (set! seconds (mod (round total-seconds) 60))
    (set! icon (nerd-icons-mdicon "nf-md-clock_in"))
    (with-current-buffer (marker-buffer org-clock-hd-marker)
      (save-excursion
        (goto-char org-clock-hd-marker)
        (set! task-name (org-get-heading t t t t))))
    (set! task-name (oo-first-words 3 task-name))
    (if 1 (format "%s %dm %02ds - %s..." icon minutes seconds task-name)
      (format "CLOCKED-IN %dm %02ds" minutes seconds))))

(defun! oo-mode-line-component--battery ()
  "Return mode line battery indicator."
  (set! status (funcall battery-status-function))
  (set! display-charging-p nil)
  (set! percentage (round (string-to-number (battery-format "%p" status))))
  (set! battery-status (battery-format "%B" status))
  (pcase oo-mode-line-icons
    ('nerd-icons
     (cond ((and (not display-charging-p)
                 (or (equal battery-status "Charging")
                     (and (equal battery-status "Not charging")
                          (> percentage 95))))
            ;; If the battery's good, why display it?
            "")
           ;; There's a distinction between charging, not charging and
           ;; discharging.  When the battery's completely full the computer's
           ;; status becomes "Not charging" even though it is already plugged in.
           ((equal battery-status "Charging")
            (set! name (format "nf-md-battery_charging_%s" (* (/ percentage 10) 10)))
            (set! icon (nerd-icons-mdicon name :face 'success))
            (set! percent-indicator (propertize (concat (number-to-string percentage) "%") 'face 'success))
            (format "%s %s" percent-indicator icon))
           (t
            (set! face (cond ((> percentage 80) 'success) ((> percentage 40) 'warning) (t 'error)))
            (set! name (alet! (* (/ percentage 10) 10)
                         (if (= 100 it)
                             (format "nf-md-battery" it)
                           (format "nf-md-battery_%s" it))))
            (set! icon (nerd-icons-mdicon name))
            (propertize (format "%s %s" (concat (number-to-string percentage) "%") icon) 'face face))))
    ('all-the-icons
     (cond ((> percentage 90)
            (all-the-icons-faicon "battery-full" :v-adjust 0.01))
           ((> percentage 60)
            (format "%s %s" (all-the-icons-faicon "battery-three-quarters") percentage))
           ((> percentage 50)
            (format "%s %s" (all-the-icons-faicon "battery-half") percentage))
           ((> percentage 25)
            (format "%s %s" (all-the-icons-faicon "battery-quarter") percentage))))))

(declare-function emms-track-description "emms")
(declare-function emms-playlist-current-selected-track "emms")
(defun! oo-mode-line-component--emms ()
  "Return indicator for emms.
Returns whether current track is playing."
  (when (bound-and-true-p emms-player-playing-p)
    (set! path (emms-track-description (emms-playlist-current-selected-track)))
    (set! track (file-name-nondirectory (directory-file-name path)))
    (cond ((bound-and-true-p emms-player-paused-p)
           (pcase oo-mode-line-icons
             ('nerd-icons
              (nerd-icons-faicon "nf-fa-circle_pause"))
             ('all-the-icons
              (all-the-icons-faicon "pause-circle"))
             (_
              (format "PAUSED %s" track))))
          ((bound-and-true-p emms-repeat-track)
           (pcase oo-mode-line-icons
             ('nerd-icons
              (nerd-icons-faicon "nf-fa-repeat"))
             ('all-the-icons
              (all-the-icons-faicon "repeat"))
             (_
              (format "REPEAT %s" track))))
          (t
           (pcase oo-mode-line-icons
             ('nerd-icons
              (nerd-icons-faicon "nf-fa-play"))
             ('all-the-icons
              (all-the-icons-material "play-circle"))
             (_
              (format "PLAY %s" track)))))))
;;;; segments
(defun oo-mode-line-segment--evil-state ()
  "Display segment for displaying evil state."
  (oo-mode-line-join-components '(evil-state)))

(defun oo-mode-line-segment--buffer-info ()
  "Display general (usually buffer-related) information."
  (oo-mode-line-join-components '(narrow read-only kbd-macro buffer-modified buffer-name)))

(defun oo-mode-line-segment--version-control ()
  "Display version control information."
  (when (and (buffer-file-name) vc-mode (string-match "Git" vc-mode))
    (oo-mode-line-join-components '(branch git-ahead))))

(defun oo-mode-line-segment--current-time ()
  "Display the current date and time."
  (oo-mode-line-join-components '(current-time)))

(defun oo-mode-line-segment--pomodoro ()
  "Display time elapsed for work or play."
  (oo-mode-line-join-components '(pomodoro)))

(defun oo-mode-line-segment--battery ()
  "Display the battery status."
  (oo-mode-line-join-components '(battery)))

(defun oo-mode-line-segment--buffer-location ()
  "Display the buffer location.
This means the line number and percentage."
  (oo-mode-line-join-components '(line-number percentage-of-buffer)))

(defun oo-mode-line-segment--log-error ()
  (oo-mode-line-join-components '(log-error)))

(defun oo-mode-line-segment--text-scale ()
  (oo-mode-line-join-components '(text-scale)))

(defun oo-mode-line-segment--clocked-in ()
  (oo-mode-line-join-components '(clocked-in)))
;;;; custom modelines
(defvar oo-mode-line-main ""
  "Contain the value of the main modeline.")
(put 'oo-mode-line-main 'risky-local-variable t)

(defun! oo-mode-line-main ()
  "Return my main modeline."
  (set! active (powerline-selected-window-active))
  (set! face1 (if active 'oo-mode-line-segment-1 'powerline-inactive1))
  (set! face2 (if active 'mode-line 'mode-line-inactive))
  (set! face3 (if active 'powerline-active2 'powerline-inactive2))
  (set! fill-face (if active 'powerline-active0 'powerline-inactive0))
  (set! evil-face (spaceline-highlight-face-evil-state))
  (oo-mode-line-render '(evil-state buffer-info version-control)
                       '(text-scale clocked-in pomodoro battery buffer-location current-time)
                       `(,fill-face ,evil-face ,face1 ,face2 ,face3)))
;;;; commands
(defun oo-mode-line-increment-height ()
  "Update the mode line in all buffers to reflect the default `mode-line-format'."
  (interactive)
  (cl-incf powerline-height 2)
  (powerline-reset))

(defun oo-mode-line-decrement-height ()
  "Update the mode line in all buffers to reflect the default `mode-line-format'."
  (interactive)
  (cl-incf powerline-height 2)
  (powerline-reset))

(defun oo-mode-line-update ()
  "Update the mode line in all buffers to reflect the default `mode-line-format'."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (setq-local mode-line-format (default-value 'mode-line-format))))
  (force-mode-line-update))

(defun oo-mode-line-enable ()
  "Enable my custom mode line.
Save the value of `mode-line-format' in a register, and enable my mode line in
all buffers."
  (interactive)
  (set-register :mode-line-format mode-line-format)
  (setq-default mode-line-format '("%e" (:eval (progn (setq-local oo-mode-line-main (oo-mode-line-main)) "")) oo-mode-line-main))
  (oo-mode-line-update))

(defun oo-mode-line-disable ()
  "Disable my custom mode line and restore the previous modeline."
  (interactive)
  (setq-default mode-line-format (get-register :mode-line-format))
  (oo-mode-line-update))

(defun oo-mode-line-cycle-icons (select-p)
  "Cycle the available icon modeline options in `oo-mode-line-icons'.
With prefix argument, SELECT-P, prompt for specific icon type to display."
  (interactive "P")
  (if select-p
      (alet! (list 'all-the-icons 'nerd-icons 'none)
        (completing-read "Choose type of icons: " it))
    (pcase oo-mode-line-icons
      ('all-the-icons
       (require 'all-the-icons)
       (setq oo-mode-line-icons 'nerd-icons))
      ('nerd-icons
       (require 'nerd-icons)
       (setq oo-mode-line-icons nil))
      ('nil
       (setq oo-mode-line-icons 'all-the-icons)))))

(defun! oo-mode-line-cycle-separators (select-p)
  "Cycle through available powerline separators.
With prefix argument, SELECT-P, select one explicitly."
  (interactive "P")
  (set! separators '(alternate
                     arrow
                     arrow-fade
                     bar
                     box
                     brace
                     butt
                     chamfer
                     contour
                     curve
                     rounded
                     roundstub
                     wave
                     zigzag
                     slant
                     utf-8))
  (if select-p
      (awhen! (completing-read "Choose separator: " separators)
        (setq powerline-default-separator (seq-random-elt it))
        (powerline-reset))
    (setq powerline-default-separator (seq-random-elt separators))
    (powerline-reset)))
;;; provide
(provide 'base-mode-line)
;;; base-mode-line.el ends here
