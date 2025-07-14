;;; 123-base-mode-line.el --- My mode line -*- lexical-binding: t; -*-
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
;; My mode line.
;;
;;; Code:
(require '050-base)
(require 'battery)
(require 'seq)
;;;; silence byte-compilation
(defvar evil-state)
(declare-function +evil-state-face "130-init-evil")
;;;; faces
(defface oo-mode-line-segment-1
  '((t (:inherit mode-line :background "blue1" :foreground "white")))
  "Face for the background of the first modeline segment."
  :group 'oo-modeline)

(setf (alist-get 'oo-mode-line-segment-1 oo-custom-faces-alist) 'font-lock-function-name-face)

(defface oo-mode-line-segment-2
  '((t (:inherit mode-line :background "#008b8b" :foreground "white")))
  "Face for the background of the second modeline segment."
  :group 'oo-modeline)

(setf (alist-get 'oo-mode-line-segment-2 oo-custom-faces-alist) 'font-lock-constant-face)

(defface oo-mode-line-segment-3
  '((t (:inherit mode-line :background "#a0522d")))
  "Face for the background of the third modeline segment."
  :group 'oo-modeline)

(setf (alist-get 'oo-mode-line-segment-3 oo-custom-faces-alist) 'font-lock-variable-name-face)

(defface oo-mode-line-segment-4
  '((t (:inherit mode-line :background "#b22222")))
  "Face for the background of the fourth modeline segment."
  :group 'oo-modeline)

(setf (alist-get 'oo-mode-line-segment-4 oo-custom-faces-alist) 'font-lock-comment-face)

(defface oo-mode-line-segment-5
  '((t (:inherit mode-line :background "#228b22")))
  "Face for the background of the fifth modeline segment."
  :group 'oo-modeline)

(setf (alist-get 'oo-mode-line-segment-5 oo-custom-faces-alist) 'font-lock-type-face)
;;;; utility functions
;; When you modify the modeline variable the modeline is not automatically
;; updated.  You only see the updated version when you open a new buffer.  To
;; actually see the updated modeline in buffers that are already open you need
;; to change their buffer-local mode-line-format variable and then call
;; `force-mode-line-update' after you make the change to the default value of
;; `mode-line-format'.
(defun oo-cycle (list)
  "Return an infinite circular copy of LIST.
The returned list cycles through the elements of LIST and repeats
from the beginning."
  (declare (pure t) (side-effect-free t))
  ;; Also works with sequences that aren't lists.
  (let ((newlist (append list ())))
    (nconc newlist newlist)))

(defun! oo-mode-line-render-segments (segments)
  "Render the segment."
  (flet! full-p (segment) (and segment (not (string-empty-p segment))))
  (string-join (cl-remove-if-not #'full-p (mapcar #'oo-mode-line-render-segment segments)) "\s"))

(defun! oo-mode-line-render-side (side)
  "Render the segment."
  (flet! full-p (segment) (and segment (not (string-empty-p segment))))
  (format "\s%s\s" (string-join (cl-remove-if-not #'full-p (mapcar #'oo-mode-line-render-segment side)) "\s\s")))

(defun! oo-mode-line-render (left right fill-face)
  "Render mode line with LEFT and RIGHT, joining them with FILL-FACE."
  (set! lhs-str left)
  (set! rhs-str right)
  (set! rhs (string-pixel-width rhs-str))
  (set! lhs (string-pixel-width lhs-str))
  (set! window (window-pixel-width))
  (set! scroll-bar (window-scroll-bar-width))
  (set! right-divider (window-right-divider-width))
  (set! right-fringe (frame-parameter nil 'right-fringe))
  ;; I do one extra pixel to cover the 1 pixel wide space at the end.
  ;; Also it is debatable to make the modeline smaller for the window
  ;; divider, in my opinion it looks better covering it and the mode line
  ;; is not big enough to make using the scroll bar difficult.
  ;; I do not know what this 4 pixels is for but that is what it takes to
  ;; balance my mode line.
  (set! mid (1+ (- window lhs rhs right-divider scroll-bar right-fringe)))
  ;; Doom has this margin but I tell you it was just messing me up.  The
  ;; modeline fits perfectly.
  ;; (margin (* (or (cdr (window-margins)) 1) (frame-char-width)))
  ;; (margin (+ 40 (* (or (cdr (window-margins)) 1) (frame-char-width))))
  ;; (margin 0)
  (set! fill (propertize "\s" 'face (or fill-face 'mode-line) 'display `(space :align-to (,(+ lhs mid)))))
  (concat lhs-str fill rhs-str))

(defun! oo-mode-line-render-segment (name)
  (funcall (intern (format "oo-mode-line-segment--%s" name))))
;;;; segments
(defun oo-mode-line-segment--line-number ()
  "Return the line-number indicator for the mode line."
  (format-mode-line "%l"))

(defun! oo-mode-line-segment--percentage-of-buffer ()
  "Return the percentage of the buffer indicator for the mode line."
  (set! percentage (* 100 (/ (float (point)) (point-max))))
  (cond ((> percentage 95) "BOT")
        ((< percentage 5) "TOP")
        (t (concat (number-to-string (round percentage)) "%"))))

(defun oo-mode-line-segment--buffer-name ()
  "Return the current buffer name indicator for the mode line."
  (buffer-name))

(defun oo-mode-line-segment--kbd-macro ()
  "Return an indicator for keyboard macro recording or playback."
  (or (and defining-kbd-macro "•REC")
      (and executing-kbd-macro "KBD-PLAY")))

(defun oo-mode-line-segment--abbrevs ()
  "Indicate how many abbrevs I have in `999-abbrevs.el'."
  (when (equal (buffer-file-name)
               (expand-file-name "999-abbrevs.el" oo-lisp-dir))
    (format "%d abbrevs" (how-many "^(define-abbrev" (point-min) (point-max)))))

(defun oo-mode-line-segment--word-count ()
  "Indicate how many words I have in a text buffer."
  (when (derived-mode-p 'text-mode)
    (format "%d words" (count-words (point-min) (point-max)))))

(defun oo-mode-line-segment--tab ()
  "Return an indicator for a `tab-bar-mode' tab."
  ;; This really convoluted way for just getting the current tab.  But I cannot
  ;; immediately see a simpler way based on the source code.
  (and (bound-and-true-p tab-bar-mode)
       (let* ((tabs (funcall tab-bar-tabs-function))
              (tab-number (1+ (tab-bar--current-tab-index tabs)))
              (tab-index (if (integerp tab-number)
                             (1- (max 0 (min tab-number (length tabs))))
                           (tab-bar--current-tab-index tabs)))
              (current-tab (nth tab-index tabs)))
         (alist-get 'name current-tab))))

(defun! oo-mode-line-segment--branch ()
  "Return the branch name of the current repository."
  (and vc-mode (cadr (split-string (string-trim vc-mode) "^[A-Z]+[-:]+"))))

(defun oo-mode-line-segment--version-control ()
  "Return a segment composed of branch and git-ahead segments"
  (oo-mode-line-render-segments '(branch git-ahead)))

(defun! oo-mode-line-segment--git-ahead ()
  "Display the number of commits ahead of origin.
If 0, do not display anything."
  (set! count (string-to-number (shell-command-to-string "git rev-list --count @{upstream}..HEAD")))
  (when (> count 0)
    (format "%s@" count)))

(declare-function fancy-narrow-active-p "fancy-narrow")
(defun! oo-mode-line-segment--narrow ()
  "Return an indicator for a narrowed buffer in the modeline."
  (when (or (buffer-narrowed-p)
            (and (bound-and-true-p fancy-narrow-mode)
                 (fancy-narrow-active-p))
            (bound-and-true-p dired-narrow-mode))
    "><"))

(defun! oo-mode-line-segment--pomodoro ()
  "Return indicator for remaining Pomodoro time for work or break."
  (defvar pomodoro-mode-line-string)
  (when (and (bound-and-true-p pomodoro-mode-line-string)
             (not (string-empty-p pomodoro-mode-line-string)))
    (string-match "\\([[:alpha:]]\\)\\([[:digit:]][[:digit:]]:[[:digit:]][[:digit:]]\\)" pomodoro-mode-line-string)
    (set! type (match-string 1 pomodoro-mode-line-string))
    (set! time (match-string 2 pomodoro-mode-line-string))
    (format "%s%s" type time)))

(defun! oo-mode-line-segment--time ()
  "Display the current time."
  (format-time-string "%H:%M"))

(defun! oo-mode-line-segment--date ()
  "Display the current date."
  (format-time-string "%a %m-%d"))

(defun! oo-mode-line-segment--read-only ()
  "Return indicator for whether file is read-only."
  (and buffer-read-only "LOCKED"))

(defun! oo-mode-line-segment--buffer-modified ()
  "Return indicator for buffer modified.
If the current buffer is modified."
  (when (and (buffer-file-name) (buffer-modified-p))
    (propertize "MODIFIED" 'face 'error)))

(defun! oo-mode-line-segment--evil-state ()
  "Return indicator for evil state."
  (when (bound-and-true-p evil-mode)
    (capitalize (char-to-string (seq-first (symbol-name evil-state))))))

(defvar text-scale-mode-amount)
(defun oo-mode-line-segment--text-scale ()
  "Return an indicator for text scaling."
  (and! (boundp 'text-scale-mode-amount)
        (/= text-scale-mode-amount 0)
        (if (> text-scale-mode-amount 0) "(%+d)" "(%-d)")
        (format it text-scale-mode-amount)))

(defun oo-mode-line-segment--buffer-info ()
  "Return an indicator for various buffer information."
  (oo-mode-line-render-segments '(narrow read-only kbd-macro buffer-modified buffer-name)))

(defun oo-mode-line-segment--buffer-location ()
  "Display the line number and percentage of buffer."
  (oo-mode-line-render-segments '(line-number percentage-of-buffer)))

(defun oo-mode-line-segment--time-info ()
  "Display the time and date in mode line."
  (oo-mode-line-render-segments '(time date)))

(defvar emms-playing-time-string)
(declare-function emms-track-description "emms")
(declare-function emms-playlist-current-selected-track "emms")
(defun! oo-mode-line-segment--emms ()
  "Return indicator for playing a track.

This indicator will have an indicator of whether the track is paused or playing
or playing with repeat."
  (when (bound-and-true-p emms-player-playing-p)
    (set! path (emms-track-description (emms-playlist-current-selected-track)))
    (set! title (file-name-nondirectory (directory-file-name path)))
    (set! track (truncate-string-to-width title 20 nil nil 'ellipsis))
    (set! playtime (propertize (string-trim emms-playing-time-string) 'face 'success))
    (cond ((bound-and-true-p emms-player-paused-p)
           (set! indicator (propertize "PAUSED" 'face 'error))
           (format "%s %s" indicator track))
          ((bound-and-true-p emms-repeat-track)
           (set! indicator "REPEAT")
           (format "%s %s %s" indicator playtime track))
          (t
           (set! indicator (propertize "PLAYING" 'face 'success))
           (format "%s %s %s" indicator playtime track)))))
;;;; custom modelines
(defvar oo-mode-line-main ""
  "Contain the value of the main mode line.")
(put 'oo-mode-line-main 'risky-local-variable t)

;; Right now this assumes evil-mode is enabled.
(defun! oo-mode-line-main ()
  "Return the main mode line."
  (set! faces '(oo-mode-line-segment-1 oo-mode-line-segment-2 oo-mode-line-segment-3 oo-mode-line-segment-4))
  (flet! add-face (face segment) (add-face-text-property 0 (length segment) face t segment) segment)
  (flet! empty-p (segment) (not (and segment (not (string-empty-p segment)))))
  (flet! pad (segment) (format "\s%s\s" segment))
  (flet! render (side) (mapcar #'pad (cl-remove-if #'empty-p (mapcar #'oo-mode-line-render-segment side))))
  (set! lhs (render '(evil-state tab buffer-info version-control emms)))
  (set! rhs (render '(time-info buffer-location pomodoro word-count abbrevs text-scale)))
  ;; Now apply the faces.  This is kind of messy.
  (set! evil-state-face (+evil-state-face))
  (set! lhs-head (add-face evil-state-face (car lhs)))
  (set! rhs-head (add-face evil-state-face (car rhs)))
  ;; Now apply the rest of the faces for the other segments which should be more regular.
  (set! cycle (oo-cycle faces))
  (dolist (segment (cdr lhs))
    (collecting! lhs-tail (add-face (pop cycle) segment)))
  (set! cycle (oo-cycle faces))
  (dolist (segment (cdr rhs))
    (collecting! rhs-tail (add-face (pop cycle) segment)))
  (set! lhs (apply #'concat (cons lhs-head lhs-tail)))
  (set! rhs (apply #'concat (reverse (cons rhs-head rhs-tail))))
  (set! fill-face 'mode-line)
  (oo-mode-line-render lhs rhs fill-face))
;;;; commands
(defun oo-mode-line-update ()
  "Update the mode line in all buffers to reflect the default `mode-line-format'."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (setq-local mode-line-format (default-value 'mode-line-format))))
  (force-mode-line-update))

(defvar oo-old-mode-line-format nil
  "This is where to store the existing mode line.")

(define-minor-mode oo-mode-line-mode
  "Display icons in the mode line."
  :global t
  (cond (oo-mode-line-mode
         (setq oo-old-mode-line-format mode-line-format)
         (setq-default mode-line-format '("%e" (:eval (progn (setq-local oo-mode-line-main (oo-mode-line-main)) "")) oo-mode-line-main))
         (oo-mode-line-update))
        (t
         (setq-default mode-line-format oo-old-mode-line-format)
         (oo-mode-line-update))))
;;; provide
(provide '123-base-mode-line)
;;; 123-base-mode-line.el ends here
