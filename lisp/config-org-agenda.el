;;; config-org-agenda.el --- Configure `org-agenda' -*- lexical-binding: t; -*-
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
;; Configure `org-agenda'.
;;
;;; Code:
;;;; requirements
(require 'org-agenda)
(require 'ts)
;;;; settings
(setq org-agenda-files `(,org-directory))
(setq org-agenda-span 'week)
(setq org-agenda-start-on-weekday 1)
(setq org-agenda-confirm-kill t)
(setq org-agenda-show-all-dates t)
(setq org-agenda-show-outline-path nil)
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-prefix-format
      '((agenda . " %i %?-12t")
        ;; Remove the category displayed on the left of entries.
        ;; (todo   . " %(+org-agenda--overdue-string)")
        (tags   . " ")
        (search . " %i %-12:c")))

;; Always show the time grid even if I do not have anything scheduled or any
;; deadlines.
;; Also, increase the sequence of numbers shown on the grid.
(setq org-agenda-time-grid `((daily today)
                             ,(number-sequence 300 2300 200)
                             "-------------- "
                             "---------------"))

;; Do not use a special character for the block separator.  I like it to look
;; old-fashionish.
(setq org-agenda-block-separator ?-)
(setq org-agenda-skip-deadline-if-done t)
;;;; sorting entries
;;;;; helpers
(defun +org-agenda-call-at-entry (entry fn)
  "Call function from entry."
  (set! marker (get-text-property 0 'org-marker entry))
  (unless marker (error "Entry: %S" entry))
  (with-current-buffer (marker-buffer marker)
    (goto-char (marker-position marker))
    (funcall fn)))

(defmacro org-with-entry! (entry &rest body)
  (declare (indent 1))
  `(org-with-point-at (get-text-property 0 'org-hd-marker ,entry)
     (progn ,@body)))
;;;;; comparators
;; These are comparators I have written to aid me with sorting entries.
;;;;;; effort
(defun +org-agenda-effort-comparator (a b)
  "Return 1 if A requires less effort than B.
If B requires more effort than A, return -1.  Otherwise, return 0."
  (* -1 (or (org-cmp-effort a b) 0)))
;;;;;; priority comparator
(defun! +org-agenda-priority-comparator (a b)
  "Return 1 if priority A is greater than priority B.
Return -1 if priority B is greater than priority A.  Otherwise, if return 0."
  (or (org-cmp-values a b 'priority) 0))
;;;;;; tag comparator
(defun! +org-agenda-tag-comparator (a b)
  "Compare two entries A and B based on their tags."
  (set! tag-weights '(("job" . 2) ("blog" . 1) ("emacs" . 0)))
  (flet! weight (tag)
    (alist-get (substring-no-properties tag) tag-weights 0 nil #'equal))
  (set! weight-a (apply #'+ (mapcar #'weight (get-text-property 0 'tags a))))
  (set! weight-b (apply #'+ (mapcar #'weight (get-text-property 0 'tags b))))
  (cond ((> weight-a weight-b) 1)
        ((< weight-a weight-b) -1)
        (t 0)))
;;;;;; schedule comparator
;;;;;; overdue deadline comparator
(defun! +org-agenda-overdue-deadline-comparator (a b)
  "Return 1 if A is more overdue than B.
Return -1 if B is more overdue than A.  Otherwise return 0."
  (set! da (org-with-entry! a (org-get-deadline-time (point))))
  (set! db (org-with-entry! b (org-get-deadline-time (point))))
  (set! now (current-time))
  (set! diff-a (and da (float-time (time-subtract da now))))
  (set! diff-b (and db (float-time (time-subtract db now))))
  (cond ((or (and (not diff-a) (not diff-b))
             (and diff-a (oo-positive-p diff-a) (not diff-b))
             (and (not diff-a) diff-b (oo-positive-p diff-b))
             (and diff-a diff-b (oo-positive-p diff-a) (oo-positive-p diff-b))
             (and diff-a diff-b (= diff-a diff-b)))
         0)
        ((and diff-a (oo-negative-p diff-a) (not diff-b))
         1)
        ((and (not diff-a) diff-b (oo-negative-p diff-b))
         -1)
        ((and (oo-negative-p diff-a) (oo-positive-p diff-b))
         1)
        ((and (oo-positive-p diff-a) (oo-negative-p diff-b))
         -1)
        ((and (oo-negative-p diff-a) (oo-negative-p diff-b) (/= diff-a diff-b))
         (if (> diff-a diff-b) 1 -1))))
;;;;;; deadline comparator
(defun +org-agenda-has-deadline-comparator (a b)
  (set! da (org-with-entry! a (org-get-deadline-time (point))))
  (set! db (org-with-entry! b (org-get-deadline-time (point))))
  (cond ((and da (not db)) 1)
        ((and (not da) db) -1)
        (t 0)))
;;;;;; closest deadline comparator
(defun +org-agenda-closest-deadline-comparator (a b)
  "Prioritize entries with the closest non-overdue deadline.
This assumes that an entry with a non-overdue deadline is always closer than one
with no deadline."
  (set! da (org-with-entry! a (org-get-deadline-time (point))))
  (set! db (org-with-entry! b (org-get-deadline-time (point))))
  (set! now (current-time))
  (set! diff-a (and da (float-time (time-subtract da now))))
  (set! diff-b (and db (float-time (time-subtract db now))))
  (cond ((and diff-a (oo-positive-p diff-a) (not diff-b))
         1)
        ((and diff-b (oo-positive-p diff-b) (not diff-a))
         -1)
        ((and diff-a diff-b (oo-positive-p diff-a) (oo-positive-p diff-b))
         (if (< diff-a diff-b) 1 (if (> diff-a diff-b) -1 0)))
        ((and diff-a (oo-positive-p diff-a) diff-b (oo-negative-p diff-b))
         1)
        ((and diff-b (oo-positive-p diff-b) diff-a (oo-negative-p diff-a))
         -1)
        (t
         0)))
;;;;;; timestamp ID comparator
;; I am ignoring the microseconds.  I should not be making capture templates
;; within microseconds of each other.  I got this function from chatgpt.
(defun +org-id-to-time (org-id)
  "Convert an Org ID string into an Emacs time object.
ORG-ID should be in the format 'YYYYMMDDTHHMMSS.SSSSSS'."
  (let* ((date-str (substring org-id 0 8))
         (time-str (substring org-id 9 15))
         (microseconds-str (substring org-id 16))
         (year (string-to-number (substring date-str 0 4)))
         (month (string-to-number (substring date-str 4 6)))
         (day (string-to-number (substring date-str 6 8)))
         (hour (string-to-number (substring time-str 0 2)))
         (minute (string-to-number (substring time-str 2 4)))
         (second (string-to-number (substring time-str 4 6))))
    (encode-time second minute hour day month year)))

;; The sort function accepts two entries and by entries the manual means
;; propertized strings.  These strings have references to the headline it refers to.
(defun! +org-agenda-tsid-comparator (a b)
  "Compare two entries A and B based on their ID property to sort by oldest first."
  (if-let* ((time-a (org-with-entry! a (org-id-get)))
            (time-b (org-with-entry! b (org-id-get)))
            (id-a (+org-id-to-time time-a))
            (id-b (+org-id-to-time time-b)))
      (if (time-less-p id-a id-b) 1 -1)
    0))
;;;;;; STARTED comparator
;; I should prefer entries that have already been started.
(defun! +org-agenda-started-comparator (a b)
  "Prefer entries that have a \"STARTED\" TODO keyword."
  (flet! started-or-not (entry)
    (if (equal "STARTED" (org-with-entry! entry (org-get-todo-state))) "STARTED" ""))
  (pcase (mapcar #'started-or-not (list a b))
    (`("" "") 0)
    (`("STARTED" "") 1)
    (`("" "STARTED") -1)
    (`("STARTED" "STARTED") 0)))
;;;;; replace `org-agenda-sorting-strategy'
;; The mechanism for adding your own sorting to org-agenda provided via
;; `org-agenda-sorting-strategy' assumes the user would only ever want to add
;; just one additional sorting strategy.  It is design makes it inconvenient to
;; add more sorters.  I have decided to scrap the default sorters and use my
;; own.
(defun! +org-agenda-main-comparator (a b)
  "Return whether entry A should be ordered before entry B."
  (set! comparators +org-agenda-comparators)
  (while comparators
    (set! comparator (pop comparators))
    (set! result (funcall comparator a b))
    (unless (zerop result)
      (return! result)))
  0)

(setq org-agenda-cmp-user-defined #'+org-agenda-main-comparator)

(defvar +org-agenda-comparators nil
  "Comparators used for sorting org agenda.
This is a more flexible replacement for `org-agenda-sorting-strategy'.")

(setq +org-agenda-comparators '(
                                ;; +org-agenda-overdue-deadline-comparator
                                +org-agenda-priority-comparator
                                +org-agenda-closest-deadline-comparator
                                +org-agenda-tag-comparator
                                +org-agenda-effort-comparator
                                +org-agenda-tsid-comparator))
;;;;; dealing with composite tasks
;; Composite tasks are entries that contain one or more subtasks.  These are
;; created when.  They have certain props.

(defun! +org-has-tasks-to-be-done-p ()
  "Return non-nil if current headline has any subtasks that need to be done."
  (interactive)
  (flet! not-done-p ()
    (aand! (substring-no-properties (org-get-todo-state))
          (not (member it '("DONE" "CANCELLED")))))
  (save-excursion
    (when (org-goto-first-child)
      (when (not-done-p)
        (return! t))
      (while (org-goto-sibling)
        (when (not-done-p)
          (return! t))))))

(defun +org-agenda--filter-parents-with-undone-children (entry)
  (when (not (org-with-entry! entry (+org-has-tasks-to-be-done-p)))
    entry))
;;;;;; Update agenda after certain actions
(defun oo--update-agenda (orig-fn &rest args)
  (prog1 (apply orig-fn args)
    (quietly! (call-interactively #'org-agenda-redo))))

(advice-add 'org-agenda-todo :around #'oo--update-agenda)
;;;; miscellaneous
;; A task is overdue if the deadline of the task is past the current time.
(defun +org-overdue-p ()
  "Return non-nil if entry is overdue."
  (aand! (org-get-deadline-time (point))
        (< (float-time (time-subtract it (current-time))) 0)
        (not (org-entry-is-done-p))))

(defun +org-agenda--agenda-filter (entry)
  "Do not show overdue or done entries."
  (if (ignore-errors (org-with-entry! entry
                       (or (+org-overdue-p)
                           (org-entry-is-done-p))))
      nil
    entry))

;; If the difference in dates is less than 7 days, display the days left until
;; entry is due.
(defun! +org-agenda--deadline-string (deadline-time)
  "Return string describing the deadline of entry."
  (set! seconds (- (float-time deadline-time) (float-time (current-time))))
  (set! seconds-in-day 86400)
  (set! seconds-in-week (* 7 seconds-in-day))
  (set! seconds-in-hour 3600)
  (cond ((>= seconds seconds-in-week)
         "")
        ((> seconds (* 2 seconds-in-day))
         (set! days (/ seconds seconds-in-day))
         (format "due in %d days " days))
        ((> seconds seconds-in-day)
         (format "due in 1 day "))
        ((> seconds (* 2 seconds-in-hour))
         (set! hours (round (/ seconds seconds-in-hour)))
         (format "due in %d hours " hours))
        ((> seconds seconds-in-hour)
         (set! minutes (round (/ seconds 60.0)))
         (format "due in 1 hour"))
        ((> seconds (* 2 60))
         (set! minutes (round (/ seconds 60.0)))
         (format "due in %d minutes " minutes))
        ((> seconds 60)
         (set! minutes (round (/ seconds 60.0)))
         (format "due in 1 minute " minutes (* 100 (1- minutes))))
        ((> seconds 0)
         (format "due %d seconds " seconds))
        ((> (abs seconds) (* 2 seconds-in-week))
         (set! weeks (round (/ (abs seconds) seconds-in-week)))
         (format "%d weeks overdue " weeks))
        ((> (abs seconds) seconds-in-week)
         "1 week overdue ")
        ((> (abs seconds) (* 2 seconds-in-day))
         (set! days (round (/ (abs seconds) seconds-in-day)))
         (format "%d days overdue " days))
        ((> (abs seconds) seconds-in-day)
         "1 day overdue ")
        ((> (abs seconds) (* 2 seconds-in-hour))
         (set! hours (round (abs seconds) seconds-in-hour))
         (format "%d hours overdue " hours))
        ((> (abs seconds) seconds-in-hour)
         "1 hour overdue ")
        ((> (abs seconds) (* 2 60))
         (set! minutes (/ (abs seconds) 60))
         (format "%d minutes overdue " minutes))
        ((> (abs seconds) 60)
         "1 minute overdue ")
        ((> (abs seconds) 0)
         (format "%s seconds overdue " (round (abs seconds))))))

(defun! +org-agenda-deadline-string ()
  "Return string indicating deadline status."
  (aif! (org-get-deadline-time (point))
      (+org-agenda--deadline-string it)
    ""))
;;;; views
;; The main view is the day view.
;; Overdue items are first, then items with high priority, then items with low
;; effort, and then items that were created first.

;; effort-up -> prioritize headlines that have an effort from "easiest" to
;; "hardest"--or lowest effort to highest.
;; priority-down -> order headlines from the highest priority to the lowest
(defun +org-agenda-day-view ()
  "Day agenda."
  (interactive)
  (let ((org-agenda-custom-commands
         `(("_" "Daily Agenda"
            ((tags "TODO=\"STARTED\"|TODO=\"TODO\""
                   ((org-agenda-overriding-header "\nTODO")
                    (org-agenda-sorting-strategy '(user-defined-down))
                    ;; (org-agenda-before-sorting-filter-function #'+org-agenda--filter-parents-with-undone-children)
                    (org-agenda-max-entries 5)))
             (todo "ON-HOLD" ((org-agenda-overriding-header "\nON-HOLD")))
             (agenda "" ((org-agenda-overriding-header "\nSchedule")
                         (org-agenda-start-on-weekday nil)
                         ;; Do not show overdue items in agenda.  Overdue items
                         ;; are displaced awkwardly at the end of the agenda which
                         ;; is confusing because I am thinking they are at the
                         ;; end of the day.  ID rather have them in the TODO
                         ;; section or their own dedicated section.
                         (org-agenda-before-sorting-filter-function #'+org-agenda--agenda-filter)
                         (org-scheduled-past-days 0)
                         (org-deadline-warning-days 0)
                         (org-agenda-span 1)))
             (todo "DONE" ((org-agenda-files (list (expand-file-name "archive.org" org-directory)))
                           (org-agenda-overriding-header "\nDone")
                           (org-agenda-max-entries 5))))))))
    (org-agenda nil "_")))
;;; provide
(provide 'config-org-agenda)
;;; config-org-agenda.el ends here
