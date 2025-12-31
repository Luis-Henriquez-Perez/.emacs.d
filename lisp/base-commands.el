;;; base-commands.el --- Generic commands -*- lexical-binding: t; -*-
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
;; This file contains non-package specific commands that I use generally.  Some
;; of these commands will not be perfect in that they are specific to me instead
;; of generalized polished commands you might see in packages.  Instead,
;; these functions are very specific to me and my workflow.
;;
;;; Code:
(require 'base-lib)
;; (require 'f)
;; (require 'ctable)
;;;; opening specific files
(defun o-open-emacs-config ()
  "Open Emacs configuration."
  (interactive)
  (switch-to-buffer (dired user-emacs-directory)))

(defun o-open-emacs-init-file ()
  "Open init file."
  (interactive)
  (switch-to-buffer (find-file-noselect user-init-file)))

(defun o-open-emacs-lisp-dir ()
  "Open lisp directory."
  (interactive)
  (switch-to-buffer (dired (expand-file-name "lisp" user-emacs-directory))))
;;;; window splitting
(defun o-split-window-right-and-focus ()
  "Split window right and select the window created with the split."
  (interactive)
  (select-window (split-window-right)))

(defun o-split-window-below-and-focus ()
  "Split window below and select the window created with the split."
  (interactive)
  (select-window (split-window-below)))
;;;; font
(o-defun o-set-font-face ()
  "Apply an existing xfont to all graphical frames."
  (interactive)
  (o-set font (completing-read "Choose font: " (x-list-fonts "*")))
  (set-frame-font font nil t))
;;;; sorting
;; This is meant to sort the great number of install package forms I have in
;; `init-elpaca'.
(o-defun o-sort-elpaca-forms (beg end)
  "Sort elpaca forms lexicographically by package name."
  (o-set rx "^\\(?:;; \\)?(elpaca \\(?:(\\(?1:\\(?:[[:alnum:]]\\|-\\)+\\)\\|\\(?1:\\(?:[[:alnum:]]\\|-\\)+\\)\\)[^z-a]+?$")
  (save-excursion (sort-regexp-fields nil rx "\\1" beg end)))

(o-defun o-sort-autoload-forms (beg end)
  "Sort autoload forms lexicographically by package name."
  (o-set rx "(autoload[[:blank:]]+#'[^[:space:]]+[[:blank:]]+\"\\(.+?\\)\".+?$")
  (save-excursion (sort-regexp-fields nil rx "\\1" beg end)))

;; This is meant to sort the great number of `require' forms in the init file.
(o-defun o-sort-require-forms (beg end)
  "Sort require forms lexicographically by feature name."
  (o-set rx "(require[[:blank:]]+'\\(.+\\))")
  (save-excursion (sort-regexp-fields nil rx "\\1" beg end)))

(o-defun o-sort-dwim (beg end)
  "Sort lines the way I like it."
  (interactive
   (cond ((region-active-p)
	      (list (region-beginning) (region-end)))
	     ((save-excursion (o-aand "(\\(\\(?:autoload\\|elpaca\\|require\\)\\)"
                                 (re-search-forward it (point-max) t nil)))
          (list (match-beginning 0) (point-max)))
         (t
          (list nil nil))))
  (save-excursion
    (goto-char beg)
    (pcase (match-string 1)
	  ("autoload" (o-sort-autoload-forms beg end))
	  ("require" (o-sort-require-forms beg end))
	  ("elpaca" (o-sort-elpaca-forms beg end))
	  (_ (error "No sorting method detected")))))
;;;; miscellaneous
(declare-function org-narrow-to-block "org")
(declare-function org-narrow-to-subtree "org")
(declare-function outli-toggle-narrow-to-subtree "org")
(defun o-dwim-narrow (keep-narrowing-p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: narrow to region, outline heading, org-src-block, org-subtree, or
defun, whichever applies first.

With prefix KEEP-NARROWING-P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (cond ((and (buffer-narrowed-p) (not keep-narrowing-p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((equal 'comment (o-in-string-or-comment-p))
         (save-excursion (outli-toggle-narrow-to-subtree)))
        ((derived-mode-p 'org-mode)
         (or (ignore-errors (org-narrow-to-block) t)
             (org-narrow-to-subtree)))
        (t
         (narrow-to-defun))))
;; You could actually do this via abbrev-mode as well.  And actually it might be
;; better in a sense because.
(o-defun o-dwim-space ()
  "Replace two consecutive spaces with a period."
  (interactive)
  (cond ((and (or (derived-mode-p 'text-mode)
                  (o-in-string-or-comment-p))
              (looking-back "\\([[:word:]]\\)[[:space:]]\\{2,\\}" nil))
         (replace-match "\\1.\s\s"))
        (t
         (insert "\s"))))

(declare-function consult-buffer "consult")
(defvar consult--buffer-display)
(o-defun o-pop-to-buffer ()
  (interactive)
  (require 'consult)
  (o-set consult--buffer-display #'pop-to-buffer)
  (call-interactively #'consult-buffer))

(defun o-kill-emacs-no-confirm ()
  "Kill Emacs without confirmation."
  (let (confirm-kill-emacs)
    (call-interactively #'kill-emacs)))

;; Keep track of the themes that I have loaded and do not allow repetitions.
(defvar o-loaded-themes nil
  "Themes that have already been loaded.")

(o-defun o-load-random-theme ()
  "Load a random theme."
  (interactive)
  (o-set not-loaded (cl-set-difference (custom-available-themes) o-loaded-themes))
  (o-set theme (seq-random-elt not-loaded))
  (message "Loading theme `%s'..." theme)
  (load-theme theme 'noconfirm)
  (push theme o-loaded-themes))

;; This idea is based on the following link where xah lee talks about why the
;; scratch buffer is outdated.  It does not follow the trend of "untitled1",
;; "untitled2" as xah lee recommended because it is just easier and more
;; consistent to use Emacs's buffer naming style.
;; http://xahlee.info/emacs/emacs/modernization_scratch_buffer.html
(o-defun o-new-buffer ()
  "Create a new blank buffer."
  (interactive)
  (display-buffer (generate-new-buffer "untitled")))

(declare-function vc-git--pushpull "vc-git")
(o-defun o-dwim-vc-push ()
  (interactive)
  (o-pushing display-buffer-alist '("\\*vc-git"
                                   (display-buffer-no-window)
                                   (allow-no-window . t)))
  (vc-git--pushpull "push" nil (list "--force")))


(declare-function vc-checkin "vc")
(declare-function vc-deduce-fileset "vc")

(defalias 'eshell/dotadd 'o-dwim-vc-action)
(o-defun o-dwim-vc-action (file)
  "Register, stage, commit and push FILE to dotfiles repository.
If FILE is not in registered in dotfile repo, register it.  In any case commit
the file.  Additionally, push the file but only if the battery is charging or

the battery percentage is greater than 90%."
  (interactive (list (or (buffer-file-name)
                         (read-file-name "Select file to add to dofiles:"))))
  (o-set backend (car (vc-deduce-fileset nil t 'state-model-only-files)))
  (o-set root (vc-root-dir))
  (o-set commit-msg (format "%s" (f-relative file root)))
  ;; Adding the log-edit-files to the display-buffer-alist and even nopping
  ;; display-buffer does not work.  I have to actually nope the function
  ;; `log-edit-show-files'.
  ;; https://mail.gnu.org/archive/html/help-gnu-emacs/2021-02/msg00197.html
  ;; (noflet! display-buffer #'ignore)
  ;; ("\\*log-edit-files\\*"
  ;;  (display-buffer-no-window)
  ;;  (allow-no-window . t))
  ;; (noflet! pop-to-buffer #'ignore)
  (noflet! log-edit-show-files #'ignore)
  (o-set display-buffer-alist `(("\\*vc-git.+\\*"
                                (display-buffer-no-window)
                                (allow-no-window . t))
                               ,@display-buffer-alist))
  (save-buffer)
  ;; Make sure functions used by `vc-check-in' are defined.
  (require 'log-edit)
  (pcase (vc-state file)
    ('edited
     (vc-checkin (list file) backend commit-msg)
     (o-dwim-vc-push))
    ('nil
     (vc-register)
     (vc-checkin (list file) backend commit-msg)
     (o-dwim-vc-push))
    (_
     nil)))

(o-defun o-one-line (beg end)
  "Join lines in the region between BEG and END into a single line.
Additionally, make any duplicate spaces in line become a single space."
  (interactive "r")
  (replace-string-in-region "\n" "\s" beg end))

(o-defun o-remove-consequtive-spaces (beg end)
  "Replace consequtive spaces in region with a single space."
  (interactive "r")
  (replace-regexp-in-region "[[:space:]]\\{2,\\}" "\s" beg end))

(o-defun o-startup-time-table ()
  "Produce a table that shows the time taken by each feature during startup."
  (interactive)

  (require 'ctable)

  (pcase-dolist (`(,feature ,time) (get-register :require-times))
    (o-collecting new (list feature time))
    (o-summing total time))

  (o-set init-time (string-to-number (emacs-init-time "%.2f")))

  (o-flet percent (time total) (format "%3d%%" (* 100 (/ time total))))

  (pcase-dolist (`(,feature ,time) new)
    (o-set dtime (format "%.2f" (/ (fround (* time 100)) 100.0)))
    (o-pushing data (list feature dtime (percent time total) (percent time init-time))))

  (o-set data (sort data (-on #'> (-compose #'string-to-number #'cl-second))))

  (o-set column-model (list (make-ctbl:cmodel :title "Feature" :align 'left)
                            (make-ctbl:cmodel :title "Time (s)" :align 'center)
                            (make-ctbl:cmodel :title "% of Total" :align 'center)
                            (make-ctbl:cmodel :title "% of Init" :align 'center)))
  (o-set model (make-ctbl:model :column-model column-model :data data))
  (o-set component (ctbl:create-table-component-buffer :model model))
  (pop-to-buffer (ctbl:cp-get-buffer component)))

(o-defun oo/kill-emacs-no-errors ()
  "Ignore `kill-emacs-hook' when killing Emacs."
  (interactive)
  ;; Manually run kill-Emacs-ho
  (o-flet noerrs (fn &rest args) (ignore-errors (apply fn args)) nil)
  (run-hooks-wrapped 'kill-emacs-hook #'noerrs))

(defun oo/kill-emacs-no-hook ()
  "Ignore `kill-emacs-hook' when killing Emacs."
  (interactive)
  (let (kill-emacs-hook)
    (kill-emacs)))
;;; provide
(provide 'base-commands)
;;; base-commands.el ends here
