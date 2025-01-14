;;; oo-commands.el --- Generic commands -*- lexical-binding: t; -*-
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
(require 'base)
;;;; opening specific files
(defun oo-open-emacs-config ()
  "Open Emacs configuration."
  (interactive)
  (switch-to-buffer (dired user-emacs-directory)))

(defun oo-open-emacs-init-file ()
  "Open init file."
  (interactive)
  (switch-to-buffer (find-file-noselect user-init-file)))

(defun oo-open-emacs-lisp-dir ()
  "Open lisp directory."
  (interactive)
  (switch-to-buffer (dired (expand-file-name "lisp" user-emacs-directory))))
;;;; window splitting
(defun oo-split-window-right-and-focus ()
  "Split window right and select the window created with the split."
  (interactive)
  (select-window (split-window-right)))

(defun oo-split-window-below-and-focus ()
  "Split window below and select the window created with the split."
  (interactive)
  (select-window (split-window-below)))
;;;; font
(defun! oo-set-font-face ()
  "Apply an existing xfont to all graphical frames."
  (interactive)
  (set! font (completing-read "Choose font: " (x-list-fonts "*")))
  (set-frame-font font nil t))
;;;; sorting
;; This is meant to sort the great number of install package forms I have in
;; `init-elpaca'.
(defun! oo-sort-elpaca-forms (beg end)
  "Sort elpaca forms lexicographically by package name."
  (set! rx "^\\(?:;; \\)?(elpaca \\(?:(\\(?1:\\(?:[[:alnum:]]\\|-\\)+\\)\\|\\(?1:\\(?:[[:alnum:]]\\|-\\)+\\)\\)[^z-a]+?$")
  (save-excursion (sort-regexp-fields nil rx "\\1" beg end)))

(defun! oo-sort-autoload-forms (beg end)
  "Sort autoload forms lexicographically by package name."
  (set! rx "(autoload[[:blank:]]+#'[^[:space:]]+[[:blank:]]+\"\\(.+?\\)\".+?$")
  (save-excursion (sort-regexp-fields nil rx "\\1" beg end)))

;; This is meant to sort the great number of `require' forms in the init file.
(defun! oo-sort-require-forms (beg end)
  "Sort require forms lexicographically by feature name."
  (set! rx "(require[[:blank:]]+'\\(.+\\))")
  (save-excursion (sort-regexp-fields nil rx "\\1" beg end)))

(defun! oo-sort-dwim (beg end)
  "Sort lines the way I like it."
  (interactive
   (cond ((region-active-p)
	      (list (region-beginning) (region-end)))
	     ((save-excursion (aand! "(\\(\\(?:autoload\\|elpaca\\|require\\)\\)"
                                 (re-search-forward it (point-max) t nil)))
          (list (match-beginning 0) (point-max)))
         (t
          (list nil nil))))
  (save-excursion
    (goto-char beg)
    (pcase (match-string 1)
	  ("autoload" (oo-sort-autoload-forms beg end))
	  ("require" (oo-sort-require-forms beg end))
	  ("elpaca" (oo-sort-elpaca-forms beg end))
	  (_ (error "No sorting method detected")))))
;;;; alignment
(defun! oo-dwim-align ()
  (interactive)
  (set! regexp "(define-abbrev\\(?1:\\s-+\\)\\S-+\\(?2:\\s-+\\)\".*?\"\\(?3:\\s-+\\)\".*?\"\\(?4:\\s-+\\)\\S-+\\(?5:\\s-+\\):enable-function\\(?6:\\s-+\\).+)")
  (set! rules `((rule1 . ((regexp . ,regexp) (group . (1 2 3 4 5 6))))))
  (lisp-data-mode)
  (align (point-min) (point-max) nil rules)
  (emacs-lisp-mode 1))
;;;; miscellaneous
(defun oo-dwim-narrow (keep-narrowing-p)
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
        ((equal 'comment (oo-in-string-or-comment-p))
         (save-excursion (outli-toggle-narrow-to-subtree)))
        ((derived-mode-p 'org-mode)
         (or (ignore-errors (org-narrow-to-block) t)
             (org-narrow-to-subtree)))
        (t
         (narrow-to-defun))))
;; You could actually do this via abbrev-mode as well.  And actually it might be
;; better in a sense because.
(defun! oo-dwim-space ()
  "Replace two consecutive spaces with a period."
  (interactive)
  (cond ((and (or (derived-mode-p 'text-mode)
                  (oo-in-string-or-comment-p))
              (looking-back "\\([[:word:]]\\)[[:space:]]\\{2,\\}" nil))
         (replace-match "\\1.\s\s"))
        (t
         (insert "\s"))))

(defun! oo-pop-to-buffer ()
  (interactive)
  (require 'consult)
  (set! consult--buffer-display #'pop-to-buffer)
  (call-interactively #'consult-buffer))

(defun oo-kill-emacs-no-confirm ()
  "Kill Emacs without confirmation."
  (let (confirm-kill-emacs)
    (call-interactively #'kill-emacs)))

;; Keep track of the themes that I have loaded and do not allow repetitions.
(defvar oo-loaded-themes nil
  "Themes that have already been loaded.")

(defun! oo-load-random-theme ()
  "Load a random theme."
  (interactive)
  (set! not-loaded (cl-set-difference (custom-available-themes) oo-loaded-themes))
  (set! theme (seq-random-elt not-loaded))
  (condition-case err
      (progn (load-theme theme)
             (push theme oo-loaded-themes)
             (message "Loaded theme `%s'..." theme))
    (error
     (signal (car err) (cdr err)))))

;; This idea is based on the following link where xah lee talks about why the
;; scratch buffer is outdated.  It does not follow the trend of "untitled1",
;; "untitled2" as xah lee recommended because it is just easier and more
;; consistent to use Emacs's buffer naming style.
;; http://xahlee.info/emacs/emacs/modernization_scratch_buffer.html
(defun! oo-new-buffer ()
  "Create a new blank buffer."
  (interactive)
  (display-buffer (generate-new-buffer "untitled")))

;; (defun oo-ensure-file-header ()
;;   (interactive)
;;   (oo--ensure-file-header))

(defun! oo--create-lisp-dir-file (name dir comment1 comment2)
  "Auxiliary function."
  (set! filename (expand-file-name name dir))
  (cl-assert (not (file-exists-p filename)))
  (with-current-buffer (find-file filename)
    (oo--ensure-file-header)
    (goto-char (point-min))
    ;; This is a kind of roundabout way of doing it.  Not sure if it is the
    ;; "best" way whatever that means, but it works.
    (search-forward "TODO: add commentary" nil t nil)
    (replace-match comment1)
    (search-forward "TODO: add commentary" nil t nil)
    (replace-match comment2)
    (save-excursion (oo--ensure-provide filename))))

(defun! oo-create-new-init-file (feature)
  "Create a new init file for feature."
  (interactive "sFeature: ")
  (set! filename (format "init-%s.el" feature))
  (set! comment1 (format "Initialize %s" feature))
  (set! comment2 (format "Initialize %s." feature))
  (oo--create-lisp-dir-file filename oo-lisp-dir comment1 comment2))

(defun! oo-create-new-config-file (feature)
  "Create a new config file for feature."
  (interactive "sFeature: ")
  (set! filename (format "config-%s.el" feature))
  (set! comment1 (format "Configure %s" feature))
  (set! comment2 (format "Configure %s." feature))
  (oo--create-lisp-dir-file filename oo-lisp-dir comment1 comment2))

(defun! oo-create-new-test-file (feature)
  "Create a new config file for feature."
  (interactive "sFeature: ")
  (set! test-dir (expand-file-name "test" user-emacs-directory))
  (set! filename (format "base-%s-test.el" feature))
  (set! comment1 (format "Test %s" feature))
  (set! comment2 (format "Test %s." feature))
  (oo--create-lisp-dir-file filename test-dir comment1 comment2))

;;;###autoload
(defun oo-ensure-boilerplate ()
  (interactive)
  (oo-ensure-file-header)
  (oo-ensure-provide))

(defun! oo-dwim-vc-push ()
  (interactive)
  (pushing! display-buffer-alist '("\\*vc-git"
                                   (display-buffer-no-window)
                                   (allow-no-window . t)))
  (vc-git--pushpull "push" nil (list "--force")))

(defalias 'eshell/dotadd 'oo-dwim-vc-action)
(defun! oo-dwim-vc-action (file)
  "Register, stage, commit and push FILE to dotfiles repository.
If FILE is not in registered in dotfile repo, register it.  In any case commit
the file.  Additionally, push the file but only if the battery is charging or

the battery percentage is greater than 90%."
  (interactive (list (or (buffer-file-name)
                         (read-file-name "Select file to add to dofiles:"))))
  (set! backend (car (vc-deduce-fileset nil t 'state-model-only-files)))
  (set! root (vc-root-dir))
  (set! commit-msg (format "%s" (f-relative file root)))
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
  (set! display-buffer-alist `(("\\*vc-git.+\\*"
                                (display-buffer-no-window)
                                (allow-no-window . t))
                               ,@display-buffer-alist))
  (pcase (vc-state file)
    ('edited
     (vc-checkin (list file) backend commit-msg)
     (oo-dwim-vc-push))
    ('nil
     (vc-register)
     (vc-checkin (list file) backend commit-msg)
     (oo-dwim-vc-push))
    (_
     nil)))

(defhook! oo-auto-commit-and-push-dotfile-h (after-save-hook)
  "Commit and push changes to dotfile on save.
When a buffer is saved, check whether the saved file is part of the dotfiles
repository and if it is, commit and push all changes.  Otherwise, do nothing."
  (aand! (vc-root-dir)
         (buffer-file-name)
         (or (not (equal "Discharging" (battery-format "%B" (funcall battery-status-function))))
             (> (string-to-number (battery-format "%p" (funcall battery-status-function))) 90))
         (or (f-same-p it (f-full user-emacs-directory))
             (f-same-p it (f-full "~")))
         (not (equal (vc-state (buffer-file-name)) 'unregistered))
         (save-restriction (oo-dwim-vc-action (buffer-file-name)))))

(defun! oo-one-line (beg end)
  "Join lines in the region between BEG and END into a single line.
Additionally, make any duplicate spaces in line become a single space."
  (interactive "r")
  (replace-string-in-region "\n" "\s" beg end))

(defun! oo-remove-consequtive-spaces (beg end)
  "Replace consequtive spaces in region with a single space."
  (interactive "r")
  (replace-regexp-in-region "[[:space:]]\\{2,\\}" "\s" beg end))
;;; provide
(provide 'oo-commands)
;;; oo-commands.el ends here
