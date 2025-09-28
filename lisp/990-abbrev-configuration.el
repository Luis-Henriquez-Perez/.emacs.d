;;; 990-abbrev-configuration.el --- abbrev configuration -*- lexical-binding: t; -*-
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
;; This is my configuration for abbrev.
;;
;;; Code:
(require! "^0[01]")
(require 'abbrev)
;;;; PREDICATES
(defun! abbrev|in-text-p ()
  "Return non-nil when text-mode abbrevs should be enabled.
This is when the current major-mode is derived from text-mode or point is in a
string or comment."
  (or (member major-mode '(org-mode text-mode vc-git-log-edit-mode))
	  ;; These cases prevent abbreviation from expanding words outside of a
	  ;; string or comment when in some programming mode.
	  (cl-case (oo-in-string-or-comment-p)
		(string
		 (set! string-beg (car (bounds-of-thing-at-point 'string)))
		 (set! word-beg (save-excursion (backward-word) (point)))
		 (> word-beg string-beg))
		(comment
		 (set! comment-beg (save-excursion (comment-beginning) (point)))
		 (set! word-beg (save-excursion (backward-word) (point)))
         ;; The first word of a comment actually starts at `comment-beg' but
         ;; this never happens for a string.
         (>= word-beg comment-beg)))))

(defun abbrev|in-org-p ()
  "Return non-nil if the current buffer is in org-mode."
  (declare (pure t) (side-effect-free error-free))
  (derived-mode-p 'org-mode))

(defun abbrev|in-elisp-mode-p ()
  "Return non-nil if current buffer is in emacs-lisp mode."
  (declare (pure t) (side-effect-free error-free))
  (derived-mode-p 'emacs-lisp-mode))

(defun abbrev|in-elisp-code-p ()
  "Return non-nil if current buffer is elisp code."
  (declare (pure t) (side-effect-free error-free))
  (and (derived-mode-p 'emacs-lisp-mode)
       (not (oo-in-string-or-comment-p))
       ;; This is to avoid cases where we have an abbrev in a preceding string.
       ;; I do not want that to be expanded.
       (word-at-point)))

(defun abbrev|in-elisp-comment-p ()
  "Return non-nil if currently in an emacs-lisp comment."
  (declare (pure t) (side-effect-free error-free))
  (and (derived-mode-p 'emacs-lisp-mode)
       (oo-in-string-or-comment-p)))
;;;; DO NOT EXPAND ABBREV IF IT IS PART OF ANOTHER WORD
;; Some abbrevs I do not want to expand if they are immediately preceded by a
;; non-space character.  For example, I want "emacs" to expand into "Emacs" but
;; not when in a symbol like `user-emacs-directory'.
(defun! abbrev|part-of-another-word-p ()
  "Return non-nil if current abbrev is part of another word."
  (declare (pure t) (side-effect-free error-free))
  (set! rx (rx-to-string `(seq (1+ (not blank)) ,(symbol-name last-abbrev) (0+ blank))))
  (not (looking-back rx)))
;;;; DO NOT EXPAND ESCAPE CHARACTERS
;; Do not expand single letter abbrevs when they are meant to be used as escape
;; characters.
(defun abbrev|escape-char-p ()
  "Return non-nil if what was typed was an escape character."
  (declare (pure t) (side-effect-free error-free))
  (not (and (equal 'string (oo-in-string-or-comment-p))
            (save-match-data (looking-back "\\\\[ntf][[:space:]]?" (- (point) 4))))))
;;;; PREVENT GREEDY EXPANSION
;;  with `backward-word'
(abbrev-table-put global-abbrev-table :regexp "\\<\\(\\sw+\\)\\Sw*")
;;;; PARENT TABLES
(abbrev-table-put text-mode-abbrev-table :enable-function  #'abbrev|in-text-p)
(abbrev-table-put global-abbrev-table :parents (list text-mode-abbrev-table emacs-lisp-mode-abbrev-table))
;;;; ADVICES
;;;;; AUTOMATICALLY ADD PERIOD
;; I do not like manually adding periods to the end of sentences.  Having moved
;; from using one space after a sentence to two, I find it particularl daunting
;; to type period, space, space whenever I am ending one sentence and starting a
;; new one.  With this customization when I type space, space, following a word
;; it is converted into period space space.  Additionally, if I end a sentence
;; line with two spaces and I press ESC, the trailing two spaces are replaced
;; with a period.
(defun! abbrev|insert-period-maybe-a (expand-fn)
  "Add a period when necessary."
  (prog1 (funcall expand-fn)
    (when (or (member major-mode '(org-mode text-mode)) (oo-in-string-or-comment-p))
      (set! eol (line-beginning-position -1))
      (set! rx "\\([^\n!.?[:blank:]]\\)\\([[:blank:]][[:blank:]]\\)\\([^[:blank:]]+\\)")
      (cond ((looking-back rx eol)
             (replace-match "\\1.\\2\\3" nil nil nil 0))
            ((looking-back "\\([^!.?[:blank:]]\\)[[:blank:]]\\{2,\\}" eol)
             (replace-match "\\1."))))))

(advice-add 'abbrev--default-expand :around #'abbrev|insert-period-maybe-a)
;;;;; PULSE EXPANSION
;; You would be surprised at how much of an aesthetic improvement little things
;; like this can make a difference.
(defun! abbrev|pulse-expand-a (expand-fn)
  "Pulse around the expansion of an abbrev."
  (aprog1! (funcall expand-fn)
    (and it
         last-abbrev-location
         (require 'pulse nil t)
         (pulse-momentary-highlight-region last-abbrev-location (point)))))

(advice-add 'abbrev--default-expand :around #'abbrev|pulse-expand-a)
;;;;; ENSURE `POST-SELF-INSERT-HOOK' IS RUN AFTER EACH WORD
;; Captain fails to capitalize the beginning of a sentence if the beginning of a
;; sentence was generated by the expansion of an abbrev because Captain
;; captializes a word during `post-insert-hook' and a multi-word expansion will
;; skip calling that hook after each word except the last one.  So here I call
;; the hook myself at the proper places.
(defun! abbrev|ensure-post-insert-a (expand-fn)
  "Run `post-insert-hook' after each word in a multi-word expansion."
  (aprog1! (funcall expand-fn)
    (when (and it last-abbrev-location)
      (set! end (point))
      (save-excursion (goto-char last-abbrev-location)
                      (while (re-search-forward ".+?[[:blank:]]" end t nil)
                        (run-hooks 'post-self-insert-hook))))))

(advice-add 'abbrev--default-expand :around #'abbrev|ensure-post-insert-a)
;;;; LOAD ABBREVS
(defun abbrev|write-abbrev-file-a (&rest _)
  "Override `write-abbrev-file' with my own function."
  (quiet! (abbrev|update-abbrev-tables))
  (oo-log 'trace "Updating abbrevs."))
;; This is a bit crude.  It would be precise to not load the elisp abbrev table when
;; enabling abbrev mode in a text-mode but it is not significant because it
;; Emacs loads abbrevs so fast.
(advice-add 'write-abbrev-file :around #'abbrev|write-abbrev-file-a)
;;;; UPDATING ABBREVS
;; I only want this function to add complex abbrevs. abbrev
(defun abbrev/add (arg)
  "Add the abbrev I mean."
  (interactive "P")
  (add-abbrev text-mode-abbrev-table "text-mode" arg))

(defun abbrev/inverse-add (arg)
  "Add the abbrev I mean."
  (interactive "P")
  (inverse-add-abbrev text-mode-abbrev-table "text-mode" (or arg 1)))

(defun! abbrev|update-abbrev-tables ()
  "Update abbrev tables and commit changes."
  (dolist (table abbrev-table-name-list)
    (set! file (expand-file-name (format "910-%s.el" table) oo-lisp-dir))
    (when (and (abbrev--table-symbols table) (file-exists-p file))
      (set! buffer (or (get-file-buffer file) (find-file-noselect file nil t)))
      ;; TODO: handle better opening an existing buffer.
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (when (re-search-forward "^(define-abbrev-table" nil)
              (goto-char (match-beginning 0))
              (set! beg (point))
              (forward-sexp)
              (delete-region beg (point))
              (goto-char beg)
              (insert (abbrev|table-string table)))
            (save-buffer)
            (when (equal 'edited (vc-state file))
              (set! backend (car (vc-deduce-fileset nil t 'state-model-only-files)))
              (set! commit-msg (format "Add abbrevs to the %s..." (string-remove-prefix "910-" (file-name-base file))))
              (message "update table: %S %S %S %S %S" file (vc-state file) (vc-root-dir) backend commit-msg)
              ;; TODO: inhibit opening buffers.
              (vc-git-checkin (list file) commit-msg)))
        (kill-buffer buffer)))))

(defun! abbrev|table-string (table)
  "Print TABLE as `define-abbrev-table' with aligned abbrevs and no :count."
  (set! abbrevs '())
  (set! name (symbol-name table))
  (flet! insert-at-column (column string)
    "Insert STRING at COLUMN, padding with spaces if necessary."
    (let ((pad (- column (current-column))))
      (when (> pad 0)
        (insert (make-string pad ?\s)))
      (insert string)))
  (mapatoms
   (lambda (sym)
     (let* ((name (symbol-name sym))
            (expansion (symbol-value sym))
            (hook (symbol-function sym))
            (plist (symbol-plist sym))
            ;; I checked the properties and the only ones that I will
            ;; realistically use are these two.
            (case-fixed (plist-get plist :case-fixed))
            (enable-function (plist-get plist :enable-function))
            (entry (list name expansion)))
       ;; There's one entry whose name is the entry string.  Looks like ("" nil
       ;; nil).  No point in having that in the table (it is probrably always
       ;; implicitly there though).
       (unless (string-empty-p name)
         (and hook (setq entry (append entry (list hook))))
         (and enable-function (setq entry (append entry (list :enable-function enable-function))))
         (and case-fixed (setq entry (append entry (list :case-fixed case-fixed))))
         (push entry abbrevs))))
   (symbol-value table))
  (setq abbrevs (sort abbrevs (-on #'string< #'car)))
  (with-temp-buffer
    (erase-buffer)
    (insert (format "(define-abbrev-table '%s\n  '(" name))
    (set! column (current-column))
    (dolist (abbrev abbrevs)
      (set! (name expansion hook . plist) abbrev)
      (insert-at-column column (format "%S\n" abbrev)))
    ;; This is the last newline.
    (delete-char -1)
    (insert-at-column (current-column) "))")
    (buffer-string)))
;;; provide
(provide '990-abbrev-configuration)
;;; 990-abbrev-configuration.el ends here
