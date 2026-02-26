;;; lib-abbrev.el --- abbrev configuration -*- lexical-binding: t; -*-
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
(require 'init-core)
(require 'abbrev)
(require 'text-mode-abbrev-table)
(require 'emacs-lisp-mode-abbrev-table)
;;;; PREDICATES
;;;;; MODAL
(o-defun o-abbrev-in-text-p ()
  "Return non-nil when text-mode abbrevs should be enabled.
This is when the current major-mode is derived from text-mode or point is in a
string or comment."
  (or (member major-mode '(vc-git-log-edit-mode))
      (derived-mode-p 'text-mode)
	  ;; These cases prevent abbreviation from expanding words outside of a
	  ;; string or comment when in some programming mode.
	  (cl-case (o-in-string-or-comment-p)
		(string
		 (o-set string-beg (car (bounds-of-thing-at-point 'string)))
		 (o-set word-beg (save-excursion (backward-word) (point)))
		 (> word-beg string-beg))
		(comment
		 (o-set comment-beg (save-excursion (comment-beginning) (point)))
		 (o-set word-beg (save-excursion (backward-word) (point)))
         ;; The first word of a comment actually starts at `comment-beg' but
         ;; this never happens for a string.
         (>= word-beg comment-beg)))))

(defun o-abbrev-in-org-p ()
  "Return non-nil if the current buffer is in org-mode."
  (declare (pure t) (side-effect-free error-free))
  (derived-mode-p 'org-mode))

(defun o-abbrev-in-elisp-mode-p ()
  "Return non-nil if current buffer is in emacs-lisp mode."
  (declare (pure t) (side-effect-free error-free))
  (derived-mode-p 'emacs-lisp-mode))

(defun o-abbrev-in-elisp-code-p ()
  "Return non-nil if current buffer is elisp code."
  (declare (pure t) (side-effect-free error-free))
  (and (derived-mode-p 'emacs-lisp-mode)
       (not (o-in-string-or-comment-p))
       ;; This is to avoid cases where we have an abbrev in a preceding string.
       ;; I do not want that to be expanded.
       (word-at-point)))

(defun o-abbrev-in-elisp-comment-p ()
  "Return non-nil if currently in an emacs-lisp comment."
  (declare (pure t) (side-effect-free error-free))
  (and (derived-mode-p 'emacs-lisp-mode)
       (o-in-string-or-comment-p)))
;;;;; DO NOT EXPAND ABBREV IF IT IS PART OF ANOTHER WORD
;; Some abbrevs I do not want to expand if they are immediately preceded by a
;; non-space character.  For example, I want "emacs" to expand into "Emacs" but
;; not when in a symbol like `user-emacs-directory'.
(o-defun o-abbrev-part-of-another-word-p ()
  "Return non-nil if current abbrev is part of another word."
  (declare (pure t) (side-effect-free error-free))
  (o-set rx (rx-to-string `(seq (1+ (not blank)) ,(symbol-name last-abbrev) (0+ blank))))
  (not (looking-back rx (line-beginning-position))))
;;;;; DO NOT EXPAND ESCAPE CHARACTERS
;; Do not expand single letter abbrevs when they are meant to be used as escape
;; characters.
(defun o-abbrev-escape-char-p ()
  "Return non-nil if what was typed was an escape character."
  (declare (pure t) (side-effect-free error-free))
  (not (and (equal 'string (o-in-string-or-comment-p))
            (save-match-data (looking-back "\\\\[ntf][[:space:]]?" (- (point) 4))))))
;;;; ADVICES
;;;;; AUTOMATICALLY ADD PERIOD
;; I do not like manually adding periods to the end of sentences.  Having moved
;; from using one space after a sentence to two, I find it particularl daunting
;; to type period, space, space whenever I am ending one sentence and starting a
;; new one.  With this customization when I type space, space, following a word
;; it is converted into period space space.  Additionally, if I end a sentence
;; line with two spaces and I press ESC, the trailing two spaces are replaced
;; with a period.
(o-defun o-advice--abbrev-insert-period-maybe (expand-fn)
  "Add a period when necessary."
  (prog1 (funcall expand-fn)
    (when (or (member major-mode '(org-mode text-mode)) (o-in-string-or-comment-p))
      (o-set eol (line-beginning-position -1))
      (o-set rx "\\([^\n!.?[:blank:]]\\)\\([[:blank:]][[:blank:]]\\)\\([^[:blank:]]+\\)")
      (cond ((looking-back rx eol)
             (replace-match "\\1.\\2\\3" nil nil nil 0))
            ((looking-back "\\([^!.?[:blank:]]\\)[[:blank:]]\\{2,\\}" eol)
             (replace-match "\\1."))))))
;;;;; PULSE EXPANSION
;; You would be surprised at how much of an aesthetic improvement little things
;; like this can make a difference.
(o-defun o-advice--abbrev-pulse-expand (expand-fn)
  "Pulse around the expansion of an abbrev."
  (o-aprog1 (funcall expand-fn)
    (and it
         last-abbrev-location
         (require 'pulse nil t)
         (pulse-momentary-highlight-region last-abbrev-location (point)))))
;;;;; ENSURE `POST-SELF-INSERT-HOOK' IS RUN AFTER EACH WORD
;; Captain fails to capitalize the beginning of a sentence if the beginning of a
;; sentence was generated by the expansion of an abbrev because Captain
;; captializes a word during `post-insert-hook' and a multi-word expansion will
;; skip calling that hook after each word except the last one.  So here I call
;; the hook myself at the proper places.
(o-defun o-advice--abbrev-ensure-post-insert (expand-fn)
  "Run `post-insert-hook' after each word in a multi-word expansion."
  (o-aprog1 (funcall expand-fn)
    (when (and it last-abbrev-location)
      (o-set end (point))
      (save-excursion (goto-char last-abbrev-location)
                      (while (re-search-forward ".+?[[:blank:]]" end t nil)
                        (run-hooks 'post-self-insert-hook))))))
;;;;; WRITING THE ABBREV FILE

;;;; COMMANDS
;; I only want this function to add complex abbrevs. abbrev
(defun abbrev/add (arg)
  "Add the abbrev I mean."
  (interactive "P")
  (add-abbrev text-mode-abbrev-table "text-mode" arg))

(defun abbrev/inverse-add (arg)
  "Add the abbrev I mean."
  (interactive "P")
  (inverse-add-abbrev text-mode-abbrev-table "text-mode" (or arg 1)))
;;; provide
(provide 'lib-abbrev)
;;; lib-abbrev.el ends here
