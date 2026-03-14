;;; init-core-commands.el --- Generic commands -*- lexical-binding: t; -*-
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
(require 'init-core-lib)

(declare-function org-narrow-to-block "org")
(declare-function org-narrow-to-subtree "org")
(declare-function outli-toggle-narrow-to-subtree "org")

(declare-function puni--wrap-region "puni")
(declare-function puni-bounds-of-sexp-around-point "puni")
;;;; emacs
(defun o-emacs-open-config ()
  "Open Emacs configuration."
  (interactive)
  (switch-to-buffer (dired user-emacs-directory)))

(defun o-emacs-open-init-file ()
  "Open init file."
  (interactive)
  (switch-to-buffer (find-file-noselect user-init-file)))

(defun o-emacs-open-lisp-dir ()
  "Open lisp directory."
  (interactive)
  (switch-to-buffer (dired (expand-file-name "lisp" user-emacs-directory))))

(o-defun o-emacs-kill-no-errors ()
  "Ignore `kill-emacs-hook' when killing Emacs."
  (interactive)
  (o-flet noerrs (fn &rest args) (ignore-errors (apply fn args)) nil)
  (run-hooks-wrapped 'kill-emacs-hook #'noerrs))

(defun o-emacs-kill-no-hook ()
  "Ignore `kill-emacs-hook' when killing Emacs."
  (interactive)
  (let (kill-emacs-hook)
    (kill-emacs)))

(defun o-emacs-kill-no-confirm ()
  "Kill Emacs without confirmation."
  (let (confirm-kill-emacs)
    (call-interactively #'kill-emacs)))

(defvar o-loaded-themes nil
  "Themes that have already been loaded by `o-Emacs-load-random-thme'.")

(o-defun o-emacs-load-random-theme ()
  "Load a random theme."
  (interactive)
  (o-set themes (custom-available-themes))
  (o-set not-loaded (cl-set-difference themes o-loaded-themes))
  (if not-loaded
      (o-set theme (seq-random-elt not-loaded))
    (setq o-loaded-themes nil)
    (o-set theme (seq-random-elt themes)))
  (message "Loading theme `%s'..." theme)
  (load-theme theme 'noconfirm)
  (push theme o-loaded-themes))

(o-defun o-emacs-set-font-face ()
  "Apply an existing xfont to all graphical frames."
  (interactive)
  (o-set font (completing-read "Choose font: " (x-list-fonts "*")))
  (set-frame-font font nil t))
;;;; window splitting
(defun o-window-split-right-and-focus ()
  "Split window right and select the window created with the split."
  (interactive)
  (select-window (split-window-right)))

(defun o-window-split-below-and-focus ()
  "Split window below and select the window created with the split."
  (interactive)
  (select-window (split-window-below)))
;;;; wrap
(defun o-delim-wrap-round (beg end)
  "Wrap region with parentheses."
  (interactive "r")
  (puni--wrap-region beg end "(" ")"))

(defun o-delim-wrap-square (beg end)
  "Wrap region with brackets."
  (interactive "r")
  (puni--wrap-region beg end "[" "]"))

(defun o-delim-wrap-curly (beg end)
  "Wrap region with curly braces."
  (interactive "r")
  (puni--wrap-region beg end "{" "}"))

(defun o-delim-wrap-angle (beg end)
  "Wrap region with angle brackets."
  (interactive "r")
  (puni--wrap-region beg end "<" ">"))

(defun o-delim-wrap-spike (beg end)
  "Wrap region with spike brackets."
  (interactive "r")
  (puni--wrap-region beg end "`" "'"))

(defun o-delim-wrap-single-quote (beg end)
  "Wrap region with single quotes."
  (interactive "r")
  (puni--wrap-region beg end "'" "'"))

(defun o-delim-wrap-double-quote (beg end)
  "Wrap region with double quotes."
  (interactive "r")
  (puni--wrap-region beg end "\"" "\""))
;;;; mark
;; For now I am placing the point at the end of region on every command for
;; consistency.  I think in general the end is more useful because you can
;; presumably continue to expand the selection forward which is what I think
;; most will want to do.  For line selection this is definitely a thing.
(defun o--mark-region (bounds &optional at-end)
  "Activate mark at BOUNDS.
If AT-END is non-nil place point at end of mark.  Otherwise, place point at the
beginning of region."
  (goto-char (if at-end (cdr bounds) (car bounds)))
  (set-mark (if at-end (car bounds) (cdr bounds)))
  (activate-mark))
;;;; pulse
(defun o-region-kill-safe (beg end)
  "Kill region while preserving delimiters."
  (interactive "r")
  (puni-soft-delete beg end 'strict-sexp 'beyond 'kill))

(defun o-pulse-toggle ()
  "Toggle `pulse-flag' between t and never."
  (interactive)
  (setq pulse-flag (if (equal pulse-flag 'never) t 'never)))

(defun o-advice-pulse-region-maybe (fn beg end &rest args)
  "Advice that causes."
  (unless executing-kbd-macro
    (pulse-momentary-highlight-region beg end))
  (apply fn beg end args))

(advice-add 'eval-region :around #'o-advice-pulse-region-maybe)
(advice-add 'copy-region-as-kill :around #'o-advice-pulse-region-maybe)
;;;; delimiters
(defun o-delim-change-surround (beg-delim end-delim)
  "Change the delimiters of sexp around point."
  (when-let* ((bounds-inside (puni-bounds-of-list-around-point))
              (bounds-around (puni-bounds-of-sexp-around-point))
              (beg1 (car bounds-around))
              (end1 (car bounds-inside))
              (beg2 (cdr bounds-inside))
              (end2 (cdr bounds-around))
              (open-delim-length (- end1 beg1))
              (close-delim-length (- end2 beg2)))
    (puni-delete-region beg1 end1)
    (puni-delete-region (- beg2 open-delim-length)
                        (- end2 open-delim-length))
    (save-excursion
      (goto-char (- beg2 open-delim-length))
      (insert end-delim)
      (goto-char beg1)
      (insert beg-delim))
    (setq deactivate-mark nil)))

(defun o-delim-change-surround-to-angle ()
  "Change surrounding delimiters to angle braces."
  (interactive)
  (o-delim-change-surround "<" ">"))

(defun o-delim-change-surround-to-square ()
  "Change surrounding delimiters to square braces."
  (interactive)
  (o-delim-change-surround "[" "]"))

(defun o-delim-change-surround-to-round ()
  "Change surrounding delimiters to parentheses."
  (interactive)
  (o-delim-change-surround "(" ")"))

(defun o-delim-change-surround-to-curly ()
  "Change surrounding delimiters to curly braces."
  (interactive)
  (o-delim-change-surround "{" "}"))
;;;; dwim
(defun o-dwim-narrow-or-widen (keep-narrowing-p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: narrow to region, outline heading, org-src-block, org-subtree, or
defun, whichever applies first.

With prefix KEEP-NARROWING-P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (cond ((and (buffer-narrowed-p) (not keep-narrowing-p))
         (widen))
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

(defun o-dwim-kmacro-start-or-end ()
  "Start kboard macro if not started.
Otherwise if current defining or executing a keyboard macro, end it."
  (interactive)
  (cond (defining-kbd-macro
         (kmacro-end-macro nil))
        (t
         (kmacro-start-macro nil))))

(defvar o-escape-hook nil
  "Hook run for `o-dwim-escape'.")

(defun o-dwim-escape ()
  "Exit out of whatever is happening after escape.
Enter normal state.  If in minibuffer, exit the minibuffer.  When in a
non-readonly file buffer, save the buffer."
  (interactive)
  (run-hooks 'o-escape-hook)
  (cond
   ((minibuffer-window-active-p (minibuffer-window))
    (if (or defining-kbd-macro executing-kbd-macro)
        (minibuffer-keyboard-quit)
      (abort-recursive-edit)))
   ((or defining-kbd-macro executing-kbd-macro)
    nil)
   (t
    (when (and (not buffer-read-only) (buffer-file-name) (buffer-modified-p))
      (save-buffer))
    (keyboard-quit))))
;;;; scroll
(defun o-scroll-to-bottom ()
  "Scroll line to bottom of page."
  (interactive)
  (recenter -1))

(defun o-scroll-to-top ()
  "Scroll line to top of page."
  (interactive)
  (recenter 1))
;;;; miscellaneous
(declare-function consult-buffer "consult")
(defvar consult--buffer-display)
(o-defun o-pop-to-buffer ()
  (interactive)
  (require 'consult)
  (o-set consult--buffer-display #'pop-to-buffer)
  (call-interactively #'consult-buffer))

;; TODO: make surround smart enough to add escaped quotes when necessary.
(o-defun o-abbrev-inverse-add ()
  "Add the abbrev to abbrevs."
  (interactive)
  (o-set lisp-dir (expand-file-name "lisp/" user-emacs-directory))
  (o-set abbrev-file (expand-file-name "text-mode-abbrev-table.el" lisp-dir))
  ;; Get the abbrev at point.
  (o-set abbrev (downcase (substring-no-properties (word-at-point))))
  ;; Get the expansion.
  (o-set expansion (read-string (format "Expansion for %s? " abbrev)))
  ;; Add the abbrev to the abbrev file.
  (o-set newline (format "(define-abbrev text-mode-abbrev-table %S %S)\n" abbrev expansion))
  ;; (message newline)
  ;; If the file is modified, save it first.
  (o-set buffer (or (get-file-buffer abbrev-file) (find-file-noselect abbrev-file)))
  ;; Find the first abbrev in the file.
  (with-current-buffer buffer
    (goto-char (point-min))
    (if (re-search-forward "^(define-abbrev" nil t nil)
        (progn (goto-char (line-beginning-position))
               (insert newline)
               (eval-buffer))
      (error "No define abbrev form in file.")))
  ;; Expand the abbrev at point to new expansion.
  ;; this needs to be called at the end of the abbrev.
  (expand-abbrev))

(defun o-advice--expand-abbrevs-no-query (fn start end &optional _)
  "Same as `expand-region-abbrevs' but without querying."
  (funcall fn start end 'noquery))

(advice-add 'expand-region-abbrevs :around #'o-advice--expand-region-abbrevs-no-query)
;;; provide
(provide 'init-core-commands)
;;; init-core-commands.el ends here
