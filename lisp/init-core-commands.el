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
;;;; bounds
(defun o--bounds-of-delim-inner ()
  "Return the bounds of inner delimiter pair."
  (when-let* ((bounds-inside (puni-bounds-of-list-around-point))
              (beg (car bounds-inside))
              (end (cdr bounds-inside)))
    (cons beg end)))

(defun o--bounds-of-delim-outer ()
  "Return the bounds of outer delimiter pair."
  (when-let* ((bounds-around (puni-bounds-of-sexp-around-point))
              (beg (car bounds-around))
              (end (cdr bounds-around)))
    (cons beg end)))
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

(defun o-mark-delim-inner ()
  "Mark inner bounds of surrounding delimiters."
  (interactive)
  (o--mark-region (o--bounds-of-delim-inner) t))

(defun o-mark-delim-outer ()
  "Mark outer bounds of surrounding delimiters."
  (interactive)
  (o--mark-region (o--bounds-of-delim-outer) t))

(defun o--bounds-outline-subtree-inner ()
  "Return the bounds of inner subtree at point as (beg . end)."
  (save-excursion
    (outline-back-to-heading t)
    (let ((beg (progn
                 (forward-line 1)
                 (point)))
          (end (progn
                 (outline-end-of-subtree)
                 ;; `outline-end-of-subtree' does not include the last newline
                 ;; at the end.
                 (1+ (point)))))
      (cons beg end))))

(o-defun o-outline-mark-subtree-contents ()
  "Mark the contents of the outline subtree at point."
  (interactive)
  (o-set (beg . end) (o--bounds-outline-subtree-inner))
  (goto-char beg)
  (set-mark end)
  (activate-mark))
;;;; region
(defun o-region-expand-abbrevs-no-query (beg end)
  "Same as `expand-region-abbrevs' but without querying."
  (interactive "r")
  (expand-region-abbrevs beg end t))

(defun o-region-eval (beg end)
  "Same as `eval-region'."
  (interactive "r")
  (unless executing-kbd-macro
    (pulse-momentary-highlight-region beg end))
  (eval-region beg end))

;; meep's variant does not seem to save it to the system clipboard properly.
;; I know that `kill-ring-save' does also give visual indication but in my
;; opinion its indication is not very good (it is like a little pause and the
;; cursor going to beg and end)
(defun o-region-copy-as-kill (beg end)
  "Same as `copy-region-as-kill' but give visual feedback."
  (interactive "r")
  (unless executing-kbd-macro
    (pulse-momentary-highlight-region beg end))
  (copy-region-as-kill beg end))
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
Otherwise if current defining or execing a kb."
  (interactive)
  (cond (defining-kbd-macro
         (kmacro-end-macro nil))
        (t
         (kmacro-start-macro nil))))

(defun o-dwim-find-char ()
  "Find char with `flash-jump' unless in keybinding macro."
  (interactive)
  (if (or defining-kbd-macro executing-kbd-macro)
      (call-interactively #'flash-jump)
    (call-interactively #'meep-find-char)))
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
(o-defun o-abbrev-inverse-add (beg end)
  "Add the abbrev to abbrevs."
  (interactive "r")
  (o-set lisp-dir (expand-file-name "lisp/" user-emacs-directory))
  (o-set abbrev-file (expand-file-name "text-mode-abbrev-table.el" lisp-dir))
  ;; Get the abbrev at point.
  (o-set abbrev (substring-no-properties (word-at-point)))
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

(defun o-region-copy-line ()
  )
;;; provide
(provide 'init-core-commands)
;;; init-core-commands.el ends here
