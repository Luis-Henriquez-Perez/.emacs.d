;;; config-evil-easymotion.el --- evil-easymotion configuration -*- lexical-binding: t; -*-
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
;; This is my configuration for `evil-easymotion'.  Here I define commands for
;; jumping to points and  replace the default evil motions =w=, =e=, =E=, =W=
;; with more useful counterparts.
;;
;; Let me go over specifically what I do.
;;
;; 1. I create `evil-easymotion' commands that are similar but not the same as the
;; following commands: `evil-forward-word-begin'.  They differ in that they.
;;
;; 2. Instead of using the default scope I change the scope to the visible window
;; and make the point collection start from the start of the visible window
;; instead of the current point which is the default.  This is my preference and
;; it allows me to reduce the amount of movement commands I use for jumping back
;; and forth from words and it reduces the mental overhead of deciding whether
;; to go forward or backward.
;;
;; 3. In the new sort function I provide `', I remove the current point from the
;; list of candidates because it simply does not make sense to have it in there.
;; I am not going to use `evil-easymotion' to "move" to the same exact point I
;; am on; and even if I change my mind and I do want to stay I will just cancel
;; the motion.  The default sort function is fine if your only going
;; forward--which is from what I have seen how most people use
;; `evil-easymotion'--but if as I am you can go before point as well then it is
;; not the same thing.
;;
;; 4. I customize the way the `evilem-keys' are assigned with the points by
;; changing the order in which they are sorted.  It is important that the
;; `evilem-keys' are assigned in a consistent order to candidates so that these
;; commands may be used in keyboard macros.
;;
;; one important consideration is keyboard macros.  I do not think that choosing
;; with easymotion goes well with keyboard macros because the characters might
;; change positions..
;; TODO: Exclude the current point from the set because obviously I do not want
;; to jump to the place I am already at.
;;
;; The README of `evil-easymotion' suggests that by default motions should
;; ignore overlays but that did not seem to be the case for me.  But I need to
;; figure out how to tell.  Thus far I have only been able to tell by the lag in
;; large org files.
;;
;; TODO: if the there is a word at the very beginning of the buffer this does
;; not match it because then it would not be able to advance to the next points.
;; as in, I would need to start at point -1 but that position does not exist.
;; It is not a massive deal because it is not often your at the top of the
;; window and there is a word right at the beginning of the buffer but I would
;; like to fix it.
;; TODO: captain does not work well in comments.  Captain uses the
;; `sentence-at-point' function I think so I just think that function does not
;; work in comments.
;;
;; TODO: find a function that returns the point at the top of the window (not
;; the top of the buffer).  I should be using that instead of `point-min'.
;;
;; On another note, the `:point-collection' key for `evilem-make-motion' will
;; let me insert the point at the beginning of the buffer.  Alternatively, I
;; could specify another function that just gets that one point if it's at the
;; top of the window.  That function can also choose to sort the points so I can
;; sort them based on.
;;
;;; Code:
(require 'rx)
(require 'dash)
(require 'evil-easymotion)
;;;; ignore overlay
;; TODO: abbrev that specifies only words isolated by spaces.
;; I got this idea from writing some function name suffixed with "-fn" and
;; getting the unwanted abbrev expansion.  Essentially, I want to tell abbrev if
;; there is a non-blank, do not expand it.  I only intended this abbrev to work
;; in isolation.  For things like abnormal hooks I can use text-completion.

;; (defun oo--left-bouned-by-space-p ())
;; (defun oo--skip-ov-fn (motion-fn)
;;   "Return MOTION-FUNCTION."
;;   (oo-before-fn motion-fn (apply-partially #'goto-char (end-of-overlay)))
;;   (oo-if-fn #'in-overlay-p it motion-function))
;;;; set the evilem-keys
;; The package `evil-easymotion' has its own style and keys separate from avy
;; keys.
(opt! evilem-style 'at)
(opt! evilem-keys (eval-when-compile (string-to-list "jfkdlsaurieowncpqmxzb")))
;;;; more appropriate sort function
;; TODO: The reason that the letters are inconsistent is because the default
;; sorting function assigns values based on the /character distance/ not the
;; distance in words.  So if there is a long word.  What I need to do is sort by
;; word distance.
;;
;; 0. Remove the current point from points
;; 1. sort the points by distance from the main point
;; 2. separate the points into points less than origin and points greater than it
;; 3. interleave the points
(defun! +evilem--sort-by-match (points)
  "Return points sorted by match occurance.
This is as opposed to character length."
  (set! point (point))
  (set! points (cl-remove-if (-compose (apply-partially #'= point) #'car) points))
  (set! points (sort points (-on (apply-partially #'< point) #'car)))
  (set! (less-than greater-than) (-separate (-on (apply-partially #'< point) #'car) points))
  ;; Interleave the points.  I really wish that I could use dash's `-interleave'
  ;; but if the interleaved lists are not the same size the extra values are
  ;; thrown away.
  (while (or less-than greater-than)
    (when less-than
      (pushing! interleaved (pop less-than)))
    (when greater-than
      (pushing! interleaved (pop greater-than))))
  (nreverse interleaved))
;;;; improve scope
;; Something similar is used in doom.
(put 'visible 'bounds-of-thing-at-point (lambda () (cons (window-start) (window-end))))
;;;; macro to simplify defining motions
;; It is a peeve of mine seeing excessive wordiness in defining these motions.
;; Instead of defining a helper function and then using it in the
;; `evil-make-motion' invocation, I would like to do it all at once.
(defmacro! +evilem-defemotion! (name args &rest body)
  "Convenience macro for defining evil motions.
This is a wrapper around `evilem-make-motion'."
  (declare (indent defun))
  ;; TODO: use what I defined on base-lib for this.
  ;; Remove the keywords passed in after docstring.
  (when (stringp (car body))
    (set! docstring (pop body)))
  (while (keywordp (car body))
    (appending! map (list (pop body) (pop body))))
  `(evilem-make-motion ,name (lambda ,args ,docstring (interactive) (autolet! ,@body)) ,@map))
;;;; beginning of word
(+evilem-defemotion! +evilem-motion-beginning-of-word ()
  "Jump to the beginning of a word in the current visible buffer."
  :initial-point #'window-start
  :scope 'visible
  :collect-postprocess #'+evilem--sort-by-match
  (set! regexp (rx (seq (one-or-more (not word)) (group word) (zero-or-more word))))
  (and (save-excursion (forward-char)
                       (re-search-forward regexp nil t nil))
       (goto-char (match-beginning 1))))
;;;; beginning of WORD
(+evilem-defemotion! +evilem-motion-beginning-of-WORD ()
  "Jump to the beginning of a WORD in the current visible buffer."
  :initial-point #'window-start
  :scope 'visible
  :collect-postprocess #'+evilem--sort-by-match
  (set! blank-rx (rx (or bol (1+ white)) (group (not white))))
  (set! rx (rx (: bow (group word) (* (not white)) eow)))
  (and (re-search-forward blank-rx nil t nil)
       (goto-char (match-beginning 0))
       (re-search-forward rx nil t nil)
       (goto-char (match-beginning 1))))
;;;; end of word
(+evilem-defemotion! +evilem-motion-end-of-word ()
  "Jump to the beginning of a word in the current visible buffer."
  :initial-point #'window-start
  :scope 'visible
  :collect-postprocess #'+evilem--sort-by-match
  (set! regexp "[[:alnum:]]+")
  ;; I need to ensure that the regexp does not match the word on top of the
  ;; current point.
  ;; TODO: Fix capitalization rules, current at the top of the sentence above
  ;; was capitalized but should not be.
  (and (save-excursion (forward-char)
                       (re-search-forward regexp nil t nil))
       (goto-char (1- (match-end 0)))))
;;;; end of WORD
(+evilem-defemotion! +evilem-motion-end-of-WORD ()
  "Jump to the end of a WORD in the current visible buffer."
  :initial-point #'window-start
  :scope 'visible
  :collect-postprocess #'+evilem--sort-by-match
  (set! regexp "\\(?:\\`\\|[^[:blank:]]+\\)\\([[:word:]]\\)")
  (and (re-search-forward regexp nil t nil)
       (goto-char (match-beginning 1))))
;;;; parentheses
;; TODO: how to find current form?
(+evilem-defemotion! +evilem-open-paren ()
  "Jump to opening parenthesis in current form."
  :scope 'visible
  :collect-postprocess #'+evilem--sort-by-match
  (set! regexp "(")
  (and (save-excursion (forward-char) (re-search-forward regexp nil t nil))
       (goto-char (match-beginning 0))))
;;;; first non-whitespace character in line
;; TODO: screenshot the difference between this with and without the
;; postprocess.
(+evilem-defemotion! +evilem-motion-beginning-of-line ()
  "Jump to the beginning of line in the current visible buffer."
  :initial-point #'window-start
  :scope 'visible
  :collect-postprocess #'+evilem--sort-by-match
  (set! regexp "^[[:space:]]*\\(.\\)")
  (and (save-excursion (forward-char) (re-search-forward regexp nil t nil))
       (goto-char (match-beginning 1))))
;;;; a character
(+evilem-defemotion! +evilem-motion-char ()
  "Jump to a character in current visible buffer."
  :bind ((char (read-char "Char: ")))
  :initial-point #'window-start
  :scope 'visible
  :collect-postprocess #'+evilem--sort-by-match
  (and (save-excursion (forward-char)
                       (re-search-forward (rx-to-string (char-to-string char)) nil t nil))
       (goto-char (match-beginning 0))))
;;; provide
(provide 'config-evil-easymotion)
;;; config-evil-easymotion.el ends here
