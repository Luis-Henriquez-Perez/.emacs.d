;;; config-telephone-line.el --- Configure telephone-line -*- lexical-binding: t; -*-
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
;; Configure telephone-line.
;;
;;; Code:
;;;; custom segments
;;;;; major-mode information
(telephone-line-defsegment* +telephone-line-major-mode-segment ()
  ;; Do not show the ugly "\l" that indicates lexical binding.
  (alet! (format-mode-line (funcall (telephone-line-major-mode-segment) face))
    (if (string-match "\\`ELisp" it)
        (substring it (match-beginning 0) (match-end 0))
      it)))
;;;;; kbd-macro information
(telephone-line-defsegment* +telephone-line-kbd-macro-segment ()
  (oo-modeline-component--kbd-macro))
;;;;; narrowing information
(telephone-line-defsegment* +telephone-line-narrow-segment ()
  (oo-modeline-component--narrow))
;;;;; buffer
(telephone-line-defsegment* +telephone-line-buffer-segment ()
  (oo-modeline-component--buffer-name))
;;;;; pomodoro
(telephone-line-defsegment* +telephone-line-pomodoro-segment ()
  (oo-modeline-component--pomodoro))
;;;;; org timer (what I use as pomodoro)
;;;;; current-time
;; TODO: how to display somet
(telephone-line-defsegment* +telephone-line-current-time-segment ()
  (oo-modeline-component--current-time))
;;;;; battery
(telephone-line-defsegment* +telephone-line-battery-segment ()
  (oo-modeline-component--battery))
;;;;; emms
;; TODO: Add how much time is left plaing...
(telephone-line-defsegment* +telephone-line-emms-segment ()
  (oo-modeline-component--emms))
;;;;; version control information
(telephone-line-defsegment* +telephone-line-vc-segment ()
  (oo-modeline-component--version-control))
;;;;; read-only
(telephone-line-defsegment* +telephone-line-read-only-segment ()
  (oo-modeline-component--read-only))
;;;; add utilities for updating the modeline
(defun! +telephone-line-update ()
  "Update the telephone-line modeline."
  (interactive)
  (set! modeline (if telephone-line-mode `("%e" ,@(telephone-line--generate-mode-line)) telephone-line--default-mode-line))
  (setq-default mode-line-format modeline)
  (oo-update-modeline))
;;; provide
(provide 'config-telephone-line)
;;; config-telephone-line.el ends here
