;;; init-frame.el --- initialize frame -*- lexical-binding: t; -*-
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
;; Initialize frame.
;;
;;; Code:
(require 'base)
;;;; window-divider
;; TODO: The display flickers when setting the initial theme.  Maybe this is
;; inevitable.  But maybe this has to do with me either disabling the previous
;; theme first or the order of setting the window-divider, or maybe I can
;; specify the default theme to load beforehand.  I need to play around with
;; settings and see if this flickering can be avoided.
(hook! after-init-hook window-divider-mode :depth 12)
(opt! window-divider-default-bottom-width 7)
(opt! window-divider-default-right-width 7)
(opt! window-divider-default-places t)
;;;; disable cursor blinking
;; By default the cursor blinks.  The point is so that it is easier to find on the
;; screen.  Usually, however, I have no trouble finding it so I disable it.
(blink-cursor-mode 1)
;; By default after a certain amount of blinks the cursor becomes solid.  By
;; setting this to a negative value I make the cursor blink forever.
(opt! blink-cursor-blinks -1)

;; Increase the blink interval slightly.
(opt! blink-cursor-interval 0.4)

(bind! oo-toggle-map "c" #'blink-cursor-mode)
;;; provide
(provide 'init-frame)
;;; init-frame.el ends here
