;;; 01-base-settings.el --- core settings -*- lexical-binding: t; -*-
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
;; Here lies a collection of built-in settings that I want to take effect
;; immediately.  Many of them have to do with disabling default Emacs behaviors
;; that I don't like. I specifically place them at the forefront of my configuration
;; to ensure that they will always be evaluated regardless of what unexpected error
;; should occur afterwards.
;;
;;; Code:
;;; built-in settings
;;;; c source code
;;;;; user information
(setq user-full-name "Luis Henriquez-Perez")
(setq user-mail-address "luis@luishp.xyz")
;;;;; qtile
;; https://old.reddit.com/r/emacs/comments/xk7k6x/emacs_wont_go_fullscreen_in_qtile/
;; In qtile this does not allow emacs to go completely fullscreen.
(setq frame-resize-pixelwise t)
;;;;; load newer files
(setq-default load-prefer-newer t)
;;;;; by default do not wrap lines
;; When a line is too long to be displayed in the screen do not wrap it around;
;; just let the rest of the line go out of view (with an indicator that there is
;; more to the line in the fringe).  Although I can see the whole line when it
;; wraps around, I find it makes the text confusing and harder to read.  If I
;; want this, then I will toggle it myself with [[][toggle-truncate-lines]].
(setq-default truncate-lines t)
;;;;; show newlines as =\n= instead of an actual newline
;; They are easier to deal with and do not occupy unnecessary lines.
(setq print-escape-newlines t)
;;;;; set the fill-column 80 by default
(setq-default fill-column 80)
;;;;; use =yes-or-no-p= instead of =y-or-n-p=
;; Essentially, I am telling all Emacs functions that prompt the user for a =yes=
;; or =no= to instead allow me to type =y= or =p=.  [[helpfn:yes-or-no-p][yes-or-no-p]] is defined in c
;; source code.
(advice-add #'yes-or-no-p :override #'y-or-n-p)
;;;;; don't create lockfiles
(setq create-lockfiles nil)

;; Do not create backup files ever.
(setq version-control 'never)
;;;;; don't flash unstyled modeline at startup
;; When emacs starts up, the default modeline will show up.  Rendering this default
;; modeline at startup does slightly slow down emacs (insignificant on it's own but
;; these things add up).  So I disable it.
;; (setq-default mode-line-format nil)
;;;;; don't ask me whether I want to kill a buffer with a live process
;; I got this from [[https://www.masteringemacs.org/article/disabling-prompts-emacs][this-post]].  Every time you try to kill a buffer with a live
;; process, Emacs will ask you if you're sure you want to kill it.
(setq kill-buffer-query-functions (remq 'process-kill-buffer-query-function kill-buffer-query-functions))
;;;;; move files to trash instead of deleting them
;; By default Emacs actually deletes files.  By setting this to t, you tell Emacs
;; to move a file to trash instead of actually deleting it.  This is better because
;; if you accidentally delete a file or discover you can still just go get your
;; file from the trash.
(setq delete-by-moving-to-trash t)
;;;;; enable recursive minibuffer
;; With this enabled, I can invoke the minibuffer while still being in the
;; minibuffer.  At the very least this is useful so that I can inspect which keys
;; are bound in the minibuffer.
(setq enable-recursive-minibuffers t)
;;;;; recenter point if it goes 20 lines past what is visible
;; Note that the following comment is taken from noctuid's config: "Recenter the
;; point if it goes greater than 20 lines past what is visible the default, 0, is
;; kind of annoying because it recenters even if you just go one line down from
;; the window bottom, but a higher value is nice to automatically recenter after
;; any bigger jump."
(setq scroll-conservatively 20)
;;;;; skip fontification functions when there's input pending
(setq redisplay-skip-fontification-on-input t)
;;;;; don't echo keystrokes
;; By default emacs shows.
(setq echo-keystrokes 0)
;;;;; set the tab-width to =4=; it's =8= by default
(setq-default tab-width 4)
;;;;; don't beep
;; This variable controls whether emacs makes a sound when certain events happen
;; such as invoking a binding that doesn't have anything bound to it or trying
;; to exceed the end of the buffer--things like that.  Personally, I don't want
;; such beeping.  Setting this variable to nil still result in beeping, emacs
;; just uses its default function.  Instead, to be disabled it must
;; be set to [[file:snapshots/helpful-command:ignore.png][ignore]].
(setq ring-bell-function #'ignore)
;;;;; disable repeated error message functions
;; When you try to move past the beginning and end of a buffer Emacs produces
;; error messages.
;; [[https://emacs.stackexchange.com/questions/10932/how-do-you-disable-the-buffer-end-beginning-warnings-in-the-minibuffer][disable warnings]]
(defun oo-command-error-function (data context caller)
  "Ignore the buffer-read-only, beginning-of-buffer,
end-of-buffer signals; pass the rest to the default handler."
  (unless (memq (car data) '(buffer-read-only beginning-of-buffer end-of-buffer))
    (command-error-default-function data context caller)))

(setq command-error-function #'oo-command-error-function)
;;;;; stop emacs from asking to save buffers on quit
;; https://stackoverflow.com/questions/35658509/gnu-emacs-how-to-disable-prompt-to-save-modified-buffer-on-exit
;; https://emacs.stackexchange.com/questions/22275/save-a-particular-buffer-without-prompting-on-emacs-exit
;; https://stackoverflow.com/questions/6762686/prevent-emacs-from-asking-modified-buffers-exist-exit-anyway
;;;;; uncategorized
;; if you don't use RTL ever, this could improve perf
;; https://news.ycombinator.com/item?id=39127859
(setq-default bidi-display-reordering 'left-to-right)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)

;; improves terminal emulator (vterm/eat) throughput
(setq read-process-output-max (* 2 1024 1024))
(setq process-adaptive-read-buffering nil)
(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)
(setq inhibit-compacting-font-caches t)

(setq idle-update-delay 1.0)
;;;;;; unknown
;; I like an indentation of 4 spaces; maybe I have gotten used to it with Python.
(setq sgml-basic-offset 4)
;;;;;; dabbrev
;; (setq dabbrev-check-all-buffers nil)
;;;;; re-builder
;; By default, use `rx' syntax.  It is my preferred syntax.
(setq reb-re-syntax 'rx)
;;;;; do not auto-enable =show-paren-mode= in editing modes
;; By default =show-paren-mode= is enabled in all editing mode (non-special
;; modes).  I want to control when to enable this mode normally--as in, add it to
;; hooks myself if I want it enabled.  Therefore, I disable it here.
(setq show-paren-predicate nil)
;;;;; do not use dialog box
;; https://olddeuteronomy.github.io/post/some-excerpts-from-my-emacs-config/
;; Don’t use dialog boxes.
(setq use-dialog-box nil)
;;;;; speed up cursor movement
;; https://olddeuteronomy.github.io/post/some-excerpts-from-my-emacs-config/
;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746
(setq auto-window-vscroll nil)
;;;;; scrolling
;;;; formally disable scroll-bar
;; When you disable the scroll-bar via early-init.el powerline does not realize
;; the scroll-bar is dabled because the value of `scroll-bar-mode' is right.
(setq scroll-bar-mode nil)
;;;;; Prevent devaralias from generating a warning
;; The built-in package `woman' overwrites the existing variable
;; `woman-topic-history' by aliasing it to `Man-topic-history' and emacs tells
;; you this by popping up a *Warnings* buffer whenever woman.el is loaded.  This
;; whole thing is probably some bug.  So I stop this whole thing from happening.
(defun oo--suppress-woman-warning (orig-fn &rest args)
  (pcase args
    (`(woman-topic-history Man-topic-history . ,_)
     (advice-remove 'defvaralias #'oo--suppress-woman-warning))
    (_
     (apply orig-fn args))))

(advice-add 'defvaralias :around #'oo--suppress-woman-warning)
;;; provide
(provide '01-base-settings)
;;; 01-base-settings.el ends here
