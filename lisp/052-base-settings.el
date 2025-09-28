;;; 052-base-settings.el --- core settings -*- lexical-binding: t; -*-
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
;; that I do not like. I specifically place them at the forefront of my configuration
;; to ensure that they will always be evaluated regardless of what unexpected error
;; should occur afterwards.
;;
;;; Code:
;;;; UNCATEGORIZED
(setq user-full-name "Luis Henriquez-Perez")
(setq user-mail-address "luis@luishp.xyz")
;; https://old.reddit.com/r/emacs/comments/xk7k6x/emacs_wont_go_fullscreen_in_qtile/
;; In qtile this does not allow emacs to go completely fullscreen.
(setq frame-resize-pixelwise t)
(setq-default load-prefer-newer t)
;; When a line is too long to be displayed in the screen do not wrap it around;
;; just let the rest of the line go out of view (with an indicator that there is
;; more to the line in the fringe).  Although I can see the whole line when it
;; wraps around, I find it makes the text confusing and harder to read.  If I
;; want this, then I will toggle it myself with [[][toggle-truncate-lines]].
(setq-default truncate-lines t)
;; They are easier to deal with and do not occupy unnecessary lines.
(setq print-escape-newlines t)
(setq-default fill-column 80)
;; When emacs starts up, the default modeline will show up.  Rendering this default
;; modeline at startup does slightly slow down emacs (insignificant on it's own but
;; these things add up).  This disable it.
;; (setq-default mode-line-format nil)
;; I got this from [[https://www.masteringemacs.org/article/disabling-prompts-emacs][this-post]].  Every time you try to kill a buffer with a live
;; process, Emacs will ask you if you're sure you want to kill it.
(setq kill-buffer-query-functions (remq 'process-kill-buffer-query-function kill-buffer-query-functions))
;; With this enabled, I can invoke the minibuffer while still being in the
;; minibuffer.  At the very least this is useful so that I can inspect which keys
;; are bound in the minibuffer.
(setq enable-recursive-minibuffers t)
;; Note that the following comment is taken from noctuid's config: "Recenter the
;; point if it goes greater than 20 lines past what is visible the default, 0, is
;; kind of annoying because it recenters even if you just go one line down from
;; the window bottom, but a higher value is nice to automatically recenter after
;; any bigger jump."
(setq scroll-conservatively 20)
(setq redisplay-skip-fontification-on-input t)
;; By default emacs shows your keystrokes in the echo area.
(setq echo-keystrokes 0)
(setq-default tab-width 4)
;; This variable controls whether emacs makes a sound when certain events happen
;; such as invoking a binding that doesn't have anything bound to it or trying
;; to exceed the end of the buffer--things like that.  Personally, I do not want
;; such beeping.  Setting this variable to nil still result in beeping, emacs
;; just uses its default function.  Instead, to be disabled it must
;; be set to [[file:snapshots/helpful-command:ignore.png][ignore]].
(setq ring-bell-function #'ignore)
;; Stop freaking out whenever I try to move past beginning or end of the buffer.
;; When you try to move past the beginning and end of a buffer Emacs produces
;; error messages.
;; [[https://emacs.stackexchange.com/questions/10932/how-do-you-disable-the-buffer-end-beginning-warnings-in-the-minibuffer][disable warnings]]
(defun oo-command-error-function (data context caller)
  "Ignore the buffer-read-only, beginning-of-buffer,
end-of-buffer signals; pass the rest to the default handler."
  (unless (memq (car data) '(buffer-read-only beginning-of-buffer end-of-buffer))
    (command-error-default-function data context caller)))

(setq command-error-function #'oo-command-error-function)
;; I like an indentation of 4 spaces; maybe I have gotten used to it with Python.
;; (setq sgml-basic-offset 4)
;; (setq dabbrev-check-all-buffers nil)
;; By default, use `rx' syntax.  It is my preferred syntax.  This is from re-builder.
(defvar reb-re-syntax)
(setq reb-re-syntax 'rx)
;; By default =show-paren-mode= is enabled in all editing mode (non-special
;; modes).  I want to control when to enable this mode normally--as in, add it to
;; hooks myself if I want it enabled.  Therefore, I disable it here.
(setq show-paren-predicate nil)
;; https://olddeuteronomy.github.io/post/some-excerpts-from-my-emacs-config/
;; Don’t use dialog boxes.
(setq use-dialog-box nil)
;; When you disable the scroll-bar via early-init.el powerline does not realize
;; the scroll-bar is dabled because the value of `scroll-bar-mode' is right.
(setq scroll-bar-mode nil)
;; Do not create a custom file.
;; I do not need it.  I'll be honest; to me it seems like the emacs's custom
;; interface is intended for people that do not know elisp.  For me it's completely
;; unnecessary.  Every variable I customize is in my emacs configuration.
;;
;; Do not set this to `null-device' as is recommended by some sites.  That leads
;; to "Maximum buffer size exceeded" error when `custom-save-all' is invoked
;; after installing packages.
(setq custom-file (make-temp-file "custom-file"))
;; Do not ask me for permission to enable a theme
;; By default Emacs will ask you whether you are sure you want to enable a theme
;; as a precaution because a theme could contain malicious code.  Downloading
;; themes with elpaca is safe.  I do not make a habit of grabbing random themes
;; from wierd places online and evaluating them.  So I do not need.
(setq custom-safe-themes t)
;; If non-nil certain commands such as narrowing are disabled.  The idea is that
;; a new user would think that emacs deleted the contents of their file if they
;; accidentally narrowed the buffer.  I am experienced enough so that I do not
;; need this.
(setq disabled-command-function nil)
;; By default =show-paren-mode= is enabled in all editing mode (non-special
;; modes).  I want to control when to enable this mode normally--as in, add it to
;; hooks myself if I want it enabled.  Therefore, I disable it here.
(setq show-paren-predicate nil)
;; Always use spaces instead of tabs
;; https://home.cs.colorado.edu/~main/cs1300/doc/emacs/emacs_24.html
;; https://stackoverflow.com/questions/9383070/tell-emacs-never-to-insert-tabs
(setq-default indent-tabs-mode nil)
;; Do not suggest keybindings for me.
(setq suggest-key-bindings nil)
;; Handle trailing whitespace.
(setq-default show-trailing-whitespace nil)
;; Do not prompt me whether to follow symlinks, just do it.
;; By default Emacs will prompt you when you want to open a file a symlink
;; references.  It will ask you whether you want to follow the symlink.  For me
;; the answer is predominately yes.
(setq vc-follow-symlinks t)
;; Confirm before quitting Emacs.
(setq confirm-kill-emacs #'y-or-n-p)
;; Ensure there's always a newline at the end of files.
;; Several linux programs require a newline at the end of a file, such as
;; chrontab--this is more or less what noctuid said and I'll take his word for
;; it.
(setq require-final-newline t)
;; Do not pass case-insensitive to =auto-mode-alist=.
;; This is taken from =centaur-emacs=.  By default [[file:snapshots/*helpful variable: auto-mode-case-fold*.png][auto-mode-case-fold]] is
;; non-nil; when enabled the auto-mode-alist is traversed twice.  This double
;; traversal can be expensive and it seems unnecessary.
(setq auto-mode-case-fold nil)
;; Stop asking me whether I want to enable file local variables.
;; When installing packages with =quelpa=, I was prompted whether I wanted to apply
;; file local variables.  I'm guessing =straight.el= and =elpaca= disable this.
;; The value safe tells Emacs to only apply the "safe" local variables.  I'm
;; assuming this means ones like "mode" which tell Emacs to open the buffer at a
;; certain major mode.  At first I had this set to nil, but I wanted to open
;; [[][]] in =common-lisp-mode= and I realized Emacs wasn't doing it because I
;; told it not to with this variable.
(setq enable-local-variables :safe)
;; Automatically kill any processes when exiting Emacs.
;; If I start a process, like the =eat= shell for example, stop me from exiting
;; to ask me whether I want to kill it, just do it.
;; https://emacsredux.com/blog/2052/07/18/automatically-kill-running-processes-on-exit/
(setq confirm-kill-processes nil)
;;;; PERFORMANCE
;; https://stackoverflow.com/questions/35658509/gnu-emacs-how-to-disable-prompt-to-save-modified-buffer-on-exit
;; https://emacs.stackexchange.com/questions/22275/save-a-particular-buffer-without-prompting-on-emacs-exit
;; https://stackoverflow.com/questions/6762686/prevent-emacs-from-asking-modified-buffers-exist-exit-anyway
;; if you do not use RTL ever, this could improve perf
;; https://news.ycombinator.com/item?id=39127859
(setq-default bidi-display-reordering 'left-to-right)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
;; https://tychoish.com/post/towards-faster-emacs-start-times/
(setq jit-lock-stealth-time nil)
(setq jit-lock-defer-time nil)
(setq jit-lock-defer-time 0.05)
(setq jit-lock-stealth-load 200)
;; https://olddeuteronomy.github.io/post/some-excerpts-from-my-emacs-config/
;; https://emacs.stackexchange.com/questions/28736/emacs-pointcursor-movement-lag/28746
(setq auto-window-vscroll nil)
;; Improve terminal emulator (vterm/eat) throughput.
(setq read-process-output-max (* 2 1024 1024))
(setq process-adaptive-read-buffering nil)
(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)
(setq inhibit-compacting-font-caches t)
;;;; INITIAL
;; Disable initial scratch message.
;; Don't display any documentation--or any message at all--in the =*scratch*=
;; buffer.  Emacs by default displays a message in the scratch buffer.
(setq initial-scratch-message nil)
;; Set the initial major mode to =fundamental-mode=.
;; This improve startup time because packages enabled for emacs-lisp-mode are not
;; loaded immediately.
(setq initial-major-mode 'fundamental-mode)
;; Do not display Emacs's default startup screen.
;; By default Emacs displays [[][this startup screen]] at startup.  No thanks!  I
;; think these variables are all aliases for eachother.
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)
;;;; STARTUP
;; Disable cursor blinking.
;; By default after a certain amount of blinks the cursor becomes solid.  By
;; setting this to a negative value I make the cursor blink forever.
(blink-cursor-mode -1)
;; Increase the blink interval slightly if I do enable it.
(setq blink-cursor-interval 0.4)
;; Enable window dividers.
(setopt window-divider-default-bottom-width 7)
(setopt window-divider-default-right-width 7)
(setopt window-divider-default-places t)
;;;; TRASH
;; By default Emacs actually deletes files.  By setting this to t, you tell Emacs
;; to move a file to trash instead of actually deleting it.  This is better because
;; if you accidentally delete a file or discover you can still just go get your
;; file from the trash.
(setq delete-by-moving-to-trash t)
;; Designate location of trash.
;; I accidentally sent files to the trash and I could not find them in my trash
;; directory.  I was confused because I knew that the variable
;; [[file:_helpful_variable__delete-by-moving-to-trash_.png][delete-by-moving-to-trash]] was non-nil and I even verified this to be the case
;; with [[file:_helpful_function__helpful-variable_.png][helpful-variable]].  After reading the documentation of [[][]] I realized
;; that emacs uses the [[][]].  To be honest I had no idea what this actually was
;; but I extracted what looked like the location, [[][]].
(setq trash-directory (expand-file-name "~/Trash"))
;;;; BACKUPS
(setq create-lockfiles nil)
;; At first I wanted to completely eschew backups partly because at first it can
;; be hard to disable them and you annoyingly find lots of backup files littered
;; everywhere.
(setq backup-directory-alist '((".*" . "~/Trash/backups")))
;; Do not make backups.
(setq make-backup-files nil)
;; Make numbered backups.
(setq version-control t)
(setq kept-old-versions 2)
(setq kept-new-versions 2)
(setq delete-old-versions t)
;;;; AUTO-SAVING
(setq auto-save-no-message t)
;; Disable auto-save-mode.
(setq auto-save-default t)
;; Stop creating =auto-save-list= directory.
;; See [[https://emacs.stackexchange.com/questions/18677/prevent-auto-save-list-directory-to-be-created][#18677]].
(setq auto-save-list-file-prefix nil)
(setq auto-save-interval 300)
;; Seconds before Emacs saves visited files.
(setq auto-save-timeout 30)
(auto-save-mode -1)
;;; provide
(provide '052-base-settings)
;;; 052-base-settings.el ends here
