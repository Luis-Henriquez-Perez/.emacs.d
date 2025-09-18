;;; 990-config-eshell.el --- configuration for eshell -*- lexical-binding: t; -*-
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
;; This is my configuration for eshell.
;;
;;; Code:
(require 'eshell-z)
(require 'eshell-up)
(require 'em-alias)
(require! "^0[01]")
(require 'vc-git)
;;;; prompt function
(defun! oo-eshell-prompt ()
  (set! path (abbreviate-file-name default-directory))
  (set! branch (aand! (car-safe (vc-git-branches)) (format "[ %s ]" it)))
  ;; Get the current time.
  (set! time (format-time-string "%H:%M"))
  (string-join (delq nil (list time path branch "λ\s")) "\s"))
;;;; clear
;; TODO: make into a snippet and/or abbrev
;; (message "current buffer %S" (buffer-name))
;; TODO: edit surrounding form so that it works in comments
;; (message "var %S" var)
;; Unexpectedly for me the eshell clear scrolled to the bottom.  As seen in a
;; stackoverflow answer as well as multiple blog posts, the solution is to use
;; "clear 1" instead, essentually telling emacs to use "clear-scrollback".  I
;; still do not like this though because it actually erases the contents of the
;; buffer and I do not want to do this unnecessarily.  I just want it to scroll
;; up.  I figured out why.
(defun oo-scroll-to-top-h (&rest _)
  "Hook that scrolls eshell to top of window."
  (recenter 0)
  (remove-hook 'eshell-post-command-hook #'oo-scroll-to-top-h 'local))

(defun eshell/scroll-to-top ()
  "Scroll the Eshell to the top without clearing the buffer."
  (add-hook 'eshell-post-command-hook #'oo-scroll-to-top-h nil 'local))

;; Replace `eshell/clear' with this function.
;; (defalias 'eshell/clear 'eshell/scroll-to-top)

;; I do not necessarily want to always scroll to the top but I want eshell to
;; preserve it is previous position in the window.
;;;; eshell
(defun! eshell/less (&rest files)
  "Essentially an alias to the `view-file' function."
  (set! (first . rest) files)
  (when files
	(view-file first)
	(when rest
	  (mapc #'view-file-other-window rest))))
;;;; settings
(setopt eshell-banner-message "")
(setopt eshell-highlight-prompt nil)
;; For now outsource to epe, but later I will make my own.  Also epe uses static
;; faces by which I mean constant faces, not existing ones that change with
;; themes.  So the prompt is difficult to read with certain themes, particularly
;; light themes.
(setopt eshell-prompt-function 'oo-eshell-prompt)
;; This is obsolete as of Emacs 30.1.
(setopt eshell-prompt-regexp "^[^λ]+λ ")
(setopt eshell-hist-ignoredups t)
;; Prefer elisp equivalents over system commands.  Mainly I like `eshell/rm'
;; which moves a file to trash instead of deleting it.  The downside is that
;; the notmuch Emacs package provides a command `notmuch' which clashes with the
;; system command which I use predominately.
(setopt eshell-prefer-lisp-functions t)
;; Represent buffers as #<buffer-name>
(setopt eshell-buffer-shorthand t)
;; boost eshell history-size
;; Increase the history size from 128 to 1000.
(setopt eshell-history-size 1000)
;; By "highlight" eshell does not just mean coloring the font with the
;; `eshell-prompt' face.  It also makes the prompt read-only.  Strangely, the
;; prompt is not read-only by default.  Furthermore, there is no way to override
;; the text properties `eshell-emit-prompt' adds to the prompt without advising
;; the it.
(setopt eshell-highlight-prompt t)
(setopt eshell-hist-ignoredups t)
;; boost eshell history-size
;; Increase the history size from 128 to 1000.
(setopt eshell-history-size 1000)
;; Stop eshell from printing messages.
;;;; keybindings
(nmap eshell-mode-map "J" #'eshell-next-prompt)
(nmap eshell-mode-map "K" #'eshell-previous-prompt)
;;;; aliases
(eshell/alias "home" "cd ~")
;;;;; git
(eshell/alias "gs" "git status --porcelain")
(eshell/alias "gad" "dot add $1 && dot commit -m \"Add $1.\" $1 && dot push")
(eshell/alias "gadd" "dot add $1 && dot commit -m \"Add $1.\" $1 && dot push")
;; https://stackoverflow.com/questions/927358/how-do-i-undo-the-most-recent-local-commits-in-git#927386
(eshell/alias "gundo" "git reset HEAD~")
(eshell/alias "git-undo" "git reset HEAD~")
(eshell/alias "delete-branch" "git branch -D $1 && git push origin --delete $1")
(eshell/alias "branch" "git checkout -b $1 && git push -u origin $1")
;; (eshell/alias "branch" "git fetch -a && git checkout")
;; "git checkout --track origin/<branch-name>"
(eshell/alias "remote-branch" "git fetch -a && git checkout -b $1 && git branch --set-upstream-to=origin/$1")
;; https://stackoverflow.com/questions/1441010/the-shortest-possible-output-from-git-log-containing-author-and-date
(eshell/alias "glog" "git --no-pager log -n 5 --pretty=format:'%h %ad %s' --date=format:'%Y-%m-%d %H:%M:%S'")
(eshell/alias "grn" "rename-file $1 $2 && git add $1 $2 && git commit -m \"Rename $1 -> $2\" && git push")
(eshell/alias "gls" "git ls-files $1")
(eshell/alias "cherry" "git cherry-pick $* && git push --force")
(eshell/alias "delete-remote" "git push origin --delete $1")
(eshell/alias "delrem" "git push origin --delete $1")
(eshell/alias "remotes" "git --no-pager branch -r")
(eshell/alias "checkout" "git checkout $*")
(eshell/alias "check" "git checkout $*")
(eshell/alias "grename" "git branch -m $1 && git push origin -u $1 && git push origin --delete <old-branch-name>")
(eshell/alias "rename" "move $*")
(eshell/alias "home" "cd ~")
(eshell/alias "exec" "chmod +x $*")
(eshell/alias "symlink" "ln -s $1 $2")
(eshell/alias "sym" "ln -s $1 $2")
(eshell/alias "mmv" "mkdir -p \"$(dirname $2)\" && mv $1 $2")
(eshell/alias "unpack" "mv $1/* . && rmdir $1")
(eshell/alias "lockfiles" "find . -name '*~'")
(eshell/alias "delete-lockfiles" "find . -name '*~'")
(eshell/alias "html-to-pdf" "wkhtmltopdf $1")
;;;;; emacs maintenance
(eshell/alias "emacs-test" "{cd $user-emacs-directory; eldev -d test $1}")
(eshell/alias "etest" "(let ((default-directory user-emacs-directory)) ${eldev -d test $1})")
(eshell/alias "estatus" "(let ((default-directory user-emacs-directory)){eldev -d test $1})")
(eshell/alias "emacs-compile" "{cd $user-emacs-directory; eldev -d compile $1}")
(eshell/alias "emacs-eval" "{cd $user-emacs-directory; eldev -d eval $1}")
(eshell/alias "eclean" "{cd $user-emacs-directory; eldev clean}")
(eshell/alias "eeval" "{cd $user-emacs-directory; eldev -d eval $1}")
(eshell/alias "update-emacs" "apply-emacs && eclean && ecompile")
(eshell/alias "ecompile" "emacs --batch -l ~/.config/emacs/compile-setup.el -f batch-byte-compile ~/.config/emacs/init.el ~/.config/emacs/early-init.el ~/.config/emacs/lisp/*.el")
(eshell/alias "eclean" "rm -f ~/.config/emacs/init.elc ~/.config/emacs/early-init.elc ~/.config/emacs/lisp/*.elc")
;;;;; archlinux
(eshell/alias "pmi" "sudo pacman -S --needed --noconfirm $*")
(eshell/alias "pmr" "sudo pacman -Rns --noconfirm $*")
(eshell/alias "pmro" "sudo pacman -Rns --noconfirm { pacman -Qdt }")
(eshell/alias "pmu" "sudo pacman -Syu")
(eshell/alias "pms" "sudo pacman -Ssq $*")
(eshell/alias "pmf" "sudo pacman -Ql $*")
;; (eshell/alias "c" "clear")
(eshell/alias "wifi" "nmcli dev wifi list")
(eshell/alias "sd" "systemctl $*")
;;;;; miscellaneous
(eshell/alias "iso" "sudo dd if=$1 of=$2 bs=4M status=progress")
(eshell/alias "up" "eshell-up $1")
(eshell/alias "pk" "eshell-up-peek $1")
(eshell/alias "unpack" "mv $1/* . && rmdir $1")
(eshell/alias "html-to-org" "pandoc -f html -t org $1 -o $2")
(eshell/alias "clear" "eshell/clear t")
(eshell/alias "ff" "fo $1")
(eshell/alias "fo" "find-file-other-window $1")
(eshell/alias "ffow" "find-file-other-window $1")
(eshell/alias "open" "find-file $1")
(eshell/alias "d" "dired $1")
;;; provide
(provide '990-config-eshell)
;;; 990-config-eshell.el ends here
