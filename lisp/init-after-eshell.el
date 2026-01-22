;;; init-after-eshell.el --- configuration for eshell -*- lexical-binding: t; -*-
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
(require 'eshell)
(require 'em-alias)
(require 'init-core)
(require 'vc-git)

;; Save eshell history periodically instead of just when killing Emacs to ensure
;; that any data that might be lost from Emacs crashing is minimized.
(add-hook 'kill-emacs-hook #'eshell-save-some-history)

(defvar o--eshell-save-hist-timer nil
  "Timer for saving eshell history.")

(defun o-hook--save-eshell-history (prev-buff _)
  "Save eshell history during idle time if leaving eshell buffer."
  (when (with-current-buffer prev-buff (derived-mode-p 'eshell-mode))
    (unless o--eshell-save-hist-timer
      (setq o--eshell-save-hist-timer (run-with-idle-timer 5 nil #'o-timer--eshell-save-hist)))))

(defun o-timer--eshell-save-hist ()
  "Timer for saving eshell history."
  (eshell-save-some-history)
  (setq o--eshell-save-hist-timer nil))

(add-hook 'o-switch-buffer-hook #'o-hook--save-eshell-history)
;;;; prompt function
(o-defun o-eshell-prompt ()
  (o-set path (abbreviate-file-name default-directory))
  (o-set branch (o-aand (car-safe (vc-git-branches)) (format "[ %s ]" it)))
  ;; Get the current time.
  (o-set time (format-time-string "%H:%M"))
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
(defun o-hook--scroll-eshell-to-top (&rest _)
  "Hook that scrolls eshell to top of window."
  (recenter 0)
  (remove-hook 'eshell-post-command-hook #'o-hook--scroll-eshell-to-top 'local))

(defun eshell/scroll-to-top ()
  "Scroll the Eshell to the top without clearing the buffer."
  (add-hook 'eshell-post-command-hook #'o-hook--scroll-eshell-to-top nil 'local))

;; Replace `eshell/clear' with this function.
;; (defalias 'eshell/clear 'eshell/scroll-to-top)

;; I do not necessarily want to always scroll to the top but I want eshell to
;; preserve it is previous position in the window.
;;;; eshell
(o-defun eshell/less (&rest files)
  "Essentially an alias to the `view-file' function."
  (o-set (first . rest) files)
  (when files
	(view-file first)
	(when rest
	  (mapc #'view-file-other-window rest))))
;;;; aliases
(eshell/alias "home" "cd ~")
;;;;; git
(eshell/alias "gs" "git status --porcelain")
(eshell/alias "gd" "git diff $*")
(eshell/alias "gb" "git branch")
;; I was thinking of pushing as well but pushing takes too long.
(eshell/alias "gac" "git add $1 && git commit $1 -m \"$2\"")
(eshell/alias "gacc" "git add $1 && git commit $1 -m \"Update $1.\"")
(eshell/alias "gp" "git push")
(eshell/alias "gca" "git commit --amend")
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
(eshell/alias "ga" "git add $*")
(eshell/alias "gcp" "git cherry-pick $*")
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
(eshell/alias "eclean" "{cd $user-emacs-directory; eldev clean}")
(eshell/alias "eeval" "{cd $user-emacs-directory; eldev -d eval $1}")
(eshell/alias "ecompile" "emacs --batch -l ~/.config/emacs/compile-setup.el -f batch-byte-compile ~/.config/emacs/init.el ~/.config/emacs/early-init.el ~/.config/emacs/lisp/*.el")
(eshell/alias "eclean" "rm -f ~/.config/emacs/init.elc ~/.config/emacs/early-init.elc ~/.config/emacs/lisp/*.elc")
;;;;; archlinux
(eshell/alias "pmi" "sudo pacman -S --needed --noconfirm $*")
(eshell/alias "pmr" "sudo pacman -Rns --noconfirm $*")
(eshell/alias "pmro" "pacman -Rns {pacman -Qdtq}")
(eshell/alias "pmu" "sudo pacman -Syu")
(eshell/alias "pms" "pacman -Ssq $*")
(eshell/alias "ys" "yay -Ssq $*")
(eshell/alias "pmf" "pacman -Ql $*")
(eshell/alias "pmii" "pacman -Qi $*")
;; (eshell/alias "c" "clear")
(eshell/alias "wifi" "nmcli dev wifi list")
(eshell/alias "sd" "systemctl $*")
(eshell/alias "sdu" "systemctl --user $*")
(eshell/alias "sdut" "systemctl --user list-timers")
(eshell/alias "timers" "systemctl --user list-timers")
(eshell/alias "utimers" "systemctl --user list-timers")
;;;;; miscellaneous
(eshell/alias "iso" "sudo dd if=$1 of=$2 bs=4M status=progress")
(eshell/alias "unpack" "mv $1/* . && rmdir $1")
(eshell/alias "clear" "eshell/clear t")
(eshell/alias "ff" "fo $1")
(eshell/alias "fo" "find-file-other-window $1")
(eshell/alias "ffow" "find-file-other-window $1")
(eshell/alias "d" "dired $1")
;;; provide
(provide 'init-after-eshell)
;;; init-after-eshell.el ends here
