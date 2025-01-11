;;; config-em-alias.el --- em-alias configuration -*- lexical-binding: t; -*-
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
;; This file contains all of my eshell aliases.
;;
;;; Code:
;;;; requirements
(require 'em-alias)
;;;; git
(eshell/alias "gad" "dot add $1 && dot commit -m \"Add $1.\" $1 && dot push")
(eshell/alias "gadd" "dot add $1 && dot commit -m \"Add $1.\" $1 && dot push")
(eshell/alias "gs" "git status --porcelain")
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
;;
;; (eshell/alias "" "find . -name '*~'")
;;;; rsync
;; Communicate with my desktop.
;; (eshell/alias "rpush" "rsync -a $1 luis@yadira:$2")
;; (eshell/alias "rpull" "rsync -a luis@yadira:$1 $2")
;;;; emacs maintenance
(eshell/alias "emacs-test" "{cd $user-emacs-directory; eldev -d test $1}")
(eshell/alias "etest" "(let ((default-directory user-emacs-directory)) ${eldev -d test $1})")
(eshell/alias "estatus" "(let ((default-directory user-emacs-directory)){eldev -d test $1})")
(eshell/alias "emacs-compile" "{cd $user-emacs-directory; eldev -d compile $1}")
(eshell/alias "ecompile" "{cd $user-emacs-directory; eldev -d compile $1}")
(eshell/alias "emacs-eval" "{cd $user-emacs-directory; eldev -d eval $1}")
(eshell/alias "eclean" "{cd $user-emacs-directory; eldev clean}")
(eshell/alias "eeval" "{cd $user-emacs-directory; eldev -d eval $1}")
(eshell/alias "apply-emacs" "chezmoi apply ~/.config/emacs --force")
(eshell/alias "update-emacs" "apply-emacs && eclean && ecompile")
;;;; archlinux
(eshell/alias "orphan" "pacman -Qtd $*")
(eshell/alias "files" "pacman -Ql $1")
(eshell/alias "pac" "sudo pacman --noconfirm $*")
(eshell/alias "install" "sudo pacman -S --noconfirm $*")
(eshell/alias "remove" "sudo pacman -Rns --noconfirm $*")
(eshell/alias "uninstall" "sudo pacman -Rns --noconfirm $*")
(eshell/alias "search" "pacman -Ss $*")
(eshell/alias "search-quiet" "pacman -Ssq $*")
(eshell/alias "update" "sudo pacman -Syu")
(eshell/alias "update-system" "sudo pacman -Syu")
(eshell/alias "update-email" "mbsync -a")
(eshell/alias "list-wifi" "nmcli dev wifi list")
(eshell/alias "listwifi" "nmcli dev wifi list")
;;;; blogging
(eshell/alias "publish" "{cd $(expand-file-name \"html\" \"~/Documents/blog\") ; (shut-up (org-publish \"blog\" t))}")
(eshell/alias "epublish" "{cd $(expand-file-name \"html\" \"~/Documents/blog\") ; (shut-up (org-publish \"blog\" t))}")
;;;; miscellaneous
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
(provide 'config-em-alias)
;;; config-em-alias.el ends here
