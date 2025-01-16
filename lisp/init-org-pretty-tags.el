;;; init-org-pretty-tags.el --- setup org-pretty-tags -*- lexical-binding: t; -*-
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
;; Configure.
;;
;;; Code:
(hook! org-mode-hook org-pretty-tags-mode)

(opt! org-pretty-tags-surrogate-strings
      (progn (require 'nerd-icons)
             `(("idea"          . ,(nerd-icons-faicon "nf-fa-lightbulb"))
               ("function"      . ,(nerd-icons-mdicon "nf-md-function"))
               ("modeline"      . ,(nerd-icons-faicon "nf-fa-grip_lines"))
               ("github"        . ,(nerd-icons-faicon "nf-fa-github"))
               ("git"           . ,(nerd-icons-mdicon "nf-md-git"))
               ("phonecall"     . ,(nerd-icons-mdicon "nf-md-cellphone"))
               ("phonetext"     . ,(nerd-icons-mdicon "nf-md-cellphone_text"))
               ("online"        . ,(nerd-icons-mdicon "nf-md-web"))
               ("email"         . ,(nerd-icons-mdicon "nf-md-email"))
               ("secret"        . ,(nerd-icons-faicon "nf-fa-user_secret"))
               ("agenda"        . ,(nerd-icons-mdicon "nf-md-view_agenda_outline"))
               ("money"         . ,(nerd-icons-faicon "nf-fa-money_bill_1"))
               ("arch"          . ,(nerd-icons-mdicon "nf-md-arch"))
               ("blog"          . ,(nerd-icons-faicon "nf-fa-blog"))
               ("eshell"        . ,(nerd-icons-devicon "nf-dev-terminal"))
               ("python"        . ,(nerd-icons-mdicon "nf-md-arch"))
               ("linux"         . ,(nerd-icons-faicon "nf-fa-linux"))
               ("question"      . ,(nerd-icons-faicon "nf-fa-question"))
               ("void"          . ,(nerd-icons-flicon "nf-linux-void"))
               ("keybinding"    . ,(nerd-icons-mdicon "nf-md-keyboard"))
               ("bug"           . ,(nerd-icons-faicon "nf-fa-bug"))
               ("org"           . ,(nerd-icons-sucicon "nf-custom-orgmode"))
               ("gym"           . ,(nerd-icons-faicon "nf-fa-dumbbell"))
               ("emacs"         . ,(nerd-icons-sucicon "nf-custom-emacs"))
               ("windowmanager" . ,(nerd-icons-mdicon "nf-md-dock_window"))
               ("job"           . ,(nerd-icons-faicon "nf-fa-suitcase"))
               ("evil"          . ,(nerd-icons-mdicon "nf-md-emoticon_devil"))
               ("thought"       . ,(nerd-icons-mdicon "nf-md-thought_bubble_outline"))
               ("performance"   . ,(nerd-icons-faicon "nf-fa-line_chart"))
               ("music"         . ,(nerd-icons-faicon "nf-fa-music")))))
;;; provide
(provide 'init-org-pretty-tags)
;;; init-org-pretty-tags.el ends here
