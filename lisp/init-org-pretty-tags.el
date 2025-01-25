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
             `(("agenda"        . ,(nerd-icons-mdicon "nf-md-view_agenda_outline"))
               ("arch"          . ,(nerd-icons-mdicon "nf-md-arch"))
               ("blog"          . ,(nerd-icons-faicon "nf-fa-blog"))
               ("browser"       . ,(nerd-icons-faicon "nf-fa-internet_explorer"))
               ("bug"           . ,(nerd-icons-faicon "nf-fa-bug"))
               ("css"           . ,(nerd-icons-devicon "nf-dev-css3"))
               ("cursor"        . ,(nerd-icons-mdicon "nf-md-cursor_default_outline"))
               ("emacs"         . ,(nerd-icons-sucicon "nf-custom-emacs"))
               ("email"         . ,(nerd-icons-mdicon "nf-md-email"))
               ("eshell"        . ,(nerd-icons-devicon "nf-dev-terminal"))
               ("evil"          . ,(nerd-icons-mdicon "nf-md-emoticon_devil"))
               ("font"          . ,(nerd-icons-sucicon "nf-seti-font"))
               ("function"      . ,(nerd-icons-mdicon "nf-md-function"))
               ("git"           . ,(nerd-icons-mdicon "nf-md-git"))
               ("github"        . ,(nerd-icons-faicon "nf-fa-github"))
               ("gym"           . ,(nerd-icons-faicon "nf-fa-dumbbell"))
               ("harddrive"     . ,(nerd-icons-faicon "nf-fa-hard_drive"))
               ("idea"          . ,(nerd-icons-faicon "nf-fa-lightbulb"))
               ("job"           . ,(nerd-icons-faicon "nf-fa-suitcase"))
               ("keybinding"    . ,(nerd-icons-mdicon "nf-md-keyboard"))
               ("linux"         . ,(nerd-icons-faicon "nf-fa-linux"))
               ("log"           . ,(nerd-icons-octicon "nf-oct-log"))
               ("modeline"      . ,(nerd-icons-faicon "nf-fa-grip_lines"))
               ("money"         . ,(nerd-icons-faicon "nf-fa-money_bill_1"))
               ("music"         . ,(nerd-icons-faicon "nf-fa-music"))
               ("online"        . ,(nerd-icons-mdicon "nf-md-web"))
               ("org"           . ,(nerd-icons-sucicon "nf-custom-orgmode"))
               ("package"       . ,(nerd-icons-codicon "nf-cod-package"))
               ("performance"   . ,(nerd-icons-faicon "nf-fa-line_chart"))
               ("phonecall"     . ,(nerd-icons-mdicon "nf-md-cellphone"))
               ("phonetext"     . ,(nerd-icons-mdicon "nf-md-cellphone_text"))
               ("python"        . ,(nerd-icons-mdicon "nf-md-arch"))
               ("question"      . ,(nerd-icons-faicon "nf-fa-question"))
               ("screenshot"    . ,(nerd-icons-mdicon "nf-md-camera"))
               ("secret"        . ,(nerd-icons-faicon "nf-fa-user_secret"))
               ("theme"         . ,(nerd-icons-mdicon "nf-md-theme_light_dark"))
               ("thought"       . ,(nerd-icons-mdicon "nf-md-thought_bubble_outline"))
               ("toggle"        . ,(nerd-icons-faicon "nf-fa-toggle_on"))
               ("void"          . ,(nerd-icons-flicon "nf-linux-void"))
               ("windowmanager" . ,(nerd-icons-mdicon "nf-md-dock_window"))
               ("account"       . ,(nerd-icons-mdicon "nf-md-account")))))
;;; provide
(provide 'init-org-pretty-tags)
;;; init-org-pretty-tags.el ends here
