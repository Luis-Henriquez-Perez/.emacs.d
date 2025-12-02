;;; init-yeetube.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; TODO: add commentary
;;
;;; Code:
;; It is much easier to see which item I am on when the lines are highlighted.
;; Also, confirmed that this hook does not work from the yeetube config file.
(add-hook 'yeetube-mode-hook #'hl-line-mode)

(opt! yeetube-download-directory (expand-file-name "~/Videos/"))

(nmap! yeetube-mode-map "p" #'yeetube-play)
(nmap! yeetube-mode-map "a" #'o-yeetube-download-audio)
(nmap! yeetube-mode-map "v" #'o-yeetube-download-video)
(nmap! yeetube-mode-map "s" #'yeetube-search)
;;; provide
(provide 'init-yeetube)
;;; init-yeetube.el ends here
