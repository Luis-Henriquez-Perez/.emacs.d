;;; init-pkg-notmuch.el --- initialize notmuch -*- lexical-binding: t; -*-
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
;; Initialize notmuch.
;;
;;; Code:
(require 'init-core)

(o-declare-package 'notmuch)

;; If you do not use `setq-default' this setting will not persist across
;; different searches.
(setq-default notmuch-search-oldest-first nil)
;; (notmuch-search "from:shein@news.edmmarket.shein.com")
;; (notmuch-search "tag:inbox and path:yadiraperez2029@gmail.com/**")
;; (notmuch-search "tag:spam and path:yadiraperez2029@gmail.com/**")
;; (notmuch-search "tag:inbox and path:luis@luishp.xyz/**")
;; (notmuch-search "tag:inbox and path:luishenriquezperez@gmail.com/**")
;;; provide
(provide 'init-pkg-notmuch)
;;; init-pkg-notmuch.el ends here
