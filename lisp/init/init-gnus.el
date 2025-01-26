;;; init-gnus.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSESee the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this programIf not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; TODO: add commentary
;;
;;; Code:
(opt! gnus-summary-line-format "%U%R%z %-16,16&user-date; %-30,30f  %B%S\n")
(opt! gnus-select-method '(nnnil))
(opt! gnus-agent t)
(opt! gnus-agent-article-alist-save-format 1)
(opt! gnus-agent-cache t)
(opt! gnus-agent-confirmation-function 'y-or-n-p)
(opt! gnus-agent-consider-all-articles  nil)
(opt! gnus-agent-directory "~/News/agent/")
(opt! gnus-agent-enable-expiration 'ENABLE)
(opt! gnus-agent-expire-all nil)
(opt! gnus-agent-expire-days 30)
(opt! gnus-agent-mark-unread-after-downloaded t)
(opt! gnus-agent-queue-mail t)
(opt! gnus-agent-synchronize-flags nil)

(opt! gnus-article-browse-delete-temp 'ask)
(opt! gnus-article-over-scroll nil)
(opt! gnus-article-show-cursor t)
(opt! gnus-article-sort-functions
      '(gnus-article-sort-by-most-recent-number
        gnus-article-sort-by-most-recent-date))
(opt! gnus-article-truncate-lines nil)
;; (gnus-html-frame-width 80)
;; (gnus-html-image-automatic-caching t)
(opt! gnus-inhibit-images t)
(opt! gnus-max-image-proportion 0.3)
(opt! gnus-treat-display-smileys nil)
;; (gnus-article-mode-line-format "%G %S %m")
(opt! gnus-visible-headers
      '("^From:" "^To:" "^Cc:" "^Newsgroups:" "^Subject:" "^Date:"
        "Followup-To:" "Reply-To:" "^Organization:" "^X-Newsreader:"
        "^X-Mailer:"))
(opt! gnus-sorted-header-list gnus-visible-headers);;; provide

(provide 'init-gnus)
;;; init-gnus.el ends here
