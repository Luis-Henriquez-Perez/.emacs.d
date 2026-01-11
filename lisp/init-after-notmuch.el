;;; init-after-notmuch.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
;; Several functions to help me .
;;
;;; Code:
(require 'notmuch)

;; These functions allow me to access my inboxes much faster.
(defun o-notmuch-search-luishp-inbox ()
  (interactive)
  (notmuch-search "path:luis@luishp.xyz/** and tag:inbox"))

(defun o-notmuch-search-yadira-inbox ()
  (interactive)
  (notmuch-search "path:yadiraperez2029@gmail.com/** and tag:inbox"))

(defun o-notmuch-search-gmail-inbox ()
  (interactive)
  (notmuch-search "path:luishenriquezperez@gmail.com/** and tag:inbox"))

(defun o-notmuch-show-mark-as-spam ()
  "Mark message with the send of the current message as spam."
  (interactive)
  ;; (message "from:%S" (notmuch-show-get-from))
  ;; (message "result -> %S" (notmuch-search-get-result))
  ;; (message "from:%S" (plist-get (notmuch-search-get-result) :from))
  ;; (notmuch-tag "from:%S" "+spam-inbox+deleted")
  )

;; Also show me all the emails from the address at point.
(defun o-notmuch-show-mail ()
  "Mark message with the send of the current message as spam."
  (interactive)
  ;; (message "from:%S" (notmuch-show-get-from))
  (message "result -> %S" (notmuch-search-get-result))
  ;; (message "from:%S" (plist-get (notmuch-search-get-result) :from))
  ;; (notmuch-tag "from:%S" "+spam-inbox+deleted")
  )

(o-defun o-notmuch-search-email-at-point ()
  "Open a notmuch search of the current message."
  (interactive)
  ;; (message "email -> %s" (substring (thing-at-point 'email) 1 -1))
  (o-set email (substring (thing-at-point 'email) 1 -1))
  (message "email -> %s" email)
  (o-set query (format "tag:inbox path:luishenriquezperez@gmail.com/** from:%s" email))
  (notmuch-search query))

(o-defun o-notmuch-tag-spam ()
  "Mark message with the send of the current message as spam."
  (interactive)
  ;; Get the email at point.
  (o-set email (substring (thing-at-point 'email) 1 -1))
  (message "email -> %s" email)
  (o-set query (format "tag:inbox path:luishenriquezperez@gmail.com/** from:%s" email))
  (notmuch-tag query '("+deleted" "+spam" "-inbox")))

(o-defun o-notmuch-tag-delete ()
  "Mark message with the send of the current message as spam."
  (interactive)
  ;; Get the email at point.
  (o-set email (substring (thing-at-point 'email) 1 -1))
  (message "email -> %s" email)
  (o-set query (format "tag:inbox path:luishenriquezperez@gmail.com/** from:%s" email))
  (notmuch-tag query '("+deleted" "-inbox")))

;; (notmuch-search "tag:inbox and path:yadiraperez2029@gmail.com/**")
;; (notmuch-search "tag:spam and path:yadiraperez2029@gmail.com/**")
;;; provide
(provide 'init-after-notmuch)
;;; init-after-notmuch.el ends here
