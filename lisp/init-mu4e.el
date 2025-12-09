;;; init-mu4e.el --- Initialize mu4e -*- lexical-binding: t; -*-
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
;; Initialize mu4e.
;;
;;; Code:
(require 'base)

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")

(autoload 'mu4e "/usr/share/emacs/site-lisp/mu4e/mu4e" nil nil 'function)
(autoload '+mu4e--main-enter-message "config-mu4e" nil nil 'function)
(autoload '+mu4e--main-leave-message "config-mu4e" nil nil 'function)
(autoload '+mu4e--legacy-enter-message "config-mu4e" nil nil 'function)
(autoload '+mu4e--legacy-leave-message "config-mu4e" nil nil 'function)
(autoload '+mu4e-jump-to-maildir "config-mu4e" nil nil 'function)
(autoload 'o--mail-signature "config-mu4e" nil nil 'function)

(o-defun o--message-signature (&rest _)
  "Produce a signature for a message."
  (insert (string-join '("-- Yours Truly," "Luis M Henriquez-Perez\n") "\n")))

(o-opt message-signature '(funcall #'o--message-signature))

(o-opt mu4e-maildir (expand-file-name "~/.mail"))
(o-opt mu4e-headers-skip-duplicates t)
(o-opt mu4e-view-show-images t)
(o-opt mu4e-view-show-addresses t)
(o-opt mu4e-compose-format-flowed nil)
(o-opt mu4e-headers-date-format "%Y/%m/%d")
(o-opt mu4e-change-filenames-when-moving t)
(o-opt mu4e-attachments-dir (expand-file-name "~/Downloads"))
(o-opt mu4e-compose-signature '(funcall #'o--message-signature))

(o-opt mu4e-get-mail-command "mbsync -a")
(o-opt sendmail-program (executable-find "msmtp"))
(o-opt mail-specify-envelope-from t)
(o-opt message-sendmail-envelope-from 'header)
(o-opt message-send-mail-function   'sendmail-send-it)

;; (require 'smtpmail)
;; (message-send-mail-function . 'smtpmail-send-it)
;; (starttls-use-gnutls . t)
;; (smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil . nil)))
;; (smtpmail-auth-credentials '(("smtp.gmail.com" 587 "USERNAME@gmail.com" . nil)))
;; (smtpmail-default-smtp-server . "smtp.gmail.com")
;; (smtpmail-smtp-server . "smtp.gmail.com")
;; (smtpmail-smtp-service . 587)

(o-opt mu4e-contexts
      (list (make-mu4e-context
             :name "luis@luishp.xyz"
             :enter-func #'+mu4e--main-enter-message
             :leave-func #'+mu4e--main-leave-message
             :vars `((user-mail-address      . "luis@luishp.xyz")
                     (user-full-name         . "Luis M Henriquez")
                     (mu4e-refile-folder     . ,(expand-file-name "/luis@luishp.xyz/archive"))
                     (mu4e-drafts-folder     . ,(expand-file-name "/luis@luishp.xyz/drafts"))
                     (mu4e-sent-folder       . ,(expand-file-name "/luis@luishp.xyz/sent"))
                     (mu4e-trash-folder      . ,(expand-file-name "/luis@luishp.xyz/trash"))
                     (smtpmail-default-smtp-server . "mail.privateemail.com")
                     (smtpmail-smtp-server . "mail.privateemail.com")
                     (smtpmail-smtp-service . 465)
                     (smtpmail-stream-type . ssl)
                     (smtpmail-smtp-user . "luis@luishp.xyz")))
            (make-mu4e-context
             :name "gmail"
             ;; :name "luishenriquezperez@gmail.com"
             :enter-func #'+mu4e--legacy-enter-message
             :leave-func #'+mu4e--legacy-leave-message
             :vars `((user-mail-address      . "luishenriquezperez@gmail.com")
                     (user-full-name         . "Luis M Henriquez")
                     (mu4e-refile-folder     . "/luishenriquezperez@gmail.com/archive")
                     (mu4e-drafts-folder     . ,(expand-file-name "/luishenriquezperez@gmail.com/drafts"))
                     (mu4e-sent-folder       . ,(expand-file-name "/luishenriquezperez@gmail.com/sent"))
                     (mu4e-trash-folder      . ,(expand-file-name "/luishenriquezperez@gmail.com/trash"))
                     (message-send-mail-function . 'smtpmail-send-it)
                     (starttls-use-gnutls . t)
                     (smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil . nil)))
                     (smtpmail-auth-credentials '(("smtp.gmail.com" 587 "USERNAME@gmail.com" . nil)))
                     (smtpmail-default-smtp-server . "smtp.gmail.com")
                     (smtpmail-smtp-server . "smtp.gmail.com")
                     (smtpmail-smtp-service . 587)
                     ;; (smtpmail-default-smtp-server . "smtp.gmail.com")
                     ;; (smtpmail-smtp-server . "smtp.gmail.com")
                     ;; (smtpmail-smtp-service . 587)
                     ;; ;; (smtpmail-smtp-service . 465)
                     ;; (smtpmail-stream-type . ssl)
                     (smtpmail-smtp-user . "luishenriquezperez@gmail.com")))))

;; (o-opt smtpmail-default-smtp-server "smtp.fastmail.com")
;; (o-opt smtpmail-smtp-server         "smtp.fastmail.com")

;; Start with the first (default) context;
;; default is to ask-if-none (ask when there's no context yet, and none match)
(o-opt mu4e-context-policy 'pick-first)

;; compose with the current context is no context matches;
;; default is to ask
(o-opt mu4e-compose-context-policy nil)
;; This allows me to use 'helm' to select mailboxes
(o-opt mu4e-completing-read-function 'completing-read)
;; Why would I want to leave my message open after I've sent it?
(o-opt message-kill-buffer-on-exit t)
;; Don't ask to quit... why is this the default?
(o-opt mu4e-confirm-quit nil)

;; [mu4e] Tip: `user-mail-address' ('luis@luishp.xyz') is not part of mu's addresses; add it with 'mu init
;; --my-address='
;;; provide
(provide 'init-mu4e)
;;; init-mu4e.el ends here
