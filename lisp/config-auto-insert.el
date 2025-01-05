;;; config-auto-insert.el --- Configure auto-insert -*- lexical-binding: t; -*-
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
;; Configure auto-insert.
;;
;;; Code:
(require 'base)
(require 'f)

(defun oo-copyright-license ()
  "Return the copyright license."
  (string-join (list ";;"
                     ";; Copyright (c) 2024 Free Software Foundation, Inc."
                     ";;"
                     ";; Author: Luis Henriquez-Perez <luis@luishp.xyz>"
                     ;; If the author is the same person as the maintainer, I do not need to specify them.
                     ;; ";; Maintainer: Luis Henriquez-Perez <luis@luishp.xyz>"
                     ;; In the manual its recommended not to write the version on every file, just the main one.
                     ;; ";; Version: 0.1"
                     ;; According to the linter, =package-requires= should only be on the main elisp file.
                     ;; ";; Package-Requires: ((emacs \"29.1\"))"
                     ";; Homepage: https://github.com/Luis-Henriquez-Perez/dotfiles/"
                     ";;"
                     ";; This file is not part of GNU Emacs."
                     ";;"
                     ";; This program is free software; you can redistribute it and/or"
                     ";; modify it under the terms of the GNU General Public License as"
                     ";; published by the Free Software Foundation, either version 3 of the"
                     ";; License, or (at your option) any later version."
                     ";;"
                     ";; This program is distributed in the hope that it will be useful, but"
                     ";; WITHOUT ANY WARRANTY; without even the implied warranty of"
                     ";; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU"
                     ";; General Public License for more details."
                     ";;"
                     ";; You should have received a copy of the GNU General Public License"
                     ";; along with this program. If not, see <http://www.gnu.org/licenses/>."
                     ";;"
                     "")
               "\n"))

(defun oo-ensure-provide ()
  (interactive)
  (oo--ensure-provide (buffer-file-name)))

(defun oo-header-regexp ()
  "Return the regular expression for an emacs package header."
  (rx-to-string '(: bos ";;;" (one-or-more space)
                    (group (one-or-more (not space)))
                    ".el" (one-or-more space)
                    "---"
                    (one-or-more space)
                    (1+ nonl)
                    "-*- lexical-binding: t; -*-\n")))

(defun oo--ensure-provide (file)
  "Ensure FILE ends with proper provide footer."
  (let* ((feature (file-name-sans-extension (file-name-nondirectory file)))
         (provide-name feature)
         (top-rx (rx ";;; provide\n"))
         (provide-rx (rx bol "(provide '" (group (1+ (not (any "\n")))) ")" eol))
         (footer-commentary (format ";;; %s.el ends here\n" feature))
         (footer-rx "\\(\n*\\)\\(?:^\\);;; \\([^[:blank:]]+\\)\\.el ends here\\(\n*\\)\\'"))
    (with-current-buffer (find-file-noselect file)
      ;; Look for provide form.
      (cond ((re-search-forward provide-rx nil t)
             (setq provide-name (match-string 1))
             ;; If it's there, make sure that the feature is correct.  If it is not
             ;; correct, fix it.  Then change.
             (unless (equal provide-name feature)
               (replace-match feature nil nil nil 1))
             ;; Go to the beginning and check if there's a provide header.  If
             ;; not, add it.
             (goto-char (match-beginning 0))
             (unless (save-match-data (looking-back top-rx (point-min)))
               (insert ";;; provide\n"))
             ;; Now check the end of it, and add the eof-comment afterwards if
             ;; needed.
             (goto-char (line-end-position))
             (if (looking-at footer-rx)
                 (unless (equal (match-string 2) feature)
                   (replace-match "\n" nil nil nil 1)
                   (replace-match feature nil nil nil 2)
                   (replace-match "\n" nil nil nil 3))
               (save-excursion (insert "\n")
                               (insert footer-commentary))))
            (t
             ;; Remove an eof comment if there is one.  And remove a header at
             ;; the end if there is one.  We will re-add it now.
             (goto-char (point-max))
             (insert (format ";;; provide\n(provide '%s)\n%s" feature footer-commentary)))))))

(defun oo--ensure-file-header ()
  "Ensure that file has a title, description."
  ;; Make sure that the file has a title.
  (let* ((file (buffer-file-name))
         (filename (file-name-sans-extension (file-name-nondirectory file)))
         (header-rx (oo-header-regexp))
         (lisence-rx (rx-to-string (oo-copyright-license))))
    (save-excursion
      (goto-char (point-min))
      (if (looking-at header-rx)
          (progn (replace-match filename nil 'literal nil 1)
                 (goto-char (match-end 0)))
        (insert (format ";;; %s.el --- TODO: add commentary -*- lexical-binding: t; -*-\n" filename)))
      ;; Ensure license.
      (unless (looking-at lisence-rx)
        (insert (oo-copyright-license)))
      ;; Ensure commentary.
      (if (looking-at ";;; Commentary:\n\\(?:\\(?:^;;$\\)\n\\|\\(?:^;;[^;].*$\\)\n\\)*")
          (goto-char (match-end 0))
        (insert ";;; Commentary:\n;;\n;; TODO: add commentary\n;;\n"))
      (if (looking-at "\\`;;;[[:blank:]]Code:\n")
          (goto-char (match-end 0))
        (insert ";;; Code:\n")))))

(defun! oo-auto-insert-elisp-template ()
  "Insert emacs-lisp template in file."
  (set! path (buffer-file-name))
  (set! base (file-name-sans-extension (file-name-nondirectory (directory-file-name path))))
  (set! lisp-dir "~/.config/emacs/lisp/")
  (when (and (f-child-of-p path lisp-dir)
             (string-match "\\`\\(\\(?:config\\|init\\)\\)-\\(.+\\)" base))
    (set! feature (match-string 2 base))
    (set! verb (pcase (match-string 1 base)
                 ("config" "Configure")
                 ("init" "Initialize")))
    (set! comment1 (format "%s %s" verb feature))
    (set! comment2 (format "%s %s." verb feature))
    (oo--ensure-file-header)
    (goto-char (point-min))
    ;; This is a kind of roundabout way of doing it.  Not sure if it is the
    ;; "best" way whatever that means, but it works.
    (search-forward "TODO: add commentary" nil t nil)
    (replace-match comment1)
    (search-forward "TODO: add commentary" nil t nil)
    (replace-match comment2)
    (save-excursion (oo--ensure-provide path))))

(defun! oo-auto-insert-html-template ()
  "Insert html template in file."
  (interactive)
  (require 'tempel)
  (tempel-insert '("<!doctype html>" n
                   "<html lang=\"en\">" > n
                   "<head>" > n
                   "<meta charset=\"UTF-8\"/>" > n
                   "<title>" p "</title>" > n
                   "</head>" > n
                   "<body>" > n
                   "</body>" > n
                   "</html>")))

(defun oo-auto-insert-python-file-header ()
  "Insert python file header."
  (require 'tempel)
  (tempel-insert '("# Filename: " (file-name-nondirectory (directory-file-name (buffer-file-name))) > n
                   "# Author: " user-full-name " <" user-mail-address ">" > n
                   "# Created: " (format-time-string "%Y-%m-%d %H:%M:%S") > n
                   "# Description: " p > n)))

(defun oo-auto-insert-bash-file-header ()
  "Insert bash file header."
  (require 'tempel)
  (tempel-insert '("#!/bin/bash" > n
                   "# Filename: " (file-name-nondirectory (directory-file-name (buffer-file-name))) > n
                   "# Author: " user-full-name " <" user-mail-address ">" > n
                   "# Created: " (format-time-string "%Y-%m-%d %H:%M:%S") > n
                   "# Description: " p > n)))

(defun oo-auto-insert-hy-file-header ()
  "Insert hy file header."
  (require 'tempel)
  (tempel-insert '(";; Filename: " (file-name-nondirectory (directory-file-name (buffer-file-name))) > n
                   ";; Author: " user-full-name " <" user-mail-address ">" > n
                   ";; Created: " (format-time-string "%Y-%m-%d %H:%M:%S") > n
                   ";; Description: " p > n)))
;;; provide
(provide 'config-auto-insert)
;;; config-auto-insert.el ends here
