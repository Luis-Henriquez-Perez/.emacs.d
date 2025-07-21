;;; 990-config-auto-insert.el --- Configure auto-insert -*- lexical-binding: t; -*-
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
(require '050-base)
(require 'f)
(require 'tempel)

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

(defun oo--ensure-file-header (&optional comment1 comment2)
  "Add an emacs-lisp copyright header to current buffer.
COMMENT1 is the description of the file at the top.  COMMENT2 is the description
in the commentary part."
  (let* ((file (buffer-file-name))
         (filename (file-name-sans-extension (file-name-nondirectory file)))
         (header-rx (oo-header-regexp))
         (lisence-rx (rx-to-string (oo-copyright-license))))
    (setq comment1 (or comment1 "TODO: add commentary"))
    (setq comment2 (or comment2 "TODO: add commentary"))
    (save-excursion
      (goto-char (point-min))
      (if (looking-at header-rx)
          (progn (replace-match filename nil 'literal nil 1)
                 (goto-char (match-end 0)))
        (insert (format ";;; %s.el --- %s -*- lexical-binding: t; -*-\n" filename comment1)))
      ;; Ensure license.
      (unless (looking-at lisence-rx)
        (insert (oo-copyright-license)))
      ;; Ensure commentary.
      (if (looking-at ";;; Commentary:\n\\(?:\\(?:^;;$\\)\n\\|\\(?:^;;[^;].*$\\)\n\\)*")
          (goto-char (match-end 0))
        (insert (format ";;; Commentary:\n;;\n;; %s\n;;\n" comment2)))
      (if (looking-at "\\`;;;[[:blank:]]Code:\n")
          (goto-char (match-end 0))
        (insert ";;; Code:\n")))))

(defun! oo-auto-insert-elisp-template ()
  "Insert emacs-lisp template in file."
  (set! path (buffer-file-name))
  (set! base (f-base path))
  (when (f-descendant-of-p path user-emacs-directory)
    (pcase path
      ((rx "test.el" eos)
       (alet! (format "Test `%s'." base)
         (oo--ensure-file-header (substring it 0 -1) it)))
      ((rx (= 3 digit) "init-" (1+ nonl) ".el" eos)
       (alet! (format "Initialize `%s'." base)
         (oo--ensure-file-header (substring it 0 -1) it)))
      ((rx (= 3 digit) "config-" (1+ nonl) ".el" eos)
       (alet! (format "Configure `%s'." base)
         (oo--ensure-file-header (substring it 0 -1) it)))
      (_
       (oo--ensure-file-header)))
    (goto-char (point-min))
    (save-excursion (oo--ensure-provide path))))

(defun! oo-auto-insert-html-template ()
  "Insert html template in file."
  (interactive)
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

(defun oo-auto-insert-org-file-header ()
  "Insert org file header."
  (interactive)
  (tempel-insert '("#+title:" (string-replace "_" "\s" (f-base (buffer-file-name))) > n
                   "#+author:" user-full-name > n)))


(defun! oo-auto-insert-script-file-header ()
  "Insert script file header."
  (set! file-dir (file-truename (file-name-directory buffer-file-name)))
  (set! script-dir (file-truename (expand-file-name "~/.local/bin/")))
  (when (and buffer-file-name (equal file-dir script-dir))
    ;; Make the file executable.
    (set-file-modes buffer-file-name (logior (file-modes buffer-file-name) #o111))
    (tempel-insert '("#!/bin/bash" > n
                     "# Filename: " (file-name-nondirectory (directory-file-name (buffer-file-name))) > n
                     "# Author: " user-full-name " <" user-mail-address ">" > n
                     "# Created: " (format-time-string "%Y-%m-%d %H:%M:%S") > n
                     "# Description: " p > n))))
;;; provide
(provide '990-config-auto-insert)
;;; 990-config-auto-insert.el ends here
