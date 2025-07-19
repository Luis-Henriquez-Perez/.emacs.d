;;; 990-config-yeetube.el --- TODO: add commentary -*- lexical-binding: t; -*-
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
(require '050-base)
(require 'yeetube)

(nmap yeetube-mode-map "a" oo-yeetube-download-audio)
(nmap yeetube-mode-map "d" yeetube-download-video)

(hook! yeetube-mode-hook hl-line-mode)

;; Make simple commands to download audio the way I want it.

(defun oo-yeetube-download-audio (&optional url)
  "Download entry at point in *yeetube* buffer with yt-dlp.

Content will be downloaded at `yeetube-download-directory'.
Optionally, provide custom own URL."
  (interactive)
  (let* ((id (tabulated-list-get-id))
	     (entry-content (cadr (assoc id yeetube-content)))
	     (type (aref entry-content (- (length entry-content) 1)))
	     (url (or (yeetube-get-url id type) url))
	     (title (or (aref entry-content 0) "Unknown"))
         (music-dir (expand-file-name "~/Audio/Music")))
    (when (string-prefix-p "http" url)
      (let ((default-directory music-dir))
        (oo-yeetube-download-audio--ytdlp url)
        (message "Downloading audio at '%s'" music-dir)))))

(defun! oo-yeetube-download-audio--ytdlp (url)
  "Download URL using yt-dlp."
  (unless (executable-find "yt-dlp")
    (error "Executable for yt-dlp not found.  Please install yt-dlp"))
  (set! ytdlp (executable-find "yt-dlp"))
  (set! command (format "%s %s --write-thumbnail --extract-audio --no-keep-video" ytdlp url))
  (call-process-shell-command command nil 0))
;;; provide
(provide '990-config-yeetube)
;;; 990-config-yeetube.el ends here
