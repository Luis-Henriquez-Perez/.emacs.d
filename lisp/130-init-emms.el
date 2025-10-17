;;; 130-init-emms.el --- initialize emms -*- lexical-binding: t; -*-
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
;; Initialize emms.
;;
;;; Code:
(require! "^0[01]")

(opt! emms-source-file-default-directory (expand-file-name "~/Audio/Music"))
(opt! emms-directory (expand-file-name "emms/" oo-cache-dir))

;; As of right now using VLC or MPV will have the effect of repeating the current track
;; in the playlist indefinitely.  These parameters at least prevent this form
;; happening with MPV.  Actually, I think the only pertinent one for this is
;; "--no-config".
(opt! emms-player-mpv-parameters (list "--quiet"
                                       "--really-quiet"
                                       "--no-config"
                                       "--no-audio-display"
                                       "--force-window=no"
                                       "--vo=null"))

(opt! emms-player-list '(emms-player-mpv emms-player-vlc))
(autoload 'emms-player-mpv "emms-player-mpv" nil nil 'function)

(opt! emms-info-functions '(emms-info-native))
;; Do not make this an invisible buffer.  I want to be able to switch to it normally.
(opt! emms-playlist-buffer "*EMMS Playlist*")

(declare-function emms-add-directory "emms")

(defun oo-emms-playlist-mode-go ()
  (interactive)
  (require 'emms)
  ;; Ah I need to figure out a better way to do this.
  ;; (require 'emms-player-mpv)
  (emms-add-directory emms-source-file-default-directory)
  (call-interactively #'emms-playlist-mode-go))

;; Without this I get an error that `emms-player-mpv' is not loaded.
(defafter! oo-ensure-proper-emms-player-is-loaded (emms)
  (or (and (executable-find "mpv") (require 'emms-player-mpv))
      (and (executable-find "vlc") (require 'emms-player-vlc))))
;;; provide
(provide '130-init-emms)
;;; 130-init-emms.el ends here
