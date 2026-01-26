;;; init-pkg-eshell.el --- initialize eshell -*- lexical-binding: t; -*-
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
;; Initialize eshell.
;;
;;; Code:
(require 'init-core)

;; Do not save aliases file.  Instead I load the aliases as elisp.
(o-opt eshell-aliases-file nil)

(o-opt eshell-banner-message "")
(o-opt eshell-highlight-prompt nil)
;; For now outsource to epe, but later I will make my own.  Also epe uses static
;; faces by which I mean constant faces, not existing ones that change with
;; themes.  So the prompt is difficult to read with certain themes, particularly
;; light themes.
(o-opt eshell-prompt-function 'o-eshell-prompt)
;; This is obsolete as of Emacs 30.1.
(o-opt eshell-prompt-regexp "^[^λ]+λ ")
(o-opt eshell-hist-ignoredups t)
(o-opt eshell-prefer-lisp-functions nil)
;; Represent buffers as #<buffer-name>
(o-opt eshell-buffer-shorthand t)
;; boost eshell history-size
;; Increase the history size from 128 to 1000.
(o-opt eshell-history-size 1000)
;; By "highlight" eshell does not just mean coloring the font with the
;; `eshell-prompt' face.  It also makes the prompt read-only.  Strangely, the
;; prompt is not read-only by default.  Furthermore, there is no way to override
;; the text properties `eshell-emit-prompt' adds to the prompt without advising
;; the it.
(o-opt eshell-highlight-prompt t)
(o-opt eshell-hist-ignoredups t)

(o-setq-mode-local eshell-mode completion-at-point-functions '(pcomplete-completions-at-point t))

(o-each '(esh-arg esh-util esh-proc esh-io esh-cmd em-dirs em-hist em-prompt em-term em-ls em-glob em-basic em-script em-cmpl em-smart)
  (push it o-idle-features))

(add-hook 'eshell-mode-hook #'abbrev-mode)
(add-hook 'eshell-mode-hook #'emacs-lock-mode)

(o-popup-at-bottom "\\*eshell")

(declare-function eshell-unload-all-modules "eshell")
(advice-add #'eshell-unload-all-modules :around #'o-advice--silence-output)

(advice-add #'eshell-mode :around #'o-advice--silence-output)

(o-require-after-load 'eshell 'init-after-eshell)
;;; provide
(provide 'init-pkg-eshell)
;;; init-pkg-eshell.el ends here
