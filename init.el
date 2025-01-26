;;; init.el --- My emacs configuration -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2024 Free Software Foundation, Inc.
;;
;; Author: Luis Henriquez-Perez <luis@luishp.xyz>
;; Maintainer: Luis Henriquez-Perez <luis@luishp.xyz>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
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
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This is my personal emacs configuration.  Please refer to the
;; README for information on how to run and modify them.
;;
;;; Code:
;;;; disable garbage collection until after startup
;; https://medium.com/@danielorihuelarodriguez/optimize-emacs-start-up-time-ae314201e04f
;; https://news.ycombinator.com/item?id=39127859
;; https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.8)
;;;; don't search for whenever a package is loaded
;; Credits to irreal for sharing that keywords can be used as registers in his
;; blog post (https://irreal.org/blog/?p=12386).
(set-register :file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
;;;; prevent flashing of unstyled modeline
;; Don't render the modeline on startup.  For one thing, the startup looks
;; better without flashing stuff on the screen.  Additionally, the more that's
;; saved on rendering, the faster the startup.
(set-register :mode-line-format mode-line-format)
(setq-default mode-line-format nil)
;;;; loader
(defmacro require! (feature &optional path)
  "Catch any errors, record and log the time taken to require FEATURE."
  `(let ((start (current-time)))
     (condition-case err
         (progn
           (require ',feature ,path)
           (message "Required '%s in %.3f seconds"
                    ',feature
                    (float-time (time-subtract (current-time) start))))
       (error
        (message "Error requiring '%s: %s" ',feature err)))))

(defmacro init! (dir)
  (let (forms feature)
    (dolist (path (directory-files dir t (rx bol (1+ nonl) ".el" eol)))
      (setq feature (intern (file-name-sans-extension (file-name-nondirectory (directory-file-name path)))))
      (push `(require! ,feature ,path) forms))
    `(progn ,@(nreverse forms))))
;;;; set load-path
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/base/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/init/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/config/" user-emacs-directory))
;; (require 'init-loader)
(init! "lisp/base")
(progn
  (let
      ((start
        (current-time)))
    (condition-case err
        (progn
          (require '00-init-no-littering "/home/luis/.config/emacs/lisp/init/00-init-no-littering.el")
          (message "Required '%s in %.3f seconds" '00-init-no-littering
                   (float-time
                    (time-subtract
                     (current-time)
                     start))))
      (error
       (message "Error requiring '%s: %s" '00-init-no-littering err))))
  (require! init-abbrev "/home/luis/.config/emacs/lisp/init/init-abbrev.el")
  (require! init-ace-helm-jump-line "/home/luis/.config/emacs/lisp/init/init-ace-helm-jump-line.el")
  (require! init-ace-window "/home/luis/.config/emacs/lisp/init/init-ace-window.el")
  (require! init-aggressive-indent "/home/luis/.config/emacs/lisp/init/init-aggressive-indent.el")
  (require! init-all-the-icons "/home/luis/.config/emacs/lisp/init/init-all-the-icons.el")
  (require! init-auto-insert "/home/luis/.config/emacs/lisp/init/init-auto-insert.el")
  (require! init-avy "/home/luis/.config/emacs/lisp/init/init-avy.el")
  (require! init-beacon "/home/luis/.config/emacs/lisp/init/init-beacon.el")
  (require! init-browse-url "/home/luis/.config/emacs/lisp/init/init-browse-url.el")
  (require! init-buffer-terminator "/home/luis/.config/emacs/lisp/init/init-buffer-terminator.el")
  (require! init-burly "/home/luis/.config/emacs/lisp/init/init-burly.el")
  (require! init-captain "/home/luis/.config/emacs/lisp/init/init-captain.el")
  (require! init-chess "/home/luis/.config/emacs/lisp/init/init-chess.el")
  (require! init-chezmoi "/home/luis/.config/emacs/lisp/init/init-chezmoi.el")
  (require! init-consult "/home/luis/.config/emacs/lisp/init/init-consult.el")
  (require! init-corfu-history "/home/luis/.config/emacs/lisp/init/init-corfu-history.el")
  (require! init-corfu-quick "/home/luis/.config/emacs/lisp/init/init-corfu-quick.el")
  (require! init-corfu "/home/luis/.config/emacs/lisp/init/init-corfu.el")
  (require! init-dashboard "/home/luis/.config/emacs/lisp/init/init-dashboard.el")
  (require! init-denote "/home/luis/.config/emacs/lisp/init/init-denote.el")
  (require! init-dimmer "/home/luis/.config/emacs/lisp/init/init-dimmer.el")
  (require! init-dired "/home/luis/.config/emacs/lisp/init/init-dired.el")
  (require! init-dirvish "/home/luis/.config/emacs/lisp/init/init-dirvish.el")
  (require! init-doom-modeline "/home/luis/.config/emacs/lisp/init/init-doom-modeline.el")
  (require! init-easy-escape "/home/luis/.config/emacs/lisp/init/init-easy-escape.el")
  (require! init-ef-themes "/home/luis/.config/emacs/lisp/init/init-ef-themes.el")
  (require! init-elfeed "/home/luis/.config/emacs/lisp/init/init-elfeed.el")
  (require! init-emmet "/home/luis/.config/emacs/lisp/init/init-emmet.el")
  (require! init-emms "/home/luis/.config/emacs/lisp/init/init-emms.el")
  (require! init-enlight "/home/luis/.config/emacs/lisp/init/init-enlight.el")
  (require! init-escr "/home/luis/.config/emacs/lisp/init/init-escr.el")
  (require! init-eshell "/home/luis/.config/emacs/lisp/init/init-eshell.el")
  (require! init-evil-cleverparens "/home/luis/.config/emacs/lisp/init/init-evil-cleverparens.el")
  (require! init-evil-collection "/home/luis/.config/emacs/lisp/init/init-evil-collection.el")
  (require! init-evil-easymotion "/home/luis/.config/emacs/lisp/init/init-evil-easymotion.el")
  (require! init-evil-exchange "/home/luis/.config/emacs/lisp/init/init-evil-exchange.el")
  (require! init-evil-fringe-mark "/home/luis/.config/emacs/lisp/init/init-evil-fringe-mark.el")
  (require! init-evil-goggles "/home/luis/.config/emacs/lisp/init/init-evil-goggles.el")
  (require! init-evil-lispy "/home/luis/.config/emacs/lisp/init/init-evil-lispy.el")
  (require! init-evil-surround "/home/luis/.config/emacs/lisp/init/init-evil-surround.el")
  (require! init-evil-textobj-anyblock "/home/luis/.config/emacs/lisp/init/init-evil-textobj-anyblock.el")
  (require! init-evil-textobj-line "/home/luis/.config/emacs/lisp/init/init-evil-textobj-line.el")
  (require! init-evil-textobj-syntax "/home/luis/.config/emacs/lisp/init/init-evil-textobj-syntax.el")
  (require! init-evil "/home/luis/.config/emacs/lisp/init/init-evil.el")
  (require! init-eww "/home/luis/.config/emacs/lisp/init/init-eww.el")
  (require! init-expreg "/home/luis/.config/emacs/lisp/init/init-expreg.el")
  (require! init-fancy-narrow "/home/luis/.config/emacs/lisp/init/init-fancy-narrow.el")
  (require! init-fill-adapt "/home/luis/.config/emacs/lisp/init/init-fill-adapt.el")
  (require! init-gcmh "/home/luis/.config/emacs/lisp/init/init-gcmh.el")
  (require! init-gnus "/home/luis/.config/emacs/lisp/init/init-gnus.el")
  (require! init-grugru "/home/luis/.config/emacs/lisp/init/init-grugru.el")
  (require! init-gumshoe "/home/luis/.config/emacs/lisp/init/init-gumshoe.el")
  (require! init-helm "/home/luis/.config/emacs/lisp/init/init-helm.el")
  (require! init-helpful "/home/luis/.config/emacs/lisp/init/init-helpful.el")
  (require! init-highlight-quoted "/home/luis/.config/emacs/lisp/init/init-highlight-quoted.el")
  (require! init-htmlize "/home/luis/.config/emacs/lisp/init/init-htmlize.el")
  (require! init-hungry-delete "/home/luis/.config/emacs/lisp/init/init-hungry-delete.el")
  (require! init-hy-mode "/home/luis/.config/emacs/lisp/init/init-hy-mode.el")
  (require! init-keyfreq "/home/luis/.config/emacs/lisp/init/init-keyfreq.el")
  (require! init-lispyville "/home/luis/.config/emacs/lisp/init/init-lispyville.el")
  (require! init-logger "/home/luis/.config/emacs/lisp/init/init-logger.el")
  (require! init-lorem-ipsum "/home/luis/.config/emacs/lisp/init/init-lorem-ipsum.el")
  (require! init-macroexpand "/home/luis/.config/emacs/lisp/init/init-macroexpand.el")
  (require! init-magit "/home/luis/.config/emacs/lisp/init/init-magit.el")
  (require! init-marginalia "/home/luis/.config/emacs/lisp/init/init-marginalia.el")
  (require! init-modaled "/home/luis/.config/emacs/lisp/init/init-modaled.el")
  (require! init-modus-themes "/home/luis/.config/emacs/lisp/init/init-modus-themes.el")
  (require! init-mu4e "/home/luis/.config/emacs/lisp/init/init-mu4e.el")
  (require! init-notmuch "/home/luis/.config/emacs/lisp/init/init-notmuch.el")
  (require! init-orderless "/home/luis/.config/emacs/lisp/init/init-orderless.el")
  (require! init-org-appear "/home/luis/.config/emacs/lisp/init/init-org-appear.el")
  (require! init-org-fancy-priorities "/home/luis/.config/emacs/lisp/init/init-org-fancy-priorities.el")
  (require! init-org-pretty-tags "/home/luis/.config/emacs/lisp/init/init-org-pretty-tags.el")
  (require! init-org-superstar "/home/luis/.config/emacs/lisp/init/init-org-superstar.el")
  (require! init-org "/home/luis/.config/emacs/lisp/init/init-org.el")
  (require! init-outli "/home/luis/.config/emacs/lisp/init/init-outli.el")
  (require! init-outline "/home/luis/.config/emacs/lisp/init/init-outline.el")
  (require! init-package "/home/luis/.config/emacs/lisp/init/init-package.el")
  (require! init-pomodoro "/home/luis/.config/emacs/lisp/init/init-pomodoro.el")
  (require! init-project "/home/luis/.config/emacs/lisp/init/init-project.el")
  (require! init-rainbow-delimiters "/home/luis/.config/emacs/lisp/init/init-rainbow-delimiters.el")
  (require! init-re-builder "/home/luis/.config/emacs/lisp/init/init-re-builder.el")
  (require! init-recentf "/home/luis/.config/emacs/lisp/init/init-recentf.el")
  (require! init-restart-emacs "/home/luis/.config/emacs/lisp/init/init-restart-emacs.el")
  (require! init-savehist "/home/luis/.config/emacs/lisp/init/init-savehist.el")
  (require! init-saveplace "/home/luis/.config/emacs/lisp/init/init-saveplace.el")
  (require! init-smartparens "/home/luis/.config/emacs/lisp/init/init-smartparens.el")
  (require! init-stripes-buffer "/home/luis/.config/emacs/lisp/init/init-stripes-buffer.el")
  (require! init-super-save "/home/luis/.config/emacs/lisp/init/init-super-save.el")
  (require! init-tab-bar "/home/luis/.config/emacs/lisp/init/init-tab-bar.el")
  (require! init-tempel "/home/luis/.config/emacs/lisp/init/init-tempel.el")
  (require! init-vertico-buffer "/home/luis/.config/emacs/lisp/init/init-vertico-buffer.el")
  (require! init-vertico-multiform "/home/luis/.config/emacs/lisp/init/init-vertico-multiform.el")
  (require! init-vertico-quick "/home/luis/.config/emacs/lisp/init/init-vertico-quick.el")
  (require! init-vertico "/home/luis/.config/emacs/lisp/init/init-vertico.el")
  (require! init-w3m "/home/luis/.config/emacs/lisp/init/init-w3m.el")
  (require! init-wdired "/home/luis/.config/emacs/lisp/init/init-wdired.el")
  (require! init-which-key "/home/luis/.config/emacs/lisp/init/init-which-key.el")
  (require! init-yasnippet "/home/luis/.config/emacs/lisp/init/init-yasnippet.el")
  (require! init-zone "/home/luis/.config/emacs/lisp/init/init-zone.el"))
;;;; load requirements
(require 'oo-keybindings)
(require 'oo-autoloads)
(require 'oo-init)
;;; provide init
(provide 'init)
;;; init.el ends here
