;;; init-after-evil-collection-setup.el --- Configure evil-collection-setup -*- lexical-binding: t; -*-
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
;; Configure evil-collection-setup.
;;
;;; Code:

(autoload 'evil-collection-2048-game-setup "evil-collection" nil nil 'function)
(o-defer-load-after '2048-game #'evil-collection-2048-game-setup)

(autoload 'evil-collection-ag-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'ag #'evil-collection-ag-setup)

(autoload 'evil-collection-alchemist-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'alchemist #'evil-collection-alchemist-setup)

(autoload 'evil-collection-anaconda-mode-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'anaconda-mode #'evil-collection-anaconda-mode-setup)

(autoload 'evil-collection-apropos-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'apropos #'evil-collection-apropos-setup)

(autoload 'evil-collection-arc-mode-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'arc-mode #'evil-collection-arc-mode-setup)

(autoload 'evil-collection-atomic-chrome-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'atomic-chrome #'evil-collection-atomic-chrome-setup)

(autoload 'evil-collection-auto-package-update-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'auto-package-update #'evil-collection-auto-package-update-setup)

(autoload 'evil-collection-beginend-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'beginend #'evil-collection-beginend-setup)

(autoload 'evil-collection-bluetooth-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'bluetooth #'evil-collection-bluetooth-setup)

(autoload 'evil-collection-bm-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'bm #'evil-collection-bm-setup)

(autoload #'evil-collection-bookmark-setup "evil-collection" nil nil 'function)
(add-hook 'bookmark-bmenu-mode-hook #'evil-collection-bookmark-setup)

(autoload 'evil-collection-buff-menu-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'buff-menu #'evil-collection-buff-menu-setup)

(autoload 'evil-collection-bufler-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'bufler #'evil-collection-bufler-setup)

(autoload 'evil-collection-calc-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'calc #'evil-collection-calc-setup)

(autoload 'evil-collection-calendar-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'calendar #'evil-collection-calendar-setup)

(autoload 'evil-collection-cider-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'cider #'evil-collection-cider-setup)

(autoload 'evil-collection-cmake-mode-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'cmake-mode #'evil-collection-cmake-mode-setup)

(autoload 'evil-collection-color-rg-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'color-rg #'evil-collection-color-rg-setup)

(autoload 'evil-collection-comint-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'comint #'evil-collection-comint-setup)

(autoload 'evil-collection-company-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'company #'evil-collection-company-setup)

(autoload 'evil-collection-compile-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'compile #'evil-collection-compile-setup)

(autoload 'evil-collection-consult-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'consult #'evil-collection-consult-setup)

(autoload 'evil-collection-corfu-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'corfu #'evil-collection-corfu-setup)

(autoload 'evil-collection-crdt-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'crdt #'evil-collection-crdt-setup)

(autoload 'evil-collection-csv-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'csv #'evil-collection-csv-setup)

(autoload #'evil-collection-custom-setup "evil-collection" nil nil 'function)
(add-hook 'custom-mode-hook #'evil-collection-custom-setup)

(autoload 'evil-collection-cus-theme-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'cus-theme #'evil-collection-cus-theme-setup)

(autoload #'evil-collection-dashboard-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'dashboard #'evil-collection-dashboard-setup)

(autoload 'evil-collection-daemons-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'daemons #'evil-collection-daemons-setup)

(autoload 'evil-collection-deadgrep-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'deadgrep #'evil-collection-deadgrep-setup)

(autoload 'evil-collection-debbugs-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'debbugs #'evil-collection-debbugs-setup)

(autoload 'evil-collection-debug-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'debug #'evil-collection-debug-setup)

(autoload 'evil-collection-devdocs-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'devdocs #'evil-collection-devdocs-setup)

(autoload 'evil-collection-dictionary-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'dictionary #'evil-collection-dictionary-setup)

(autoload 'evil-collection-diff-hl-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'diff-hl #'evil-collection-diff-hl-setup)

(autoload 'evil-collection-diff-mode-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'diff-mode #'evil-collection-diff-mode-setup)

(autoload 'evil-collection-dired-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'dired #'evil-collection-dired-setup)

(autoload 'evil-collection-dired-sidebar-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'dired-sidebar #'evil-collection-dired-sidebar-setup)

(autoload 'evil-collection-disk-usage-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'disk-usage #'evil-collection-disk-usage-setup)

(autoload 'evil-collection-distel-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'distel #'evil-collection-distel-setup)

(autoload 'evil-collection-doc-view-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'doc-view #'evil-collection-doc-view-setup)

(autoload 'evil-collection-docker-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'docker #'evil-collection-docker-setup)

(autoload 'evil-collection-eat-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'eat #'evil-collection-eat-setup)

(autoload 'evil-collection-ebib-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'ebib #'evil-collection-ebib-setup)

(autoload 'evil-collection-ebuku-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'ebuku #'evil-collection-ebuku-setup)

(autoload 'evil-collection-edbi-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'edbi #'evil-collection-edbi-setup)

(autoload 'evil-collection-edebug-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'edebug #'evil-collection-edebug-setup)

(autoload 'evil-collection-ediff-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'ediff #'evil-collection-ediff-setup)

(autoload 'evil-collection-eglot-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'eglot #'evil-collection-eglot-setup)

(autoload 'evil-collection-elpaca-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'elpaca #'evil-collection-elpaca-setup)

(autoload 'evil-collection-ement-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'ement #'evil-collection-ement-setup)

(autoload 'evil-collection-explain-pause-mode-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'explain-pause-mode #'evil-collection-explain-pause-mode-setup)

(autoload #'evil-collection-eldoc-setup "evil-collection" nil nil 'function)
(add-hook 'emacs-lisp-mode-hook #'evil-collection-eldoc-setup)
(add-hook 'eldoc-mode-hook #'evil-collection-eldoc-setup)

(autoload 'evil-collection-elfeed-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'elfeed #'evil-collection-elfeed-setup)

;; Disable bindings from for elisp mode because they conflict with the bindings
;; I myself am adding.
;; (autoload #'evil-collection-elisp-mode-setup "evil-collection" nil nil 'function)
;; (add-hook 'emacs-lisp-mode-hook #'evil-collection-elisp-mode-setup)

(autoload 'evil-collection-elisp-refs-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'elisp-refs #'evil-collection-elisp-refs-setup)

(autoload 'evil-collection-elisp-slime-nav-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'elisp-slime-nav #'evil-collection-elisp-slime-nav-setup)

(autoload 'evil-collection-embark-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'embark #'evil-collection-embark-setup)

(autoload 'evil-collection-emms-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'emms #'evil-collection-emms-setup)

(autoload 'evil-collection-emoji-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'emoji #'evil-collection-emoji-setup)

(autoload 'evil-collection-epa-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'epa #'evil-collection-epa-setup)

(autoload 'evil-collection-ert-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'ert #'evil-collection-ert-setup)

(autoload 'evil-collection-eshell-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'eshell #'evil-collection-eshell-setup)

(autoload 'evil-collection-eval-sexp-fu-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'eval-sexp-fu #'evil-collection-eval-sexp-fu-setup)

(autoload 'evil-collection-evil-mc-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'evil-mc #'evil-collection-evil-mc-setup)

(autoload 'evil-collection-eww-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'eww #'evil-collection-eww-setup)

(autoload 'evil-collection-fanyi-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'fanyi #'evil-collection-fanyi-setup)

(autoload 'evil-collection-finder-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'finder #'evil-collection-finder-setup)

(autoload 'evil-collection-flycheck-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'flycheck #'evil-collection-flycheck-setup)

(autoload 'evil-collection-flymake-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'flymake #'evil-collection-flymake-setup)

(autoload 'evil-collection-forge-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'forge #'evil-collection-forge-setup)

(autoload 'evil-collection-free-keys-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'free-keys #'evil-collection-free-keys-setup)

(autoload 'evil-collection-geiser-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'geiser #'evil-collection-geiser-setup)

(autoload 'evil-collection-ggtags-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'ggtags #'evil-collection-ggtags-setup)

(autoload 'evil-collection-git-timemachine-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'git-timemachine #'evil-collection-git-timemachine-setup)

(autoload 'evil-collection-gited-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'gited #'evil-collection-gited-setup)

(autoload 'evil-collection-gnus-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'gnus #'evil-collection-gnus-setup)

(autoload 'evil-collection-go-mode-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'go-mode #'evil-collection-go-mode-setup)

(autoload 'evil-collection-grep-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'grep #'evil-collection-grep-setup)

(autoload 'evil-collection-guix-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'guix #'evil-collection-guix-setup)

(autoload 'evil-collection-hackernews-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'hackernews #'evil-collection-hackernews-setup)

(autoload 'evil-collection-helm-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'helm #'evil-collection-helm-setup)

(autoload #'evil-collection-help-setup "evil-collection" nil nil 'function)
(add-hook 'help-mode-hook #'evil-collection-help-setup)

(autoload 'evil-collection-helpful-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'helpful #'evil-collection-helpful-setup)

(autoload 'evil-collection-hg-histedit-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'hg-histedit #'evil-collection-hg-histedit-setup)

(autoload 'evil-collection-hungry-delete-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'hungry-delete #'evil-collection-hungry-delete-setup)

(autoload 'evil-collection-ibuffer-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'ibuffer #'evil-collection-ibuffer-setup)

(autoload #'evil-collection-image-setup "evil-collection" nil nil 'function)
(add-hook 'image-mode-hook #'evil-collection-image-setup)

(autoload 'evil-collection-image-dired-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'image-dired #'evil-collection-image-dired-setup)

(autoload 'evil-collection-image+-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'image+ #'evil-collection-image+-setup)

(autoload 'evil-collection-imenu-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'imenu #'evil-collection-imenu-setup)

(autoload 'evil-collection-imenu-list-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'imenu-list #'evil-collection-imenu-list-setup)

(autoload 'evil-collection-indent-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'indent #'evil-collection-indent-setup)

(autoload 'evil-collection-indium-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'indium #'evil-collection-indium-setup)

(autoload 'evil-collection-info-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'info #'evil-collection-info-setup)

(autoload 'evil-collection-ivy-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'ivy #'evil-collection-ivy-setup)

(autoload 'evil-collection-js2-mode-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'js2-mode #'evil-collection-js2-mode-setup)

(autoload 'evil-collection-leetcode-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'leetcode #'evil-collection-leetcode-setup)

(autoload 'evil-collection-lispy-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'lispy #'evil-collection-lispy-setup)

(autoload 'evil-collection-lms-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'lms #'evil-collection-lms-setup)

(autoload 'evil-collection-log-edit-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'log-edit #'evil-collection-log-edit-setup)

(autoload 'evil-collection-log-view-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'log-view #'evil-collection-log-view-setup)

(autoload 'evil-collection-lsp-ui-imenu-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'lsp-ui-imenu #'evil-collection-lsp-ui-imenu-setup)

(autoload 'evil-collection-lua-mode-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'lua-mode #'evil-collection-lua-mode-setup)

(autoload 'evil-collection-kotlin-mode-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'kotlin-mode #'evil-collection-kotlin-mode-setup)

(autoload 'evil-collection-macrostep-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'macrostep #'evil-collection-macrostep-setup)

(autoload 'evil-collection-man-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'man #'evil-collection-man-setup)

(autoload 'evil-collection-magit-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'magit #'evil-collection-magit-setup)

(autoload 'evil-collection-magit-repos-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'magit-repos #'evil-collection-magit-repos-setup)

(autoload 'evil-collection-magit-section-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'magit-section #'evil-collection-magit-section-setup)

(autoload 'evil-collection-magit-todos-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'magit-todos #'evil-collection-magit-todos-setup)

(autoload 'evil-collection-markdown-mode-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'markdown-mode #'evil-collection-markdown-mode-setup)

(autoload 'evil-collection-monky-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'monky #'evil-collection-monky-setup)

(autoload 'evil-collection-mpc-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'mpc #'evil-collection-mpc-setup)

(autoload 'evil-collection-mpdel-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'mpdel #'evil-collection-mpdel-setup)

(autoload 'evil-collection-mpdired-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'mpdired #'evil-collection-mpdired-setup)

(autoload 'evil-collection-mu4e-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'mu4e #'evil-collection-mu4e-setup)

(autoload 'evil-collection-mu4e-conversation-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'mu4e-conversation #'evil-collection-mu4e-conversation-setup)

(autoload 'evil-collection-neotree-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'neotree #'evil-collection-neotree-setup)

(autoload 'evil-collection-newsticker-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'newsticker #'evil-collection-newsticker-setup)

(autoload 'evil-collection-notmuch-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'notmuch #'evil-collection-notmuch-setup)

(autoload 'evil-collection-nov-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'nov #'evil-collection-nov-setup)

(autoload 'evil-collection-omnisharp-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'omnisharp #'evil-collection-omnisharp-setup)

(autoload 'evil-collection-org-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'org #'evil-collection-org-setup)

(autoload 'evil-collection-org-present-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'org-present #'evil-collection-org-present-setup)

(autoload 'evil-collection-org-roam-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'org-roam #'evil-collection-org-roam-setup)

(autoload 'evil-collection-osx-dictionary-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'osx-dictionary #'evil-collection-osx-dictionary-setup)

(autoload 'evil-collection-outline-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'outline #'evil-collection-outline-setup)

(autoload 'evil-collection-p4-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'p4 #'evil-collection-p4-setup)

(autoload 'evil-collection-package-menu-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'package-menu #'evil-collection-package-menu-setup)

(autoload 'evil-collection-pass-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'pass #'evil-collection-pass-setup)

(autoload 'evil-collection-pdf-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'pdf #'evil-collection-pdf-setup)

(autoload 'evil-collection-popup-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'popup #'evil-collection-popup-setup)

(autoload 'evil-collection-proced-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'proced #'evil-collection-proced-setup)

(autoload 'evil-collection-process-menu-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'process-menu #'evil-collection-process-menu-setup)

(autoload 'evil-collection-prodigy-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'prodigy #'evil-collection-prodigy-setup)

(autoload 'evil-collection-profiler-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'profiler #'evil-collection-profiler-setup)

(autoload 'evil-collection-python-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'python #'evil-collection-python-setup)

(autoload 'evil-collection-quickrun-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'quickrun #'evil-collection-quickrun-setup)

(autoload 'evil-collection-racer-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'racer #'evil-collection-racer-setup)

(autoload 'evil-collection-racket-describe-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'racket-describe #'evil-collection-racket-describe-setup)

(autoload 'evil-collection-realgud-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'realgud #'evil-collection-realgud-setup)

(autoload 'evil-collection-reftex-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'reftex #'evil-collection-reftex-setup)

(autoload #'evil-collection-replace-setup "evil-collection" nil nil 'function)
(add-hook 'occur-mode-hook #'evil-collection-replace-setup)

(autoload 'evil-collection-restclient-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'restclient #'evil-collection-restclient-setup)

(autoload 'evil-collection-rg-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'rg #'evil-collection-rg-setup)

(autoload 'evil-collection-ripgrep-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'ripgrep #'evil-collection-ripgrep-setup)

(autoload 'evil-collection-rjsx-mode-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'rjsx-mode #'evil-collection-rjsx-mode-setup)

(autoload 'evil-collection-robe-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'robe #'evil-collection-robe-setup)

(autoload 'evil-collection-rtags-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'rtags #'evil-collection-rtags-setup)

(autoload 'evil-collection-ruby-mode-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'ruby-mode #'evil-collection-ruby-mode-setup)

(autoload 'evil-collection-scheme-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'scheme #'evil-collection-scheme-setup)

(autoload 'evil-collection-scroll-lock-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'scroll-lock #'evil-collection-scroll-lock-setup)

(autoload 'evil-collection-selectrum-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'selectrum #'evil-collection-selectrum-setup)

(autoload 'evil-collection-sh-script-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'sh-script #'evil-collection-sh-script-setup)

(autoload 'evil-collection-shortdoc-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'shortdoc #'evil-collection-shortdoc-setup)

(autoload #'evil-collection-simple-setup "evil-collection" nil nil 'function)
(add-hook 'special-mode-hook #'evil-collection-simple-setup)

(autoload 'evil-collection-simple-mpc-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'simple-mpc #'evil-collection-simple-mpc-setup)

(autoload 'evil-collection-slime-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'slime #'evil-collection-slime-setup)

(autoload 'evil-collection-sly-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'sly #'evil-collection-sly-setup)

(autoload 'evil-collection-snake-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'snake #'evil-collection-snake-setup)

(autoload 'evil-collection-so-long-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'so-long #'evil-collection-so-long-setup)

(autoload 'evil-collection-speedbar-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'speedbar #'evil-collection-speedbar-setup)

(autoload #'evil-collection-tab-bar-setup "evil-collection" nil nil 'function)
(add-hook 'tab-bar-mode-hook #'evil-collection-tab-bar-setup)

(autoload 'evil-collection-tablist-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'tablist #'evil-collection-tablist-setup)

(autoload #'evil-collection-tabulated-list-setup "evil-collection" nil nil 'function)
(add-hook 'tabulated-list-mode-hook #'evil-collection-tabulated-list-setup)

(autoload 'evil-collection-tar-mode-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'tar-mode #'evil-collection-tar-mode-setup)

(autoload 'evil-collection-telega-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'telega #'evil-collection-telega-setup)

(autoload 'evil-collection-term-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'term #'evil-collection-term-setup)

(autoload 'evil-collection-tetris-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'tetris #'evil-collection-tetris-setup)

(autoload 'evil-collection-thread-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'thread #'evil-collection-thread-setup)

(autoload 'evil-collection-tide-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'tide #'evil-collection-tide-setup)

(autoload 'evil-collection-timer-list-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'timer-list #'evil-collection-timer-list-setup)

(autoload 'evil-collection-transmission-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'transmission #'evil-collection-transmission-setup)

(autoload 'evil-collection-trashed-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'trashed #'evil-collection-trashed-setup)

(autoload 'evil-collection-tuareg-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'tuareg #'evil-collection-tuareg-setup)

(autoload 'evil-collection-typescript-mode-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'typescript-mode #'evil-collection-typescript-mode-setup)

(autoload 'evil-collection-vc-annotate-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'vc-annotate #'evil-collection-vc-annotate-setup)

(autoload 'evil-collection-vc-dir-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'vc-dir #'evil-collection-vc-dir-setup)

(autoload 'evil-collection-vc-git-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'vc-git #'evil-collection-vc-git-setup)

(autoload 'evil-collection-vdiff-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'vdiff #'evil-collection-vdiff-setup)

(autoload 'evil-collection-vertico-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'vertico #'evil-collection-vertico-setup)

(autoload 'evil-collection-view-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'view #'evil-collection-view-setup)

(autoload 'evil-collection-vlf-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'vlf #'evil-collection-vlf-setup)

(autoload 'evil-collection-vterm-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'vterm #'evil-collection-vterm-setup)

(autoload 'evil-collection-vundo-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'vundo #'evil-collection-vundo-setup)

(autoload 'evil-collection-w3m-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'w3m #'evil-collection-w3m-setup)

(autoload 'evil-collection-wdired-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'wdired #'evil-collection-wdired-setup)

(autoload 'evil-collection-wgrep-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'wgrep #'evil-collection-wgrep-setup)

(autoload 'evil-collection-which-key-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'which-key #'evil-collection-which-key-setup)

(autoload 'evil-collection-woman-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'woman #'evil-collection-woman-setup)

(autoload 'evil-collection-xref-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'xref #'evil-collection-xref-setup)

(autoload 'evil-collection-xwidget-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'xwidget #'evil-collection-xwidget-setup)

(autoload 'evil-collection-yaml-mode-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'yaml-mode #'evil-collection-yaml-mode-setup)

(autoload 'evil-collection-youtube-dl-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'youtube-dl #'evil-collection-youtube-dl-setup)

(autoload 'evil-collection-zmusic-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'zmusic #'evil-collection-zmusic-setup)

(autoload 'evil-collection-ztree-setup "evil-collection" nil nil 'function)
(o-defer-load-after 'ztree #'evil-collection-ztree-setup)
;;; provide
(provide 'init-after-evil-collection-setup)
;;; init-after-evil-collection-setup.el ends here
