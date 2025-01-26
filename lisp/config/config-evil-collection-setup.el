;;; config-evil-collection-setup.el --- Configure evil-collection-setup -*- lexical-binding: t; -*-
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
(defmacro after! (expr fn &optional feature)
  "Call function after EXPR is met."
  `(progn (declare-function ,fn ,(if feature (symbol-name feature) nil))
          ,@(when feature `((autoload #',fn ,(symbol-name feature) nil nil 'function)))
          (oo-call-after-load ',expr #',fn)))

(after! 2048-game evil-collection-2048-game-setup evil-collection)

(after! ag evil-collection-ag-setup evil-collection)

(after! alchemist evil-collection-alchemist-setup evil-collection)

(after! anaconda-mode evil-collection-anaconda-mode-setup evil-collection)

(after! apropos evil-collection-apropos-setup evil-collection)

(after! arc-mode evil-collection-arc-mode-setup evil-collection)

(after! atomic-chrome evil-collection-atomic-chrome-setup evil-collection)

(after! auto-package-update evil-collection-auto-package-update-setup evil-collection)

(after! beginend evil-collection-beginend-setup evil-collection)

(after! bluetooth evil-collection-bluetooth-setup evil-collection)

(after! bm evil-collection-bm-setup evil-collection)

(autoload #'evil-collection-bookmark-setup "evil-collection" nil nil 'function)
(hook! bookmark-bmenu-mode-hook evil-collection-bookmark-setup)

(after! buff-menu evil-collection-buff-menu-setup evil-collection)

(after! bufler evil-collection-bufler-setup evil-collection)

(after! calc evil-collection-calc-setup evil-collection)

(after! calendar evil-collection-calendar-setup evil-collection)

(after! cider evil-collection-cider-setup evil-collection)

(after! cmake-mode evil-collection-cmake-mode-setup evil-collection)

(after! color-rg evil-collection-color-rg-setup evil-collection)

(after! comint evil-collection-comint-setup evil-collection)

(after! company evil-collection-company-setup evil-collection)

(after! compile evil-collection-compile-setup evil-collection)

(after! consult evil-collection-consult-setup evil-collection)

(after! corfu evil-collection-corfu-setup evil-collection)

(after! crdt evil-collection-crdt-setup evil-collection)

(after! csv evil-collection-csv-setup evil-collection)

(autoload #'evil-collection-custom-setup "evil-collection" nil nil 'function)
(hook! custom-mode-hook evil-collection-custom-setup)

(after! cus-theme evil-collection-cus-theme-setup evil-collection)

(autoload #'evil-collection-dashboard-setup "evil-collection" nil nil 'function)
(defhook! oo-setup-evil-collection-h (dashboard-mode-hook)
  (if after-init-time
      (evil-collection-dashboard-setup)
    (hook! emacs-startup-hook evil-collection-dashboard-setup)))

(after! daemons evil-collection-daemons-setup evil-collection)

(after! deadgrep evil-collection-deadgrep-setup evil-collection)

(after! debbugs evil-collection-debbugs-setup evil-collection)

(after! debug evil-collection-debug-setup evil-collection)

(after! devdocs evil-collection-devdocs-setup evil-collection)

(after! dictionary evil-collection-dictionary-setup evil-collection)

(after! diff-hl evil-collection-diff-hl-setup evil-collection)

(after! diff-mode evil-collection-diff-mode-setup evil-collection)

(after! dired evil-collection-dired-setup evil-collection)

(after! dired-sidebar evil-collection-dired-sidebar-setup evil-collection)

(after! disk-usage evil-collection-disk-usage-setup evil-collection)

(after! distel evil-collection-distel-setup evil-collection)

(after! doc-view evil-collection-doc-view-setup evil-collection)

(after! docker evil-collection-docker-setup evil-collection)

(after! eat evil-collection-eat-setup evil-collection)

(after! ebib evil-collection-ebib-setup evil-collection)

(after! ebuku evil-collection-ebuku-setup evil-collection)

(after! edbi evil-collection-edbi-setup evil-collection)

(after! edebug evil-collection-edebug-setup evil-collection)

(after! ediff evil-collection-ediff-setup evil-collection)

(after! eglot evil-collection-eglot-setup evil-collection)

(after! elpaca evil-collection-elpaca-setup evil-collection)

(after! ement evil-collection-ement-setup evil-collection)

(after! explain-pause-mode evil-collection-explain-pause-mode-setup evil-collection)

(autoload #'evil-collection-eldoc-setup "evil-collection" nil nil 'function)
(hook! emacs-lisp-mode-hook evil-collection-eldoc-setup)
(hook! eldoc-mode-hook evil-collection-eldoc-setup)

(after! elfeed evil-collection-elfeed-setup evil-collection)

(autoload #'evil-collection-elisp-mode-setup "evil-collection" nil nil 'function)
(hook! emacs-lisp-mode-hook evil-collection-elisp-mode-setup)

(after! elisp-refs evil-collection-elisp-refs-setup evil-collection)

(after! elisp-slime-nav evil-collection-elisp-slime-nav-setup evil-collection)

(after! embark evil-collection-embark-setup evil-collection)

(after! emms evil-collection-emms-setup evil-collection)

(after! emoji evil-collection-emoji-setup evil-collection)

(after! epa evil-collection-epa-setup evil-collection)

(after! ert evil-collection-ert-setup evil-collection)

(after! eshell evil-collection-eshell-setup evil-collection)

(after! eval-sexp-fu evil-collection-eval-sexp-fu-setup evil-collection)

(after! evil-mc evil-collection-evil-mc-setup evil-collection)

(after! eww evil-collection-eww-setup evil-collection)

(after! fanyi evil-collection-fanyi-setup evil-collection)

(after! finder evil-collection-finder-setup evil-collection)

(after! flycheck evil-collection-flycheck-setup evil-collection)

(after! flymake evil-collection-flymake-setup evil-collection)

(after! forge evil-collection-forge-setup evil-collection)

(after! free-keys evil-collection-free-keys-setup evil-collection)

(after! geiser evil-collection-geiser-setup evil-collection)

(after! ggtags evil-collection-ggtags-setup evil-collection)

(after! git-timemachine evil-collection-git-timemachine-setup evil-collection)

(after! gited evil-collection-gited-setup evil-collection)

(after! gnus evil-collection-gnus-setup evil-collection)

(after! go-mode evil-collection-go-mode-setup evil-collection)

(after! grep evil-collection-grep-setup evil-collection)

(after! guix evil-collection-guix-setup evil-collection)

(after! hackernews evil-collection-hackernews-setup evil-collection)

(after! helm evil-collection-helm-setup evil-collection)

(autoload #'evil-collection-help-setup "evil-collection" nil nil 'function)
(hook! help-mode-hook evil-collection-help-setup)

(after! helpful evil-collection-helpful-setup evil-collection)

(after! hg-histedit evil-collection-hg-histedit-setup evil-collection)

(after! hungry-delete evil-collection-hungry-delete-setup evil-collection)

(after! ibuffer evil-collection-ibuffer-setup evil-collection)

(autoload #'evil-collection-image-setup "evil-collection" nil nil 'function)
(hook! image-mode-hook evil-collection-image-setup)

(after! image-dired evil-collection-image-dired-setup evil-collection)

(after! image+ evil-collection-image+-setup evil-collection)

(after! imenu evil-collection-imenu-setup evil-collection)

(after! imenu-list evil-collection-imenu-list-setup evil-collection)

(after! indent evil-collection-indent-setup evil-collection)

(after! indium evil-collection-indium-setup evil-collection)

(after! info evil-collection-info-setup evil-collection)

(after! ivy evil-collection-ivy-setup evil-collection)

(after! js2-mode evil-collection-js2-mode-setup evil-collection)

(after! leetcode evil-collection-leetcode-setup evil-collection)

(after! lispy evil-collection-lispy-setup evil-collection)

(after! lms evil-collection-lms-setup evil-collection)

(after! log-edit evil-collection-log-edit-setup evil-collection)

(after! log-view evil-collection-log-view-setup evil-collection)

(after! lsp-ui-imenu evil-collection-lsp-ui-imenu-setup evil-collection)

(after! lua-mode evil-collection-lua-mode-setup evil-collection)

(after! kotlin-mode evil-collection-kotlin-mode-setup evil-collection)

(after! macrostep evil-collection-macrostep-setup evil-collection)

(after! man evil-collection-man-setup evil-collection)

(after! magit evil-collection-magit-setup evil-collection)

(after! magit-repos evil-collection-magit-repos-setup evil-collection)

(after! magit-section evil-collection-magit-section-setup evil-collection)

(after! magit-todos evil-collection-magit-todos-setup evil-collection)

(after! markdown-mode evil-collection-markdown-mode-setup evil-collection)

(after! monky evil-collection-monky-setup evil-collection)

(after! mpc evil-collection-mpc-setup evil-collection)

(after! mpdel evil-collection-mpdel-setup evil-collection)

(after! mpdired evil-collection-mpdired-setup evil-collection)

(after! mu4e evil-collection-mu4e-setup evil-collection)

(after! mu4e-conversation evil-collection-mu4e-conversation-setup evil-collection)

(after! neotree evil-collection-neotree-setup evil-collection)

(after! newsticker evil-collection-newsticker-setup evil-collection)

(after! notmuch evil-collection-notmuch-setup evil-collection)

(after! nov evil-collection-nov-setup evil-collection)

(after! omnisharp evil-collection-omnisharp-setup evil-collection)

(after! org evil-collection-org-setup evil-collection)

(after! org-present evil-collection-org-present-setup evil-collection)

(after! org-roam evil-collection-org-roam-setup evil-collection)

(after! osx-dictionary evil-collection-osx-dictionary-setup evil-collection)

(after! outline evil-collection-outline-setup evil-collection)

(after! p4 evil-collection-p4-setup evil-collection)

(after! package-menu evil-collection-package-menu-setup evil-collection)

(after! pass evil-collection-pass-setup evil-collection)

(after! pdf evil-collection-pdf-setup evil-collection)

(after! popup evil-collection-popup-setup evil-collection)

(after! proced evil-collection-proced-setup evil-collection)

(after! process-menu evil-collection-process-menu-setup evil-collection)

(after! prodigy evil-collection-prodigy-setup evil-collection)

(after! profiler evil-collection-profiler-setup evil-collection)

(after! python evil-collection-python-setup evil-collection)

(after! quickrun evil-collection-quickrun-setup evil-collection)

(after! racer evil-collection-racer-setup evil-collection)

(after! racket-describe evil-collection-racket-describe-setup evil-collection)

(after! realgud evil-collection-realgud-setup evil-collection)

(after! reftex evil-collection-reftex-setup evil-collection)

(autoload #'evil-collection-replace-setup "evil-collection" nil nil 'function)
(hook! occur-mode-hook evil-collection-replace-setup)

(after! restclient evil-collection-restclient-setup evil-collection)

(after! rg evil-collection-rg-setup evil-collection)

(after! ripgrep evil-collection-ripgrep-setup evil-collection)

(after! rjsx-mode evil-collection-rjsx-mode-setup evil-collection)

(after! robe evil-collection-robe-setup evil-collection)

(after! rtags evil-collection-rtags-setup evil-collection)

(after! ruby-mode evil-collection-ruby-mode-setup evil-collection)

(after! scheme evil-collection-scheme-setup evil-collection)

(after! scroll-lock evil-collection-scroll-lock-setup evil-collection)

(after! selectrum evil-collection-selectrum-setup evil-collection)

(after! sh-script evil-collection-sh-script-setup evil-collection)

(after! shortdoc evil-collection-shortdoc-setup evil-collection)

(autoload #'evil-collection-simple-setup "evil-collection" nil nil 'function)
(hook! special-mode-hook evil-collection-simple-setup)

(after! simple-mpc evil-collection-simple-mpc-setup evil-collection)

(after! slime evil-collection-slime-setup evil-collection)

(after! sly evil-collection-sly-setup evil-collection)

(after! snake evil-collection-snake-setup evil-collection)

(after! so-long evil-collection-so-long-setup evil-collection)

(after! speedbar evil-collection-speedbar-setup evil-collection)

(autoload #'evil-collection-tab-bar-setup "evil-collection" nil nil 'function)
(hook! tab-bar-mode-hook evil-collection-tab-bar-setup)

(after! tablist evil-collection-tablist-setup evil-collection)

(autoload #'evil-collection-tabulated-list-setup "evil-collection" nil nil 'function)
(hook! tabulated-list-mode-hook evil-collection-tabulated-list-setup)

(after! tar-mode evil-collection-tar-mode-setup evil-collection)

(after! telega evil-collection-telega-setup evil-collection)

(after! term evil-collection-term-setup evil-collection)

(after! tetris evil-collection-tetris-setup evil-collection)

(after! thread evil-collection-thread-setup evil-collection)

(after! tide evil-collection-tide-setup evil-collection)

(after! timer-list evil-collection-timer-list-setup evil-collection)

(after! transmission evil-collection-transmission-setup evil-collection)

(after! trashed evil-collection-trashed-setup evil-collection)

(after! tuareg evil-collection-tuareg-setup evil-collection)

(after! typescript-mode evil-collection-typescript-mode-setup evil-collection)

(after! vc-annotate evil-collection-vc-annotate-setup evil-collection)

(after! vc-dir evil-collection-vc-dir-setup evil-collection)

(after! vc-git evil-collection-vc-git-setup evil-collection)

(after! vdiff evil-collection-vdiff-setup evil-collection)

(after! vertico evil-collection-vertico-setup evil-collection)

(after! view evil-collection-view-setup evil-collection)

(after! vlf evil-collection-vlf-setup evil-collection)

(after! vterm evil-collection-vterm-setup evil-collection)

(after! vundo evil-collection-vundo-setup evil-collection)

(after! w3m evil-collection-w3m-setup evil-collection)

(after! wdired evil-collection-wdired-setup evil-collection)

(after! wgrep evil-collection-wgrep-setup evil-collection)

(after! which-key evil-collection-which-key-setup evil-collection)

(after! woman evil-collection-woman-setup evil-collection)

(after! xref evil-collection-xref-setup evil-collection)

(after! xwidget evil-collection-xwidget-setup evil-collection)

(after! yaml-mode evil-collection-yaml-mode-setup evil-collection)

(after! youtube-dl evil-collection-youtube-dl-setup evil-collection)

(after! zmusic evil-collection-zmusic-setup evil-collection)

(after! ztree evil-collection-ztree-setup evil-collection)
;;; provide
(provide 'config-evil-collection-setup)
;;; config-evil-collection-setup.el ends here
