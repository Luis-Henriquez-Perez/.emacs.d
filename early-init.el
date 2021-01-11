;; -*- lexical-binding: t -*-

;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;; * gc-cons-threshold

;; Defer garbage collection further back in the startup process

(setq gc-cons-threshold most-positive-fixnum)

;; * Prevent

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'.

(setq package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)

;; * Disable Tool Bar, Menu Bar, and Scroll Bar

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.

(setq frame-inhibit-implied-resize t)

;; * Ignore x-resources

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;; * Prevent Default Mode Line from Showing

;; Prevent early display of modeline.

(unless after-init-time
  (setq-default mode-line-format nil))

;; [[helpfn:set-frindge-style][set-frindge-style]]
(push (cons 'left-fringe 0) default-frame-alist)
(push (cons 'right-fringe 0) default-frame-alist)
