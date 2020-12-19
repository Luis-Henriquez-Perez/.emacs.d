;; -*- lexical-binding: t -*-

;; The primary goal of this file is to tangle [[file:main.org][main.org]] if it
;; hasn't been tangled already.

;; * Startup Optimizations

;; ** Built-in Libraries

;; ** helpers

;; *** check if tangling is needed

;; I only want to tangle =void-main-org-file= when actually need to. Tangling is
;; done at the expense of some (small) initialization time increase. If it can be
;; avoided, all the better.

(defun void-needs-tangling-p ()
  "Whether `void-main-org-file' needs to be tangled.
Tangling needs to occur when either `void-main-elisp-file' does not exist or
`void-main-org-file' is newer than `void-main-elisp-file'."
  (or (not (file-exists-p void-main-elisp-file))
      (file-newer-than-file-p void-main-org-file void-main-elisp-file)))

;; *** tangling

;; ** tangle from and to files

(defvar void-main-org-file (concat user-emacs-directory "config.org")
  "Org file containing most of VOID's initialization code.")

(defvar void-main-elisp-file (concat user-emacs-directory "main.el")
  "The elisp file that `void-main-org-file' tangles to.")

;; ** main body

(defun void-tangle-org-file ()
  (let ((code ""))
    (org-babel-map-src-blocks void-main-org-file
      (setq code (concat code body "\n")))
    (with-temp-file void-main-elisp-file
      (insert ";; -*- lexical-binding: t -*-\n\n")
      (insert code))))

(defun void-tangle-org-file-maybe ()
  (when (void-needs-tangling-p)
    (require 'org)
    (void/tangle-org-file)))

(defun void/tangle-org-file ()
  (interactive)
  (void-tangle-org-file))

(let ((file-name-handler-alist nil)
      (gc-cons-threshold most-positive-fixnum))
  (void-tangle-org-file-maybe)
  (load void-main-elisp-file))
