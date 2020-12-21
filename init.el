;; -*- lexical-binding: t -*-

;; The primary goal of this file is to tangle [[file:main.org][main.org]] if it
;; hasn't been tangled already.

;; * Startup Optimizations

;; ** Built-in Libraries

;; ** helpers

;; *** check if tangling is needed

;; I only want to tangle =VOID-MAIN-ORG-FILE= when actually need to. Tangling is
;; done at the expense of some (small) initialization time increase. If it can be
;; avoided, all the better.

(defun void-needs-tangling-p ()
  "Whether `VOID-MAIN-ORG-FILE' needs to be tangled.
Tangling needs to occur when either `VOID-MAIN-ELISP-FILE' does not exist or
`VOID-MAIN-ORG-FILE' is newer than `VOID-MAIN-ELISP-FILE'."
  (or (not (file-exists-p VOID-MAIN-ELISP-FILE))
      (file-newer-than-file-p VOID-MAIN-ORG-FILE VOID-MAIN-ELISP-FILE)))

;; *** tangling

;; ** tangle from and to files

(defconst VOID-MAIN-ORG-FILE (concat user-emacs-directory "config.org")
  "Org file containing most of VOID's initialization code.")

(defconst VOID-MAIN-ELISP-FILE (concat user-emacs-directory "main.el")
  "The elisp file that `VOID-MAIN-ORG-FILE' tangles to.")

;; ** main body

(defun void-tangle-org-file ()
  (let ((code ""))
    (org-babel-map-src-blocks VOID-MAIN-ORG-FILE
      (setq code (concat code body "\n")))
    (with-temp-file VOID-MAIN-ELISP-FILE
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
  (load VOID-MAIN-ELISP-FILE nil t))
