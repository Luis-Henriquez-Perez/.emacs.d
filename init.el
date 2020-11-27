;; -*- lexical-binding: t; -*-

;; The primary goal of this file is to tangle [[file:main.org][main.org]] if it
;; hasn't been tangled already.

;; * Startup Optimizations

;; ** Built-in Libraries

;; *** cl

;; The =cl library= is used by many packages. For many the main reason to
;; require it is to use =cl-loop=. I don't really like =cl-loop= but even still
;; the cl library has a lot of useful functions and macros such as =letf= and =cl-find=.

(defvar void-package-load-paths nil "Load paths for variables.")
(defvar void-package-recipe-alist nil "Package recipe list.")

;; Save hist provides persistent storage in emacs which we can exploit to cache
;; our package load-paths. Let's load the `savehist' file if it exists and if it
;; doesn't we'll have to read the package recipes from our org file.

;; (require 'savehist)
;; (savehist-mode 1)

;; *** install required packages.

;; Emacs has org-mode built-in. I don't think this is a good idea but what's
;; done is done. It at least makes tangling literate configs for the first time
;; more convenient. However, I end up installing the latest org version with
;; straight later, so having two org versions could be problematic.

(defconst void-src-block-regexp
  (concat
   ;; (1) indentation                 (2) lang
   "^\\([ \t]*\\)#\\+begin_src[ \t]+\\([^ \f\t\n\r\v]+\\)[ \t]*"
   ;; (3) switches
   "\\([^\":\n]*\"[^\"\n*]*\"[^\":\n]*\\|[^\":\n]*\\)"
   ;; (4) header arguments
   "\\([^\n]*\\)\n"
   ;; (5) body
   "\\([^\000]*?\n\\)??[ \t]*#\\+end_src")
  "Regexp used to identify code blocks.")

;; Tangle org file if necessary.
(defun void-tangle-org-file-maybe (&optional force-p)
  "A very simple and basic version of `org-babel-tangle-file'.
This function is designed to just do one thing and be fast at doing it."
  (let* ((recipes nil)
         (code nil)
         (org-file (concat user-emacs-directory "main.org"))
         (el-file (concat user-emacs-directory "main.el"))
         (tangle-p (or (not (file-exists-p el-file))
                       (file-newer-than-file-p org-file el-file)))
         (regexp void-src-block-regexp))
    (when (or tangle-p force-p)
      (with-current-buffer (or (get-file-buffer org-file)
                               (create-file-buffer org-file))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward regexp nil t)
            (when-let ((src-block (match-string-no-properties 5))
                       (header-args (or (match-string-no-properties 4) "")))
              (setq code (concat code "\n" (match-string-no-properties 5)))))))
      (with-temp-file el-file
        (insert code)))))

(let ((gc-cons-threshold most-positive-fixnum))
  (void-tangle-org-file-maybe))

(if (bound-and-true-p void-package-load-paths)
    (setq load-path (append void-package-load-paths load-path))
  ;; Otherwise, we need to get the recipes we stored in our org file.
  ()
  )
