;; -*- lexical-binding: t; -*-

;; The primary goal of this file is to tangle [[file:main.org][main.org]] if it
;; hasn't been tangled already.

;; * Startup Optimizations

;; ** Built-in Libraries

;; *** cl

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

(defun void-tangle-org-file-maybe (&optional force-p)
  "A very simple and basic version of `org-babel-tangle-file'.
This function is designed to just do one thing and be fast at doing it."
  (let* ((code nil)
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
  (void-tangle-org-file-maybe)
  (load (concat user-emacs-directory "/.local/main.el")))
