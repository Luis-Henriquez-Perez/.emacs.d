;; -*- lexical-binding: t -*-

(defun xl--needs-tangling-p (org-file elisp-file)
  "Return non-nil if ORG-FILE needs to be tangled into ELISP-FILE"
  (or (not (file-exists-p elisp-file))
      (file-newer-than-file-p org-file elisp-file)))

(defvar xl-source-block-regexp
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

(defun xl--tangle-org-file (org-file elisp-file)
  "Tangle ORG-FILE to ELISP-FILE."
  (let ((contents ""))
    (with-temp-buffer
      (insert-file-contents org-file)
      (goto-char (point-min))
      (while (re-search-forward xl-source-block-regexp nil t nil)
	(setq contents (concat contents (match-string 5) "\n"))))
    (with-temp-file elisp-file
      (insert ";; -*- lexical-binding: t -*-\n\n")
      (insert contents))))

(let ((file-name-handler-alist nil)
      (gc-cons-threshold most-positive-fixnum)
      (org-file (concat user-emacs-directory "config.org"))
      (elisp-file (concat user-emacs-directory "main.el")))
  (when (xl--needs-tangling-p org-file elisp-file)
    (xl--tangle-org-file org-file elisp-file))
  (load elisp-file nil t))
