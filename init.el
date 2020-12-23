;; -*- lexical-binding: t -*-

(defun void--needs-tangling-p (org-file elisp-file)
  "Return non-nil if ORG-FILE needs to be tangled into ELISP-FILE"
  (or (not (file-exists-p elisp-file))
      (file-newer-than-file-p org-file elisp-file)))

(defun void--org-parse-fn (src-block)
  "Return the string for the body of the source block."
  (org-element-property :value src-block))

(defun void-source-block-strings (org-file)
  "Return a list of source block strings for ORG-FILE."
  (with-temp-buffer
    (insert-file-contents org-file)
    (org-element-map (org-element-parse-buffer) '(src-block) #'void--org-parse-fn)))

(defun void--tangle-org-file (org-file elisp-file)
  "Tangle ORG-FILE to ELISP-FILE."
  (with-temp-file elisp-file
    (insert ";; -*- lexical-binding: t -*-\n\n")
    (insert (string-join (void-source-block-strings org-file) "\n"))))

(let ((file-name-handler-alist nil)
      (gc-cons-threshold most-positive-fixnum)
      (org-file (concat user-emacs-directory "config.org"))
      (elisp-file (concat user-emacs-directory "main.el")))
  (when (void--needs-tangling-p org-file elisp-file)
    (require 'subr-x)
    (require 'org)
    (require 'org-element)
    (void--tangle-org-file org-file elisp-file))
  (load elisp-file nil t))
