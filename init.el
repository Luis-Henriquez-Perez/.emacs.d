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

(require 'savehist)
(savehist-mode 1)

(if (bound-and-true-p void-package-load-paths)
    (setq load-path (append void-package-load-paths load-path))
  ;; Otherwise, we need to get the recipes we stored in our org file.
  ()
  )

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

(setq void-property-drawer-re
      (rx (seq bol (zero-or-more (any "\11 "))
               ":PROPERTIES:" (zero-or-more (any "\11 "))
               "\n" (*\? (zero-or-more (any "\11 "))
                         (group ":" (one-or-more (not (syntax whitespace)))
                                ":" (opt " " (zero-or-more nonl)))
                         (zero-or-more (any "\11 "))
                         "\n")
               (zero-or-more (any "\11 "))
               ":END:" (zero-or-more (any "\11 "))
               eol))
      ;; (concat "^[ \t]*:PROPERTIES:[ \t]*\n"
	  ;;         "\\(?:[ \t]*:\\S-+:\\(?: .*\\)?[ \t]*\n\\)*?"
	  ;;         "[ \t]*:END:[ \t]*$")
      ;; "Matches an entire property drawer."
      )
(xr void-property-drawer-re)



(let* ((gc-cons-threshold most-positive-fixnum)
       (recipes nil)
       (code nil)
       (org-file (concat user-emacs-directory "main.org"))
       (el-file (concat user-emacs-directory ".local/main.el"))
       (tangle-p (or (not (file-exists-p el-file))
                     (file-newer-than-file-p org-file el-file)))
       (regexp void-property-drawer-re)
       (count 0)
       ;; (regexp (rx (or (regexp void-src-block-regexp)
       ;;                 (regexp void-property-drawer-re))))
       )
  (with-current-buffer (or (get-file-buffer org-file)
                           (create-file-buffer org-file))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        ;; source block
        (cond (
               nil
               ;; (match-string-no-properties 5)
               (setq code (concat (match-string-no-properties 5) "\n" code)))
              ;; property drawer
              ((match-string-no-properties 1)
               (push (match-string-no-properties 1) recipes)))
        ))
    )
  ;; (list (length recipes) (s-truncate 10 code) (s-truncate 10 (car recipes)))
  (car recipes)
  ;; (when tangle-p
  ;;   (with-temp-file (concat user-emacs-directory ".local/config.el")
  ;;     (insert code)))
  )

":ID:       a570f63a-3ec0-4571-81ed-767801d2b49a"

(712 nil ":PROPER...")

(712 nil nil)

(712 nil nil)

;; (712 nil ":PROPER...")

(712 nil nil)

(nil nil)

(nil nil)

("\n\n\n\n\n\n\n..." nil)

("\n\n\n(def..." nil)


nil


;; Get comfortable, this will take a while.
;; (let ((code "")
;;       (recipes nil)
;;       (gc-cons-threshold most-positive-fixnum)
;;       (gc-cons-percentage 0.7))
;;   (require 'cl)
;;   (require 'org)
;;   (require 'org-element)
;;   ;; Get the properties of each headline. Specifically, we're interested in the
;;   ;; ones that mean the headline is a package we have to install.

;;   ;; So we need to do two things. One is get the properties. Two is get the
;;   ;; source blocks.
;;   (org-element-map (with-current-buffer (get-file-buffer "main.org")
;;                      (prog1 (org-element-parse-buffer)
;;                        (kill-current-buffer)))
;;       '(property-drawer)
;;     (lambda (elem)
;;       (cl-case (org-element-type elem)
;;         (property-drawer
;;          (mapcan (lambda (pnode)
;;                    (list (org-element-property :key pnode)
;;                          (org-element-property :value pnode)))
;;                  (org-element-contents elem))
;;          (when (member "PACKAGE")
;;            ))
;;         (src-block
;;          (setq code
;;                (concat (org-element-property :contents elem) "\n" code))))))
(unless (file-exists-p (concat user-emacs-directory "config.el"))
  (with-temp-file (concat user-emacs-directory "config.el")
    (insert (string-join code "\n"))))
;;   (push 'void-package-recipe-cache savehist-additional-variables))

(unload-feature 'org-element t)
(unload-feature 'org t)
