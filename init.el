;; -*- lexical-binding: t; -*-

;; The primary goal of this file is to tangle [[file:main.org][main.org]] if it
;; hasn't been tangled already.

;; * Startup Optimizations

;; ** Built-in Libraries

;; *** cl

;; The =cl library= is used by many packages. For many the main reason to
;; require it is to use =cl-loop=. I don't really like =cl-loop= but even still
;; the cl library has a lot of useful functions and macros such as =letf= and =cl-find=.

;; *** subr-x

(eval-when-compile (require 'subr-x))

;; *** rx

(eval-when-compile (require 'rx))

;; * Tangling

;; ** helpers

;; *** check if tangling is needed

;; Laziness is a strength in programming. I only want to tangle
;; =void:main-org-file= when actually need to. Tangling is done at the expense of
;; some (small) initialization time increase. If it can be avoided, all the better.

(defun void:needs-tangling-p ()
  "Whether `void:main-org-file' needs to be tangled.
Tangling needs to occur when either `void:main-elisp-file' does not exist or
`void:main-org-file' is newer than `void:main-elisp-file'."
  (or (not (file-exists-p void:main-elisp-file))
      (file-newer-than-file-p void:main-org-file void:main-elisp-file)))

;; *** write to a file

(defun void:write-file (file contents)
  (let ((file-coding-system-alist nil)
        (coding-system-for-write 'binary))
    (with-temp-file file
      (setq buffer-file-coding-system 'binary)
      (set-buffer-multibyte nil)
      (insert contents))))

;; *** update tags

(defun void:update-tags (last-level curr-level curr-tags all-tags)
  "Returns new tags."
  (let ((prev all-tags))
    (cond ((> last-level curr-level)
           (setq all-tags (nthcdr (1+ (- last-level curr-level)) all-tags))
           (push curr-tags all-tags))
          ((< last-level curr-level)
           (push curr-tags all-tags))
          ((= last-level curr-level)
           (progn (pop all-tags)
                  (push curr-tags all-tags)))))
  all-tags)

;; ** tangle from and to files

(defvar void:main-org-file (concat user-emacs-directory "README.org")
  "Org file containing most of VOID's initialization code.")

(defvar void:main-elisp-file (concat user-emacs-directory ".local/main.el")
  "The elisp file that `void:main-org-file' tangles to.")

;; ** wrap-string

;; This function is for wrapping a string.

(defun void:wrap-string-form (form body)
  (let* ((name (symbol-name (car form)))
         (args (string-join (mapcar #'(lambda (it)
                                        (format "%S" it))
                                    (cdr form))
                            "\s"))
         (body (string-trim-right body)))
    (thread-last body
      (replace-regexp-in-string "\n+\\'\\|^"
                                #'(lambda (it)
                                    (if (string-empty-p it) "\s\s" "")))
      (format "(%s %s\n%s)\n" name args))))

;; ** wrap multiple forms around block


(defun void:wrap-forms (forms block)
  "Wrap."
  (let (final)
    (dolist (form (reverse forms))
      (setq final (void:wrap-string-form form block)))
    final))

;; ** regexps

;; *** headline regexp

;; This regexp was stolen from =org-complex-heading-regexp=.

(defvar void:headline-regexp
  (concat
   ;; (1) stars
   "^\\(\\*+\\)"
   ;; todo
   "\\(?: +\\([[:upper:]]*\\)\\)?"
   ;; priority cookie
   "\\(?: +\\(\\[#.]\\)\\)?"
   ;; headline
   "\\(?: +\\(.*?\\)\\)??"
   ;; tags
   "\\(?:[\011 ]+\\(:[#%:@_[:alnum:]]+:\\)\\)?[\011 ]*$")
  "Match the headline.")

;; *** Source Block

;; I got this regexp from the value of [[helpvar:org-src-block-regxp][org-babel-src-block-regexp]]. I can't just use
;; =org-babel-src-block-regexp= because I'd have to load part of org babel to use
;; it. So here I make my own. I got this =rx= expression by calling
;; [[helpfn:rxt-convert-to-rx][rxt-convert-to-rx]] from [[github:joddie/pcre2el][pcre2el]] on the raw emacs regular expression.

(defvar void:source-block-regexp
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

;; ** tangle macro

(defmacro while-tangling! (file &rest body)
  "Execute body."
  (declare (indent defun))
  `(let* ((case-fold-search nil)
          (heading-or-block-regexp
           ,(concat void:headline-regexp "\\|" void:source-block-regexp))
          (last-level nil)
          (stars) (level 1) (todo-keyword) (priority-cookie)
          (headline) (curr-tags) (all-tags) (indentation) (lang)
          (switches) (body))
     (with-temp-buffer
       (insert-file-contents-literally ,file)
       (goto-char (point-min))
       (while (re-search-forward heading-or-block-regexp nil t nil)
         (when (match-string-no-properties 1)
           (setq last-level level)
           (setq stars (match-string-no-properties 1))
           (setq level (length stars))
           (setq todo-keyword (match-string-no-properties 2))
           (setq priority-cookie (match-string-no-properties 3))
           (setq headline (match-string-no-properties 4))
           (setq curr-tags (split-string (or (match-string-no-properties 5) "") ":" t))
           ;; Update tags
           (setq all-tags (void:update-tags last-level level curr-tags all-tags)))
         (setq indentation (match-string-no-properties 6))
         (setq lang (match-string-no-properties 7))
         (setq switches (match-string-no-properties 8))
         (setq header-arguments (match-string-no-properties 9))
         (setq body (match-string-no-properties 10))
         ,@body))))

;; ** main body

(defun void/tangle-org-file ()
  (interactive)
  (let (code tags)
    (while-tangling! void:main-org-file
      (setq tags (apply #'append all-tags))
      (when (and body (not (member "omit" tags)))
        (setq code
              (concat code
                      (cond
                       ;; ((not (member "noblock" tags))
                       ;;  (void:wrap-string-form `(elisp-block! ,tags) body))
                       ((member "ewc" tags)
                        (concat "\n" (void:wrap-string-form '(eval-when-compile) body)))
                       ((member "eac" tags)
                        (concat "\n" (void:wrap-string-form '(eval-and-compile) body)))
                       ((member "disabled" tags) nil)
                       (t (concat "\n" body)))))))
    (void:write-file void:main-elisp-file (concat ";; -*- lexical-binding: t; -*-\n" code))))

(when (void:needs-tangling-p)
  (void/tangle-org-file))

;; * Main

;; The [[helpvar:file-name-handler-alist][file-name-handler-alist]] is consulted on every call to ~load~ or ~require~.
;; Here I save it's value so that when I set it to ~nil~ I'll get a minor
;; performance boost.

(let (file-name-handler-alist)
  (load void:main-elisp-file :noerror :nomessage))
