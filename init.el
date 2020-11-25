;; -*- lexical-binding: t; -*-

;; The primary goal of this file is to tangle [[file:main.org][main.org]] if it
;; hasn't been tangled already.

;; * Startup Optimizations

;; ** Built-in Libraries

;; *** cl

;; The =cl library= is used by many packages. For many the main reason to
;; require it is to use =cl-loop=. I don't really like =cl-loop= but even still
;; the cl library has a lot of useful functions and macros such as =letf= and =cl-find=.

(defun void-convert-string-to-comment (string)
  (replace-regexp-in-string (rx bol) ";; " string))

;; Get comfortable, this will take a while.
(let (code recipes)
  (require 'cl)
  (require 'org)
  (require 'org-element)
  ;; Get the properties of each headline. Specifically, we're interested in the
  ;; ones that mean the headline is a package we have to install.

  ;; So we need to do two things. One is get the properties. Two is get the
  ;; source blocks.
  (org-element-map (with-current-buffer (get-file-buffer "main.org")
                     (org-element-parse-buffer))
      '(src-block property-drawer paragraph)
    (lambda (it)
      (cl-case (org-element-type it)
        (src-block (push (org-element-property :value it) code))
        (paragraph (push (replace-regexp-in-string (rx bol) ";; " string) code)))))
  (with-temp-buffer (nreverse code)))
