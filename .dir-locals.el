;; https://superuser.com/questions/740720/reload-minor-mode-in-dir-locals-el/794979#794979
;; https://endlessparentheses.com/a-quick-guide-to-directory-local-variables.html
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html
;; I have had an org configuration for years and I am used to org
;; links and for lack of a standard for link syntax in programming
;; languages I have been using the org link syntax in comments.  Also
;; based on my past with org mode I find the outline syntax useful.
;; For these reasons I want to auto-enable [[][outshine]] and
;; [[][orglink]] in my lisp files.
;; Right now disable this because I get errors before I have orglink and outli
;; installed during package installation.  I do not now if the errors actually
;; stop the packages from being installed or if they have a significant effect
;; in practice--even if the packages fail to install.  But it is certainly
;; uncouth to let this happen.
;; ((nil    . ((mode . outli)
;;             (mode . orglink)))
;;  ("lisp" . ((mode . outli)
;;             (mode . orglink))))
