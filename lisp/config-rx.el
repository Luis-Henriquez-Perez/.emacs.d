;; https://howardism.org/Technical/Emacs/eshell-why.html
(require 'rx)

;; (rx-define integer (1+ digit))
;; (rx-define float   (seq integer "." integer))
;; (rx-define b256    (seq (optional (or "1" "2"))
;;                         (regexp "[0-9]\\{1,2\\}")))
;; (rx-define ipaddr  (seq b256 "." b256 "." b256 "." b256))
;; (rx-define time    (seq digit (optional digit) ":" (= 2 digit) (optional ":" (= 2 digit))))
;; (rx-define email   (seq (1+ (regexp "[^,< ]")) "@" (1+ (seq (1+ (any alnum "-"))) ".") (1+ alnum)))
;; (rx-define date    (seq (= 2 digit) (or "/" "-") (= 2 digit) (or "/" "-") (= 4 digit)))
;; (rx-define ymd     (seq (= 4 digit) (or "/" "-") (= 2 digit) (or "/" "-") (= 2 digit)))
;; (rx-define uuid    (seq (= 8 hex) "-" (= 3 (seq (= 4 hex) "-")) (= 12 hex)))
;; (rx-define guid    (seq uuid))

;; TODO: figure out where I should put this and how I should load this.
;; https://howardism.org/Technical/Emacs/eshell-why.html
;; (defmacro prx (&rest expressions)
;;   "Convert the rx-compatible regular EXPRESSIONS to PCRE.
;;   Most shell applications accept Perl Compatible Regular Expressions."
;;   `(rxt-elisp-to-pcre (rx ,@expressions)))

(provide 'config-rx)
