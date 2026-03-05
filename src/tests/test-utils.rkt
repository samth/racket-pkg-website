#lang racket/base
;; Tests for utility modules: version, gravatar, display-name, default,
;; challenge, rpc, package-source, github-oauth.

(require rackunit
         racket/string
         racket/list
         racket/match
         racket/set)

;; ---- version.rkt ----

(require "../version.rkt")

(test-case "sort-version-symbols: default comes first"
  (check-equal? (sort-version-symbols '(default)) '(default))
  (check-equal? (first (sort-version-symbols '(|8.0| default |7.0|))) 'default))

(test-case "sort-version-symbols: valid Racket versions sorted numerically"
  (define sorted (sort-version-symbols '(|8.0| |7.5| |8.1| |7.0|)))
  (check-equal? sorted '(|7.0| |7.5| |8.0| |8.1|)))

(test-case "sort-version-symbols: mixed default, versions, and other"
  (define sorted (sort-version-symbols '(|8.0| default beta |7.0| alpha)))
  (check-equal? (first sorted) 'default)
  ;; Racket versions come before non-versions
  (check-equal? (second sorted) '|7.0|)
  (check-equal? (third sorted) '|8.0|)
  ;; Non-versions sorted alphabetically
  (check-equal? (fourth sorted) 'alpha)
  (check-equal? (fifth sorted) 'beta))

(test-case "sort-version-symbols: empty list"
  (check-equal? (sort-version-symbols '()) '()))

;; ---- gravatar.rkt ----

(require "../gravatar.rkt")

(test-case "gravatar-hash: produces md5 hex string"
  (define h (gravatar-hash "test@example.com"))
  (check-pred string? h)
  (check-equal? (string-length h) 32)
  (check-regexp-match #px"^[0-9a-f]{32}$" h))

(test-case "gravatar-hash: case insensitive and trims whitespace"
  (check-equal? (gravatar-hash "Test@Example.COM") (gravatar-hash "test@example.com"))
  (check-equal? (gravatar-hash "  test@example.com  ") (gravatar-hash "test@example.com")))

(test-case "gravatar-image-url: default format"
  (define url (gravatar-image-url "test@example.com"))
  (check-not-false (string-contains? url "gravatar.com/avatar/"))
  (check-not-false (string-contains? url "s=80"))
  (check-not-false (string-contains? url "d=identicon")))

(test-case "gravatar-image-url: custom size and default"
  (define url (gravatar-image-url "test@example.com" 200 #:default "retro"))
  (check-not-false (string-contains? url "s=200"))
  (check-not-false (string-contains? url "d=retro")))

;; ---- display-name.rkt ----

(require "../display-name.rkt")

(test-case "display-name->preferred-tag: returns a symbol"
  (define dn (email->display-name "test@example.com"))
  (check-pred symbol? (display-name->preferred-tag dn))
  (check-pred symbol? (display-name->preferred-tag dn #:obfuscate? #t))
  (check-pred symbol? (display-name->preferred-tag dn #:obfuscate? #f)))

(test-case "display-name->xexpr: obfuscated returns span structure"
  (define dn (email->display-name "test@example.com"))
  (define x (display-name->xexpr dn #:obfuscate? #t))
  (check-equal? (car x) 'span))

(test-case "display-name->xexpr: non-obfuscated returns email string"
  (define dn (email->display-name "test@example.com"))
  (define x (display-name->xexpr dn #:obfuscate? #f))
  (check-pred string? x)
  (check-equal? x "test@example.com"))

;; ---- default.rkt ----

(require "../default.rkt")

(test-case "extract-pkg-index-config: propagates root and port"
  (define config (hash 'root "/test/root" 'pkg-index-port 9999 'ssl? #f 'pkg-index (hash)))
  (define pi (extract-pkg-index-config config))
  (check-equal? (hash-ref pi 'root) "/test/root")
  (check-equal? (hash-ref pi 'port) 9999)
  (check-equal? (hash-ref pi 'ssl?) #f))

(test-case "extract-pkg-index-config: does not override existing keys"
  (define config (hash 'root "/test/root" 'pkg-index (hash 'root "/override")))
  (define pi (extract-pkg-index-config config))
  (check-equal? (hash-ref pi 'root) "/override"))

(test-case "extract-pkg-index-config: uses defaults for missing keys"
  (define pi (extract-pkg-index-config (hash)))
  (check-not-false (hash-ref pi 'root))
  (check-pred number? (hash-ref pi 'port)))

(test-case "extract-backup-config: propagates root"
  (define config (hash 'root "/backup/root" 'backup (hash)))
  (define bc (extract-backup-config config))
  (check-equal? (hash-ref bc 'root) "/backup/root"))

(test-case "extract-backup-config: does not override existing"
  (define config (hash 'root "/test" 'backup (hash 'root "/keep")))
  (define bc (extract-backup-config config))
  (check-equal? (hash-ref bc 'root) "/keep"))

;; ---- challenge.rkt ----

(require "../challenge.rkt")

(test-case "generate-challenge: produces valid challenge struct"
  (define c (generate-challenge))
  (check-pred challenge? c)
  (check-pred number? (challenge-answer c))
  (check-pred pair? (challenge-question c)))

(test-case "challenge-passed?: correct answer"
  (define c (generate-challenge))
  (define answer-str (format "~a" (challenge-answer c)))
  (check-true (challenge-passed? c answer-str)))

(test-case "challenge-passed?: wrong answer"
  (define c (generate-challenge))
  (check-false (challenge-passed? c "99999999")))

(test-case "challenge-passed?: non-numeric answer"
  (define c (generate-challenge))
  (check-false (challenge-passed? c "not-a-number")))

;; ---- rpc.rkt ----

(require "../rpc.rkt")

(test-case "rpc-call: round-trip with echo server"
  (define server
    (thread (lambda ()
              (let loop ()
                (define msg (thread-receive))
                (match msg
                  [(cons ch (list 'echo val))
                   (when ch
                     (channel-put ch val))
                   (loop)]
                  [(cons ch (list 'stop))
                   (when ch
                     (channel-put ch 'done))])))))
  (check-equal? (rpc-call server 'echo 42) 42)
  (check-equal? (rpc-call server 'echo "hello") "hello")
  (check-equal? (rpc-call server 'stop) 'done))

(test-case "rpc-call: raises on server error"
  (define server
    (thread (lambda ()
              (define msg (thread-receive))
              (match msg
                [(cons ch _)
                 (when ch
                   (channel-put ch (exn:fail "boom" (current-continuation-marks))))]))))
  (check-exn exn:fail:rpc? (lambda () (rpc-call server 'anything))))

(test-case "rpc-cast!: does not block"
  (define received (make-channel))
  (define server
    (thread (lambda ()
              (define msg (thread-receive))
              (channel-put received (cdr msg)))))
  (rpc-cast! server 'fire-and-forget 123)
  (check-equal? (sync/timeout 2 received) '(fire-and-forget 123)))

;; ---- package-source.rkt ----

(require "../package-source.rkt")

(test-case "parse-package-source: github URL"
  (define-values (parsed complaints) (parse-package-source "https://github.com/racket/racket.git"))
  (check-pred git-source? parsed)
  (check-equal? (parsed-package-source-type parsed) 'git))

(test-case "parse-package-source: simple HTTP URL"
  (define-values (parsed complaints) (parse-package-source "https://example.com/pkg.tar.gz"))
  (check-pred parsed-package-source? parsed))

(test-case "parse-package-source: local path rejected"
  (define-values (parsed complaints) (parse-package-source "/tmp/some-pkg"))
  (check-not-false (ormap (lambda (c) (string-contains? c "local")) complaints)))

(test-case "parsed-package-source-human-url: git source"
  (define-values (parsed _) (parse-package-source "https://github.com/racket/racket.git"))
  (check-pred string? (parsed-package-source-human-url parsed)))

(test-case "parsed-package-source-human-tree-url: github source"
  (define-values (parsed _) (parse-package-source "https://github.com/racket/racket.git"))
  (define tree-url (parsed-package-source-human-tree-url parsed))
  (check-pred string? tree-url)
  (check-not-false (string-contains? tree-url "github.com")))

(test-case "unparse-package-source: round-trip for git source"
  (define-values (parsed _) (parse-package-source "https://github.com/racket/racket.git"))
  (check-pred string? (unparse-package-source parsed)))

(test-case "package-source->human-url: convenience wrapper"
  (define url (package-source->human-url "https://github.com/racket/racket.git"))
  (check-pred string? url))

(test-case "package-source->human-tree-url: convenience wrapper"
  (define url (package-source->human-tree-url "https://github.com/racket/racket.git"))
  (check-pred string? url))

;; ---- github-oauth.rkt ----

(require "../github-oauth.rkt"
         (submod "../github-oauth.rkt" for-testing))

(test-case "github-oauth-configured?: returns #f with no config"
  ;; The test config doesn't set github-login-client-id
  (check-false (github-oauth-configured?)))

(test-case "expire-csrf-states!: removes expired entries"
  (define old-state (bytes->hex-string (crypto-random-bytes 16)))
  ;; Manually insert an expired state
  (hash-set! csrf-states old-state (- (current-seconds) 100))
  (define fresh (generate-csrf-state!))
  (expire-csrf-states!)
  (check-false (hash-has-key? csrf-states old-state))
  (check-true (hash-has-key? csrf-states fresh))
  ;; Clean up
  (hash-remove! csrf-states fresh))

(require racket/random
         file/sha1)
