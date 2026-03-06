#lang racket/base

(require rackunit
         racket/string
         racket/file
         racket/port
         racket/runtime-path
         racket/tcp
         json
         net/url
         web-server/servlet-env
         web-server/servlet
         web-server/http/request-structs
         reloadable
         "../github-oauth.rkt"
         "../config.rkt"
         (submod "../github-oauth.rkt" for-testing))

;; Access the shared config state (same symbol as config.rkt uses)
(define config-handler (make-persistent-state '*config* (lambda () (hash))))
(define original-config (config-handler))

;; Read PAT from environment variable or local file (never committed).
;; Tests that need a real token skip when neither is available or the token is invalid.
(define-runtime-path pat-file "../../samth_pat.txt")
(define (non-empty-string s) (and s (not (string=? s "")) s))
(define github-pat-raw
  (or (non-empty-string (getenv "GITHUB_PAT"))
      (and (file-exists? pat-file) (non-empty-string (string-trim (file->string pat-file))))))
;; Validate the token actually works before using it for tests
(define github-pat
  (and github-pat-raw
       (let-values ([(id _u _e) (github-get-user-info github-pat-raw)])
         (and id github-pat-raw))))

(test-case "github-oauth-configured? returns #f without config"
  (check-false (github-oauth-configured?)))

(test-case "CSRF state: generate returns hex string"
  (define state (generate-csrf-state!))
  (check-pred string? state)
  (check-equal? (string-length state) 32)) ; 16 bytes = 32 hex chars

(test-case "CSRF state: validate succeeds for fresh state"
  (define state (generate-csrf-state!))
  (check-true (validate-csrf-state! state)))

(test-case "CSRF state: validate consumes state (single-use)"
  (define state (generate-csrf-state!))
  (check-true (validate-csrf-state! state))
  (check-false (validate-csrf-state! state)))

(test-case "CSRF state: validate rejects unknown state"
  (check-false (validate-csrf-state! "nonexistent-state-value")))

(test-case "CSRF state: expired state is rejected"
  (define state (generate-csrf-state!))
  ;; Manually expire it by setting expiry to the past
  (hash-set! csrf-states state 0)
  (check-false (validate-csrf-state! state)))

;; --- Real GitHub API tests (skipped when no PAT is available) ---

(when github-pat
  (test-case "real GitHub API: github-get-user-info returns valid structure"
    (define-values (id username emails) (github-get-user-info github-pat))
    (check-pred number? id "github-id should be a number")
    (check-pred string? username "github-username should be a string")
    (check-pred pair? emails "verified-emails should be a non-empty list")
    (check-true (andmap string? emails) "each email should be a string"))

  (test-case "real GitHub API: primary email is first"
    (define-values (id username emails) (github-get-user-info github-pat))
    ;; The first email should be the primary. We can't know the exact
    ;; address, but we can verify it looks like an email.
    (check-pred string? (car emails))
    (check-not-false (regexp-match #rx"@" (car emails)) "first email should contain @"))

  (test-case "real GitHub API: samth account identity"
    (define-values (id username emails) (github-get-user-info github-pat))
    (check-equal? username "samth" "PAT belongs to samth"))

  (test-case "real GitHub API: invalid token returns #f values"
    (define-values (id username emails) (github-get-user-info "ghp_invalid_token_value"))
    (check-false id)
    (check-false username)
    (check-false emails)))

;; --- Mock GitHub server tests ---
;; Tests the real HTTP client code in github-oauth.rkt against a local server.

;; Mock server handler: mimics GitHub's OAuth token and API endpoints.
(define (mock-github-handler req)
  (define path (url->string (request-uri req)))
  (define (json-response data)
    (response/output (lambda (out) (write-json data out))
                     #:code 200
                     #:headers (list (make-header #"Content-Type" #"application/json"))))
  (cond
    ;; POST /login/oauth/access_token
    [(string-contains? path "/login/oauth/access_token")
     (define body-str (bytes->string/utf-8 (or (request-post-data/raw req) #"")))
     (cond
       [(string-contains? body-str "code=valid-code")
        (json-response (hasheq 'access_token "mock-token-12345" 'token_type "bearer"))]
       [(string-contains? body-str "code=error-code")
        (json-response
         (hasheq 'error "bad_verification_code" 'error_description "The code passed is incorrect"))]
       [else (json-response (hasheq 'error "bad_verification_code"))])]
    ;; GET /user/emails (must check before /user)
    [(string-contains? path "/user/emails")
     (json-response (list (hasheq 'email "secondary@mock.com" 'verified #t 'primary #f)
                          (hasheq 'email "primary@mock.com" 'verified #t 'primary #t)
                          (hasheq 'email "unverified@mock.com" 'verified #f 'primary #f)))]
    ;; GET /user
    [(string-contains? path "/user") (json-response (hasheq 'id 77777 'login "mockuser"))]
    [else (response/output (lambda (out) (write-string "not found" out)) #:code 404)]))

;; Start mock server on a random port, run thunk, shut down.
(define (call-with-mock-github thunk)
  (define listener (tcp-listen 0 5 #t "127.0.0.1"))
  (define-values (_la port _ra _rp) (tcp-addresses listener #t))
  (tcp-close listener)
  (define base (format "http://127.0.0.1:~a" port))
  (define server-thread
    (thread (lambda ()
              (parameterize ([current-output-port (open-output-nowhere)])
                (serve/servlet mock-github-handler
                               #:port port
                               #:listen-ip "127.0.0.1"
                               #:servlet-regexp #rx""
                               #:servlet-path "/"
                               #:launch-browser? #f)))))
  (sleep 1)
  (dynamic-wind void
                (lambda ()
                  (parameterize ([current-github-oauth-base base]
                                 [current-github-api-base base])
                    (thunk base)))
                (lambda () (kill-thread server-thread))))

;; Helper: run thunk with OAuth config set, restore after
(define (with-mock-oauth-config thunk)
  (config-handler (hash-set* original-config
                              'github-login-client-id "test-id"
                              'github-login-client-secret "test-secret"))
  (dynamic-wind void thunk (lambda () (config-handler original-config))))

(test-case "mock server: github-exchange-code succeeds with valid code"
  (call-with-mock-github
   (lambda (base)
     (with-mock-oauth-config
      (lambda ()
        (define token (github-exchange-code "valid-code" "http://localhost/callback"))
        (check-equal? token "mock-token-12345"))))))

(test-case "mock server: github-exchange-code returns #f for bad code"
  (call-with-mock-github
   (lambda (base)
     (with-mock-oauth-config
      (lambda ()
        (define token (github-exchange-code "error-code" "http://localhost/callback"))
        (check-false token))))))

(test-case "mock server: github-get-user-info returns correct structure"
  (call-with-mock-github (lambda (base)
                           (define-values (id username emails) (github-get-user-info "any-token"))
                           (check-equal? id 77777)
                           (check-equal? username "mockuser")
                           (check-pred pair? emails))))

(test-case "mock server: primary email sorted first"
  (call-with-mock-github
   (lambda (base)
     (define-values (id username emails) (github-get-user-info "any-token"))
     ;; primary@mock.com should be first despite secondary@ being first in the response
     (check-equal? (car emails) "primary@mock.com")
     (check-equal? (cadr emails) "secondary@mock.com"))))

(test-case "mock server: unverified emails filtered out"
  (call-with-mock-github (lambda (base)
                           (define-values (id username emails) (github-get-user-info "any-token"))
                           (check-equal? (length emails) 2)
                           (check-false (member "unverified@mock.com" emails)))))
