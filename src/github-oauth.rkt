#lang racket/base
;; GitHub OAuth login flow.
;; Handles authorization URL generation, code-for-token exchange,
;; and fetching user info (GitHub ID + verified emails).

(provide github-authorize-url
         github-exchange-code
         github-get-user-info
         github-oauth-configured?
         validate-csrf-state!)

(require net/http-easy
         net/uri-codec
         racket/random
         file/sha1
         "config.rkt"
         "hash-utils.rkt")

(define-logger racket-pkg-website/github-oauth)

;; Config accessors
(define (github-login-client-id)
  (@ (config) github-login-client-id))

(define (github-login-client-secret)
  (@ (config) github-login-client-secret))

(define (github-oauth-configured?)
  (and (github-login-client-id) (github-login-client-secret) #t))

;; In-memory CSRF state tokens: state-string -> expiry-seconds
(define csrf-states (make-hash))

(define (generate-csrf-state!)
  (define state (bytes->hex-string (crypto-random-bytes 16)))
  (define expiry (+ (current-seconds) 600)) ; 10 minutes
  (hash-set! csrf-states state expiry)
  state)

(define (validate-csrf-state! state)
  (define expiry (hash-ref csrf-states state #f))
  (and expiry
       (> expiry (current-seconds))
       (begin
         (hash-remove! csrf-states state)
         #t)))

(define (expire-csrf-states!)
  (define now (current-seconds))
  (for ([(state expiry) (in-hash csrf-states)])
    (when (<= expiry now)
      (hash-remove! csrf-states state))))

;; Build the GitHub authorization URL
(define (github-authorize-url redirect-uri)
  (define state (generate-csrf-state!))
  (expire-csrf-states!)
  (format "https://github.com/login/oauth/authorize?client_id=~a&redirect_uri=~a&scope=~a&state=~a"
          (github-login-client-id)
          (uri-encode redirect-uri)
          "user:email"
          state))

;; Exchange authorization code for access token
(define (github-exchange-code code redirect-uri)
  (log-racket-pkg-website/github-oauth-info "Exchanging OAuth code for token")
  (define resp
    (post "https://github.com/login/oauth/access_token"
          #:headers (hasheq 'accept "application/json")
          #:form (list (cons 'client_id (github-login-client-id))
                       (cons 'client_secret (github-login-client-secret))
                       (cons 'code code)
                       (cons 'redirect_uri redirect-uri))))
  (define body (response-json resp))
  (response-close! resp)
  (define token (hash-ref body 'access_token #f))
  (define error-desc (hash-ref body 'error_description #f))
  (cond
    [token token]
    [error-desc
     (log-racket-pkg-website/github-oauth-error "GitHub OAuth error: ~a" error-desc)
     #f]
    [else
     (log-racket-pkg-website/github-oauth-error "GitHub OAuth: no access_token in response")
     #f]))

;; Fetch GitHub user info: returns (values github-id github-username verified-emails)
;; or (values #f #f #f) on failure
(define (github-get-user-info access-token)
  (log-racket-pkg-website/github-oauth-info "Fetching GitHub user info")
  (define auth-headers
    (hasheq 'authorization (format "Bearer ~a" access-token) 'user-agent "racket-pkg-website"))
  ;; Get user profile
  (define user-resp (get "https://api.github.com/user" #:headers auth-headers))
  (define user-data (response-json user-resp))
  (response-close! user-resp)
  (define github-id (hash-ref user-data 'id #f))
  (define github-username (hash-ref user-data 'login #f))
  (cond
    [(not github-id)
     (log-racket-pkg-website/github-oauth-error "GitHub API: no user id in response")
     (values #f #f #f)]
    [else
     ;; Get verified emails
     (define emails-resp (get "https://api.github.com/user/emails" #:headers auth-headers))
     (define emails-data (response-json emails-resp))
     (response-close! emails-resp)
     (define verified-emails
       (for/list ([e (in-list emails-data)]
                  #:when (hash-ref e 'verified #f))
         (hash-ref e 'email)))
     (values github-id github-username verified-emails)]))

(module+ for-testing
  (provide generate-csrf-state!
           validate-csrf-state!
           csrf-states))
