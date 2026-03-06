#lang racket/base
;; GitHub OAuth login flow.
;; Handles authorization URL generation, code-for-token exchange,
;; and fetching user info (GitHub ID + verified emails).

(provide github-authorize-url
         github-exchange-code
         github-get-user-info
         github-oauth-configured?
         validate-csrf-state!
         current-github-exchange-code
         current-github-get-user-info)

(require net/http-easy
         net/uri-codec
         racket/list
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
(define csrf-lock (make-semaphore 1))

(define (generate-csrf-state!)
  (define state (bytes->hex-string (crypto-random-bytes 16)))
  (define expiry (+ (current-seconds) 600)) ; 10 minutes
  (call-with-semaphore csrf-lock
    (lambda () (hash-set! csrf-states state expiry)))
  state)

(define (validate-csrf-state! state)
  ;; Atomic check-and-remove so two concurrent callbacks cannot both validate
  ;; the same single-use state.
  (call-with-semaphore csrf-lock
    (lambda ()
      (define expiry (hash-ref csrf-states state #f))
      (and expiry
           (> expiry (current-seconds))
           (begin
             (hash-remove! csrf-states state)
             #t)))))

(define (expire-csrf-states!)
  (define now (current-seconds))
  (call-with-semaphore csrf-lock
    (lambda ()
      ;; Snapshot keys before removing: mutating the hash while iterating
      ;; (via in-hash) is unreliable per the Racket reference.
      (define expired
        (for/list ([(state expiry) (in-hash csrf-states)]
                   #:when (<= expiry now))
          state))
      (for ([state (in-list expired)])
        (hash-remove! csrf-states state)))))

;; Build the GitHub authorization URL
(define (github-authorize-url redirect-uri)
  (define state (generate-csrf-state!))
  (expire-csrf-states!)
  (format "https://github.com/login/oauth/authorize?client_id=~a&redirect_uri=~a&scope=~a&state=~a"
          (github-login-client-id)
          (uri-encode redirect-uri)
          "user:email"
          state))

;; Parse a response body as JSON, always closing the response. Returns #f if the
;; body is not JSON-parseable (e.g. an HTML gateway error page).
(define (response-json/safe resp)
  (dynamic-wind
   void
   (lambda ()
     (with-handlers ([exn:fail? (lambda (_) #f)])
       (response-json resp)))
   (lambda () (response-close! resp))))

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
  (define body (response-json/safe resp))
  (define token (and (hash? body) (hash-ref body 'access_token #f)))
  (define error-desc (and (hash? body) (hash-ref body 'error_description #f)))
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
  (define user-data (response-json/safe user-resp))
  (define github-id (and (hash? user-data) (hash-ref user-data 'id #f)))
  (define github-username (and (hash? user-data) (hash-ref user-data 'login #f)))
  (cond
    [(not github-id)
     (log-racket-pkg-website/github-oauth-error "GitHub API: no user id in response")
     (values #f #f #f)]
    [else
     ;; Get verified emails
     (define emails-resp (get "https://api.github.com/user/emails" #:headers auth-headers))
     (define emails-data (response-json/safe emails-resp))
     (cond
       [(not (list? emails-data))
        (log-racket-pkg-website/github-oauth-error
         "GitHub API: unexpected /user/emails response shape")
        (values #f #f #f)]
       [else
        ;; Sort verified emails with primary first so (car verified-emails)
        ;; gives the user's primary email, not an arbitrary one.
        (define verified-emails
          (for/list ([e (in-list (sort emails-data
                                       (lambda (a b)
                                         (and (hash? a) (hash? b)
                                              (hash-ref a 'primary #f)
                                              (not (hash-ref b 'primary #f))))))]
                     #:when (and (hash? e) (hash-ref e 'verified #f)))
            (hash-ref e 'email)))
        (values github-id github-username verified-emails)])]))

;; Indirection for testing: allows tests to replace GitHub API calls
(define current-github-exchange-code (make-parameter github-exchange-code))
(define current-github-get-user-info (make-parameter github-get-user-info))

(module+ for-testing
  (provide generate-csrf-state!
           validate-csrf-state!
           expire-csrf-states!
           csrf-states))
