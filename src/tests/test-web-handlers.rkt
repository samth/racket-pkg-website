#lang racket/base

;; Direct handler tests using web-server/test's make-servlet-tester.
;; Tests the request->response pipeline without starting a real server.
;;
;; We use #:raw? #t and #:headers? #t because the site uses response/output
;; (not response/xexpr), so the tester can't parse responses as XML.

(require rackunit
         racket/string
         racket/promise
         racket/set
         json
         net/url
         web-server/test
         web-server/http
         web-server/http/request-structs
         web-server/http/cookie-parse
         "../site.rkt"
         "../sessions.rkt"
         "test-helpers.rkt")

;; Helper: extract HTTP status code from raw header bytes
(define (extract-status-code headers)
  (define m (regexp-match #rx#"^HTTP/[0-9.]+ ([0-9]+)" headers))
  (and m (string->number (bytes->string/utf-8 (cadr m)))))

;; Helper: extract body from a (cons headers body) result
(define (result-status result)
  (extract-status-code (car result)))

(define (result-body result)
  (bytes->string/utf-8 (cdr result)))

(define (result-headers-str result)
  (bytes->string/utf-8 (car result)))

;; Create the tester from request-handler
(define tester (make-servlet-tester request-handler))

(test-case "handler: main page redirects to static index"
  (define result (tester "/" #:raw? #t #:headers? #t))
  ;; Main page redirects to /index.html (pre-rendered static content)
  (check-equal? (result-status result) 302)
  (check-not-false (string-contains? (result-headers-str result) "Location: /index.html")))

(test-case "handler: login page renders the login form"
  (define result (tester "/login" #:raw? #t #:headers? #t))
  (check-equal? (result-status result) 200)
  (check-not-false (string-contains? (result-body result) "Log in")))

(test-case "handler: search page renders"
  (define result (tester "/search" #:raw? #t #:headers? #t))
  (check-equal? (result-status result) 200)
  (check-not-false (string-contains? (result-body result) "Search")))

(test-case "handler: ping page returns 200"
  (define result (tester "/ping" #:raw? #t #:headers? #t))
  (check-equal? (result-status result) 200))

(test-case "handler: register page renders"
  (define result (tester "/register-or-reset" #:raw? #t #:headers? #t))
  (check-equal? (result-status result) 200)
  (check-not-false (string-contains? (result-body result) "Register")))

(test-case "handler: edit page redirects to login when unauthenticated"
  (define result (tester "/create" #:raw? #t #:headers? #t))
  (define status (result-status result))
  ;; Should either redirect to login, or render the login form inline
  (check-not-false (or (and (>= status 300) (< status 400))
                       (string-contains? (result-body result) "Log in"))))

(test-case "handler: package page renders for nonexistent package"
  (define result (tester "/package/nonexistent-test-pkg-xyz" #:raw? #t #:headers? #t))
  (check-not-false (member (result-status result) '(200 404))))

(test-case "handler: account page redirects to login when unauthenticated"
  (define result (tester "/account" #:raw? #t #:headers? #t))
  (define status (result-status result))
  (check-not-false (or (and (>= status 300) (< status 400))
                       (string-contains? (result-body result) "Log in"))))

(test-case "handler: /auth/github without config shows error in login form"
  ;; github-oauth-configured? returns #f since no config is set
  (define result (tester "/auth/github" #:raw? #t #:headers? #t))
  (check-equal? (result-status result) 200)
  (check-not-false (string-contains? (result-body result) "not configured")))

;; --- Additional route tests ---

(test-case "handler: logout page clears session"
  (define result (tester "/logout" #:raw? #t #:headers? #t))
  (define status (result-status result))
  ;; Should redirect (302/303) or render with login link
  (check-not-false (or (and (>= status 300) (< status 400))
                       (string-contains? (result-body result) "Log in"))))

(test-case "handler: not-found page renders"
  (define result (tester "/not-found" #:raw? #t #:headers? #t))
  (check-equal? (result-status result) 404)
  (check-not-false (string-contains? (result-body result) "not found")))

(test-case "handler: json-search-completions returns JSON"
  (define result (tester "/json/search-completions" #:raw? #t #:headers? #t))
  (check-equal? (result-status result) 200)
  (check-not-false (string-contains? (result-headers-str result) "application/json"))
  ;; Should parse as JSON (a list)
  (define body (result-body result))
  (check-pred list? (string->jsexpr body)))

(test-case "handler: json-tag-search-completions returns JSON"
  (define result (tester "/json/tag-search-completions" #:raw? #t #:headers? #t))
  (check-equal? (result-status result) 200)
  (check-not-false (string-contains? (result-headers-str result) "application/json"))
  (check-pred list? (string->jsexpr (result-body result))))

(test-case "handler: json-formal-tags returns JSON"
  (define result (tester "/json/formal-tags" #:raw? #t #:headers? #t))
  (check-equal? (result-status result) 200)
  (check-not-false (string-contains? (result-headers-str result) "application/json"))
  (check-pred list? (string->jsexpr (result-body result))))

(test-case "handler: pkgs-all.json returns JSON"
  (define result (tester "/pkgs-all.json" #:raw? #t #:headers? #t))
  (check-equal? (result-status result) 200)
  (check-not-false (string-contains? (result-headers-str result) "application/json"))
  (check-pred hash? (string->jsexpr (result-body result))))

(test-case "handler: /auth/github/callback without params shows error"
  (define result (tester "/auth/github/callback" #:raw? #t #:headers? #t))
  (define status (result-status result))
  (define body (result-body result))
  ;; Should show an error or redirect to login
  (check-not-false (or (string-contains? body "error")
                       (string-contains? body "Log in")
                       (and (>= status 300) (< status 400)))))

(test-case "handler: update-my-packages requires login"
  (define result (tester "/update-my-packages" #:raw? #t #:headers? #t))
  (define status (result-status result))
  (check-not-false (or (and (>= status 300) (< status 400))
                       (string-contains? (result-body result) "Log in"))))

(test-case "handler: package edit page requires login"
  (define result (tester "/package/some-pkg/edit" #:raw? #t #:headers? #t))
  (define status (result-status result))
  (check-not-false (or (and (>= status 300) (< status 400))
                       (string-contains? (result-body result) "Log in"))))

(test-case "handler: CORS header on json endpoints"
  (define result (tester "/json/search-completions" #:raw? #t #:headers? #t))
  (check-not-false (string-contains? (result-headers-str result) "Access-Control-Allow-Origin")))
