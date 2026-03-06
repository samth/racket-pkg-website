#lang racket/base

;; Direct handler tests using web-server/test's make-servlet-tester.
;; Tests the request->response pipeline without starting a real server.
;;
;; We use #:raw? #t and #:headers? #t because the site uses response/output
;; (not response/xexpr), so the tester can't parse responses as XML.

(require rackunit
         racket/string
         racket/list
         racket/promise
         racket/set
         json
         net/url
         web-server/test
         web-server/http
         web-server/http/id-cookie
         web-server/http/cookie
         web-server/http/request-structs
         web-server/http/cookie-parse
         infrastructure-userdb
         "../site.rkt"
         "../sessions.rkt"
         "../users.rkt"
         (submod "../users.rkt" for-testing)
         "../pkg-index/common.rkt"
         (submod "../pkg-index/common.rkt" for-testing)
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

;; --- End-to-end test: account page survives token generation ---
;; This test catches the bug where user-id-for-email returned a list
;; instead of a string after a token was generated (due to property
;; value wrapping in the userdb serialization layer). The account page
;; would crash with an xexpr contract violation on the second render.

;; Helper: make a request struct with a signed session cookie
(define (make-authenticated-request session-key
                                    #:method [method #"GET"]
                                    #:url [url-string "/"]
                                    #:bindings [bindings '()]
                                    #:post-data [post-data #f])
  (define id-cookie (make-id-cookie "pltsession" #:key (session-signing-key) session-key))
  (define set-cookie-str (bytes->string/utf-8 (header-value (cookie->header id-cookie))))
  (define cookie-val (cadr (regexp-match #rx"pltsession=([^;]+)" set-cookie-str)))
  (define cookie-header
    (header #"Cookie"
            (string->bytes/utf-8
             (format "pltsession=~a" cookie-val))))
  (request method
           (string->url url-string)
           (list cookie-header)
           (delay bindings)
           post-data
           "127.0.0.1" 80 "127.0.0.1"))

;; Extract the path portion from a URL, stripping scheme and host.
;; Continuation URLs from send/suspend/dispatch/dynamic have dynamic-urlprefix
;; prepended (e.g. "https://localhost:7443/account;(...)").
(define (strip-to-path url-str)
  (define u (string->url url-str))
  (url->string (struct-copy url u [scheme #f] [host #f] [port #f] [user #f])))

(test-case "e2e: account page renders after token generation"
  (call-with-test-userdb
   (lambda (db)
     (call-with-test-packages-dir
      (lambda (pkgs-dir)
        (initialize-users-for-testing! db (make-registration-state))
        (set-userdb-for-testing! db)

        ;; Create user and session
        (register-or-update-user! "e2e@example.com" "testpass")
        (ensure-user-id! "e2e@example.com")
        (define session-key
          (create-session! "e2e@example.com"))

        ;; Step 1: GET /account — should render with user-id
        (define req1 (make-authenticated-request session-key #:url "/account"))
        (define result1 (tester req1 #:raw? #t #:headers? #t))
        (check-equal? (result-status result1) 200
                      "account page should return 200")
        (define body1 (result-body result1))
        (check-not-false (string-contains? body1 "User ID")
                         "account page should show User ID")
        (check-not-false (string-contains? body1 "Generate Token")
                         "account page should show token generation form")

        ;; Step 2: Extract the token generation form action URL
        ;; The token gen form contains "Generate Token" button — find the action
        ;; for the form that has class "form-inline" (the token generation form)
        (define form-actions
          (regexp-match* #rx"action=\"([^\"]+)\"" body1 #:match-select cadr))
        (check-not-false (pair? form-actions)
                         "should find at least one form action")
        ;; The token generation form is the last one (after password change forms)
        (define token-form-action (strip-to-path (last form-actions)))

        ;; Step 3: POST to generate a token
        (define req2
          (make-authenticated-request session-key
                                     #:method #"POST"
                                     #:url token-form-action
                                     #:bindings (list (binding:form #"token_label" #"test-ci"))
                                     #:post-data #"token_label=test-ci"))
        (define result2 (tester req2 #:raw? #t #:headers? #t))
        (check-equal? (result-status result2) 200
                      "token generation should return 200")
        (define body2 (result-body result2))
        (check-not-false (string-contains? body2 "rpkg_")
                         "should show the generated token")
        (check-not-false (string-contains? body2 "Continue to Account Settings")
                         "should show continue link")

        ;; Step 4: Extract the "Continue to Account Settings" link
        ;; The link is: <a href="...">Continue to Account Settings</a>
        (define continue-match
          (regexp-match #rx"href=\"([^\"]+)\">Continue to Account Settings" body2))
        (check-not-false continue-match
                         "should find continue link URL")
        (define continue-url (strip-to-path (cadr continue-match)))

        ;; Step 5: GET the continue link — renders account page again
        ;; This is where the bug would crash: user-id-for-email returns
        ;; a list instead of a string after the token save/load cycle
        (define req3 (make-authenticated-request session-key #:url continue-url))
        (define result3 (tester req3 #:raw? #t #:headers? #t))
        (check-equal? (result-status result3) 200
                      "account page after token generation should return 200")
        (define body3 (result-body result3))
        (check-not-false (string-contains? body3 "User ID")
                         "account page should still show User ID after token generation")
        (check-not-false (string-contains? body3 "test-ci")
                         "account page should show the new token in the list"))))))
