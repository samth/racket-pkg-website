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
         "../pkg-index/api.rkt"
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

;; --- Authenticated multi-step flows ---

;; Helper: set up test env with a logged-in user, run thunk with session-key
(define (call-with-logged-in-user email password thunk)
  (call-with-test-userdb
   (lambda (db)
     (call-with-test-packages-dir
      (lambda (pkgs-dir)
        (initialize-users-for-testing! db (make-registration-state))
        (set-userdb-for-testing! db)
        (register-or-update-user! email password)
        (ensure-user-id! email)
        (define session-key (create-session! email))
        (thunk session-key))))))

(test-case "e2e: change password via account page"
  (call-with-logged-in-user "pwchange@example.com" "oldpass"
    (lambda (session-key)
      ;; Step 1: GET /account
      (define result1 (tester (make-authenticated-request session-key #:url "/account")
                              #:raw? #t #:headers? #t))
      (check-equal? (result-status result1) 200)
      (define body1 (result-body result1))
      (check-not-false (string-contains? body1 "Change Password"))

      ;; Step 2: Find the password change form action (first form after Account Info)
      (define form-actions
        (regexp-match* #rx"action=\"([^\"]+)\"" body1 #:match-select cadr))
      ;; Password change form is the first form with an action
      (define pw-form-action (strip-to-path (first form-actions)))

      ;; Step 3: POST password change
      (define req2
        (make-authenticated-request session-key
                                    #:method #"POST"
                                    #:url pw-form-action
                                    #:bindings (list (binding:form #"current_password" #"oldpass")
                                                     (binding:form #"new_password" #"newpass")
                                                     (binding:form #"confirm_password" #"newpass"))
                                    #:post-data #"current_password=oldpass&new_password=newpass&confirm_password=newpass"))
      (define result2 (tester req2 #:raw? #t #:headers? #t))
      (check-equal? (result-status result2) 200)
      (define body2 (result-body result2))
      (check-not-false (string-contains? body2 "Password changed successfully")
                       "should show success message")

      ;; Verify: old password no longer works, new one does
      (check-false (login-password-correct? "pwchange@example.com" "oldpass"))
      (check-not-false (login-password-correct? "pwchange@example.com" "newpass")))))

(test-case "e2e: change password fails with wrong current password"
  (call-with-logged-in-user "pwfail@example.com" "realpass"
    (lambda (session-key)
      ;; Step 1: GET /account
      (define result1 (tester (make-authenticated-request session-key #:url "/account")
                              #:raw? #t #:headers? #t))
      (define form-actions
        (regexp-match* #rx"action=\"([^\"]+)\"" (result-body result1) #:match-select cadr))
      (define pw-form-action (strip-to-path (first form-actions)))

      ;; Step 2: POST with wrong current password
      (define req2
        (make-authenticated-request session-key
                                    #:method #"POST"
                                    #:url pw-form-action
                                    #:bindings (list (binding:form #"current_password" #"wrongpass")
                                                     (binding:form #"new_password" #"newpass")
                                                     (binding:form #"confirm_password" #"newpass"))
                                    #:post-data #"current_password=wrongpass&new_password=newpass&confirm_password=newpass"))
      (define result2 (tester req2 #:raw? #t #:headers? #t))
      (check-equal? (result-status result2) 200)
      (check-not-false (string-contains? (result-body result2) "incorrect")
                       "should show error about incorrect password")

      ;; Password unchanged
      (check-not-false (login-password-correct? "pwfail@example.com" "realpass")))))

(test-case "e2e: generate and revoke a token"
  (call-with-logged-in-user "revoke@example.com" "pass"
    (lambda (session-key)
      ;; Step 1: GET /account
      (define result1 (tester (make-authenticated-request session-key #:url "/account")
                              #:raw? #t #:headers? #t))
      (check-equal? (result-status result1) 200)
      (define body1 (result-body result1))

      ;; Verify: no tokens yet in backend
      (check-equal? (list-api-tokens "revoke@example.com") '()
                    "should start with no tokens")

      ;; Step 2: Generate a token
      (define form-actions1
        (regexp-match* #rx"action=\"([^\"]+)\"" body1 #:match-select cadr))
      (define token-form-action (strip-to-path (last form-actions1)))
      (define req2
        (make-authenticated-request session-key
                                    #:method #"POST"
                                    #:url token-form-action
                                    #:bindings (list (binding:form #"token_label" #"deploy-key"))
                                    #:post-data #"token_label=deploy-key"))
      (define result2 (tester req2 #:raw? #t #:headers? #t))
      (check-equal? (result-status result2) 200)
      (define body2 (result-body result2))
      (check-not-false (string-contains? body2 "rpkg_"))

      ;; Verify: token exists in backend
      (define tokens-after (list-api-tokens "revoke@example.com"))
      (check-equal? (length tokens-after) 1 "should have 1 token in backend")
      (check-equal? (cadr (car tokens-after)) "deploy-key"
                    "token label should be deploy-key")

      ;; Extract the plaintext token and verify it validates
      (define token-match (regexp-match #rx"(rpkg_[0-9a-f]+)" body2))
      (check-not-false token-match "should find token plaintext")
      (define plaintext (cadr token-match))
      (check-equal? (validate-api-token plaintext) "revoke@example.com"
                    "token should validate to the user's email")

      ;; Step 3: Follow continue link back to account
      (define continue-match
        (regexp-match #rx"href=\"([^\"]+)\">Continue to Account Settings" body2))
      (define continue-url (strip-to-path (cadr continue-match)))
      (define result3 (tester (make-authenticated-request session-key #:url continue-url)
                              #:raw? #t #:headers? #t))
      (check-equal? (result-status result3) 200)
      (define body3 (result-body result3))
      (check-not-false (string-contains? body3 "deploy-key")
                       "token should appear in the list")

      ;; Step 4: Find and click the revoke button for the token
      (define revoke-actions
        (regexp-match* #rx"action=\"([^\"]+)\"[^>]*>[^<]*<button[^>]*>Revoke" body3
                       #:match-select cadr))
      (check-not-false (pair? revoke-actions) "should find revoke form")
      (define revoke-url (strip-to-path (car revoke-actions)))
      (define req4
        (make-authenticated-request session-key #:method #"POST" #:url revoke-url
                                    #:post-data #""))
      (define result4 (tester req4 #:raw? #t #:headers? #t))
      (check-equal? (result-status result4) 200)
      (check-not-false (string-contains? (result-body result4) "revoked")
                       "should show token revoked message")

      ;; Verify: token no longer validates in backend
      (check-false (validate-api-token plaintext)
                   "revoked token should no longer validate")
      (check-equal? (list-api-tokens "revoke@example.com") '()
                    "should have no tokens after revocation"))))

(test-case "e2e: create a new package via the web form"
  (call-with-logged-in-user "creator@example.com" "pass"
    (lambda (session-key)
      ;; Verify: package does not exist yet
      (check-false (package-exists-as "my-new-test-pkg")
                   "package should not exist before creation")

      ;; Step 1: GET /create
      (define result1 (tester (make-authenticated-request session-key #:url "/create")
                              #:raw? #t #:headers? #t))
      (check-equal? (result-status result1) 200)
      (define body1 (result-body result1))
      (check-not-false (string-contains? body1 "Package Name"))

      ;; Step 2: Find the save form action and submit a new package
      (define form-actions
        (regexp-match* #rx"action=\"([^\"]+)\"" body1 #:match-select cadr))
      (define save-action (strip-to-path (first form-actions)))
      (define req2
        (make-authenticated-request session-key
                                    #:method #"POST"
                                    #:url save-action
                                    #:bindings (list (binding:form #"name" #"my-new-test-pkg")
                                                     (binding:form #"description" #"A test package")
                                                     (binding:form #"authors" #"creator@example.com")
                                                     (binding:form #"tags" #"test")
                                                     (binding:form #"version__default__type" #"simple")
                                                     (binding:form #"version__default__simple_url" #"https://example.com/pkg.tar.gz")
                                                     (binding:form #"action" #"save_changes"))
                                    #:post-data #"name=my-new-test-pkg&description=A+test+package&authors=creator@example.com&tags=test&version__default__type=simple&version__default__simple_url=https://example.com/pkg.tar.gz&action=save_changes"))
      (define result2 (tester req2 #:raw? #t #:headers? #t))
      ;; Should redirect to the package page on success
      (define status2 (result-status result2))
      (check-not-false (or (and (>= status2 300) (< status2 400))
                           (equal? status2 200))
                       "should redirect or show success")

      ;; Verify: package now exists in backend
      (check-not-false (package-exists-as "my-new-test-pkg")
                       "package should exist in backend after creation")

      ;; Verify: creator is the package author
      (check-not-false (package-author? "my-new-test-pkg" "creator@example.com")
                       "creator should be the package author"))))

(test-case "e2e: update-my-packages rescans user's packages"
  (call-with-logged-in-user "updater@example.com" "pass"
    (lambda (session-key)
      ;; Create a package so this user has something to update
      (parameterize ([current-user "updater@example.com"])
        (save-package! #:old-name ""
                       #:new-name "updater-pkg"
                       #:description "a package"
                       #:source "https://example.com/pkg.tar.gz"
                       #:tags '()
                       #:authors (list "updater@example.com")
                       #:versions '()))

      ;; Verify: user owns the package
      (check-not-false (package-author? "updater-pkg" "updater@example.com")
                       "user should own the package before update")
      (check-not-false (member "updater-pkg" (packages-of "updater@example.com"))
                       "packages-of should list the package")

      ;; Step 1: GET /update-my-packages
      (define result
        (tester (make-authenticated-request session-key #:url "/update-my-packages")
                #:raw? #t #:headers? #t))
      (check-equal? (result-status result) 200)
      (define body (result-body result))
      (check-not-false (string-contains? body "rescanned")
                       "should show packages being rescanned message"))))

;; --- Unauthenticated multi-step flows ---

(test-case "e2e: login page links to register page"
  ;; Step 1: GET /login
  (define result1 (tester "/login" #:raw? #t #:headers? #t))
  (check-equal? (result-status result1) 200)
  (define body1 (result-body result1))
  (check-not-false (string-contains? body1 "Register an account"))

  ;; Step 2: Find and follow the "Register an account" link
  (define register-match
    (regexp-match #rx"href=\"([^\"]+)\"[^>]*>Register an account" body1))
  (check-not-false register-match "should find register link")
  (define register-url (strip-to-path (cadr register-match)))
  (define result2 (tester register-url #:raw? #t #:headers? #t))
  (check-equal? (result-status result2) 200)
  (define body2 (result-body result2))
  (check-not-false (string-contains? body2 "Step 1")
                   "register page should show Step 1")
  (check-not-false (string-contains? body2 "Email me a code")
                   "register page should show email code button"))

(test-case "e2e: register page shows error for missing email"
  ;; Step 1: GET /register-or-reset
  (define result1 (tester "/register-or-reset" #:raw? #t #:headers? #t))
  (check-equal? (result-status result1) 200)
  (define body1 (result-body result1))

  ;; Step 2: Find the "Email me a code" form and submit with empty email
  (define form-actions
    (regexp-match* #rx"action=\"([^\"]+)\"" body1 #:match-select cadr))
  (check-not-false (pair? form-actions) "should find form actions")
  ;; The first form action is the "Email me a code" form
  (define code-form-action (strip-to-path (first form-actions)))
  (define req2
    (request #"POST"
             (string->url code-form-action)
             (list (header #"Content-Type" #"application/x-www-form-urlencoded"))
             (delay (list (binding:form #"email_for_code" #"")
                          (binding:form #"question_answer" #"")
                          (binding:form #"body" #"")))
             #"email_for_code=&question_answer=&body="
             "127.0.0.1" 80 "127.0.0.1"))
  (define result2 (tester req2 #:raw? #t #:headers? #t))
  (check-equal? (result-status result2) 200)
  (define body2 (result-body result2))
  (check-not-false (string-contains? body2 "email")
                   "should show error about email"))

(test-case "e2e: register page shows error for wrong code"
  (call-with-logged-in-user "codeuser@example.com" "pass"
    (lambda (_session-key)
      ;; Step 1: GET /register-or-reset
      (define result1 (tester "/register-or-reset" #:raw? #t #:headers? #t))
      (check-equal? (result-status result1) 200)
      (define body1 (result-body result1))

      ;; Step 2: Find the "Continue" form (step 2 form) and submit with wrong code
      (define form-actions
        (regexp-match* #rx"action=\"([^\"]+)\"" body1 #:match-select cadr))
      (check-true (>= (length form-actions) 2) "should find at least 2 form actions")
      ;; The second form action is the "use code" form
      (define code-form-action (strip-to-path (second form-actions)))
      (define req2
        (request #"POST"
                 (string->url code-form-action)
                 (list (header #"Content-Type" #"application/x-www-form-urlencoded"))
                 (delay (list (binding:form #"email" #"codeuser@example.com")
                              (binding:form #"code" #"wrongcode")
                              (binding:form #"password" #"newpass")
                              (binding:form #"confirm_password" #"newpass")))
                 #"email=codeuser@example.com&code=wrongcode&password=newpass&confirm_password=newpass"
                 "127.0.0.1" 80 "127.0.0.1"))
      (define result2 (tester req2 #:raw? #t #:headers? #t))
      (check-equal? (result-status result2) 200)
      (define body2 (result-body result2))
      (check-not-false (string-contains? body2 "incorrect")
                       "should show error about incorrect code"))))

(test-case "e2e: search page with query renders results"
  ;; Step 1: GET /search (empty)
  (define result1 (tester "/search" #:raw? #t #:headers? #t))
  (check-equal? (result-status result1) 200)
  (define body1 (result-body result1))
  (check-not-false (string-contains? body1 "Search")
                   "search page should render")

  ;; Step 2: GET /search with query parameter
  (define result2 (tester "/search?q=nonexistent-pkg-xyz" #:raw? #t #:headers? #t))
  (check-equal? (result-status result2) 200)
  (define body2 (result-body result2))
  (check-not-false (string-contains? body2 "Search")
                   "search results page should render")
  ;; The search input should have the query pre-filled
  (check-not-false (string-contains? body2 "nonexistent-pkg-xyz")
                   "search query should appear in the page"))

(test-case "e2e: nonexistent package page has navigation back to index"
  ;; Step 1: GET /package/does-not-exist-pkg
  (define result1 (tester "/package/does-not-exist-pkg" #:raw? #t #:headers? #t))
  (check-equal? (result-status result1) 404)
  (define body1 (result-body result1))
  (check-not-false (string-contains? body1 "does not exist")
                   "should say package does not exist")
  (check-not-false (string-contains? body1 "does-not-exist-pkg")
                   "should mention the package name")
  (check-not-false (string-contains? body1 "package index")
                   "should have link to return to package index"))
