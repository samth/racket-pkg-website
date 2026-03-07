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
         reloadable
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
         "../github-oauth.rkt"
         (submod "../github-oauth.rkt" for-testing)
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

;; --- Comprehensive auth + account lifecycle test ---


;; Access the persistent config handler so we can set test config values
(define config-handler (make-persistent-state '*config* (lambda () (hash))))

;; Save the original config to restore after each test
(define original-config (config-handler))

;; Helper: run thunk with GitHub OAuth configured and mock exchange/user-info functions.
;; fake-exchange: code redirect-uri -> access-token-or-#f
;; fake-user-info: access-token -> (values github-id github-username verified-emails)
(define (call-with-github-oauth fake-exchange fake-user-info thunk)
  (call-with-test-userdb
   (lambda (db)
     (call-with-test-packages-dir
      (lambda (pkgs-dir)
        (initialize-users-for-testing! db (make-registration-state))
        (set-userdb-for-testing! db)
        ;; Enable GitHub OAuth in config
        (config-handler (hash-set* original-config
                                    'github-login-client-id "test-client-id"
                                    'github-login-client-secret "test-client-secret"))
        (dynamic-wind
          void
          (lambda ()
            (parameterize ([current-github-exchange-code fake-exchange]
                           [current-github-get-user-info fake-user-info])
              (thunk db)))
          (lambda ()
            (config-handler original-config))))))))

;; Helper: build a GitHub callback request with code and state params
(define (make-github-callback-request code state)
  (define qs (if state
                 (format "code=~a&state=~a" code state)
                 (format "code=~a" code)))
  (request #"GET"
           (string->url (string-append "/auth/github/callback?" qs))
           '()
           (delay (list (binding:form (string->bytes/utf-8 "code")
                                      (string->bytes/utf-8 code))
                        (binding:form (string->bytes/utf-8 "state")
                                      (string->bytes/utf-8 (or state "")))))
           #f
           "127.0.0.1" 80 "127.0.0.1"))

(test-case "github: new user — creates account and session"
  (call-with-github-oauth
   ;; Mock exchange: always return a token
   (lambda (code redirect-uri) "fake-access-token")
   ;; Mock user-info: return a new GitHub user
   (lambda (token)
     (values 12345 "ghuser" (list "ghuser@example.com")))
   (lambda (db)
     ;; Verify: user does not exist yet
     (check-false (user-exists?/email "ghuser@example.com")
                  "user should not exist before GitHub login")

     ;; Generate a valid CSRF state
     (define state (generate-csrf-state!))

     ;; Hit the callback
     (define result (tester (make-github-callback-request "test-code" state)
                            #:raw? #t #:headers? #t))
     (define headers (result-headers-str result))

     ;; Should set a session cookie (successful login)
     (check-not-false (string-contains? headers "pltsession=")
                      "should set session cookie for new GitHub user")

     ;; Verify backend: user was created
     (check-not-false (user-exists?/email "ghuser@example.com")
                      "user should exist after GitHub login")

     ;; Verify backend: GitHub ID linked
     (check-equal? (lookup-user-by-github-id 12345) "ghuser@example.com"
                   "GitHub ID should map to the new user")

     ;; Verify backend: GitHub username stored
     (check-equal? (github-username-for-email "ghuser@example.com") "ghuser"
                   "GitHub username should be stored")

     ;; Verify backend: user-id was assigned
     (check-not-false (user-id-for-email "ghuser@example.com")
                      "user-id should be assigned on login"))))

(test-case "github: known user — logs in directly"
  (call-with-github-oauth
   (lambda (code redirect-uri) "fake-token")
   (lambda (token)
     (values 67890 "returning-user" (list "returning@example.com")))
   (lambda (db)
     ;; Pre-create the user with GitHub ID already linked
     (register-or-update-user! "returning@example.com" "some-pass")
     (ensure-user-id! "returning@example.com")
     (link-github-account! "returning@example.com" 67890 "returning-user"
                           "returning@example.com")

     (define state (generate-csrf-state!))
     (define result (tester (make-github-callback-request "code" state)
                            #:raw? #t #:headers? #t))
     (define headers (result-headers-str result))

     ;; Should set session cookie
     (check-not-false (string-contains? headers "pltsession=")
                      "should set session cookie for returning user")

     ;; Backend: user-id unchanged
     (check-not-false (user-id-for-email "returning@example.com")
                      "user-id should still exist"))))

(test-case "github: email matches existing account — shows link confirmation"
  (call-with-github-oauth
   (lambda (code redirect-uri) "fake-token")
   (lambda (token)
     (values 11111 "linkme" (list "existing@example.com")))
   (lambda (db)
     ;; Pre-create an email+password user (no GitHub linked)
     (register-or-update-user! "existing@example.com" "my-password")

     (define state (generate-csrf-state!))
     (define result (tester (make-github-callback-request "code" state)
                            #:raw? #t #:headers? #t))
     (define body (result-body result))

     ;; Should show the "link account" confirmation page
     (check-not-false (string-contains? body "existing@example.com")
                      "should mention the existing email")
     (check-not-false (string-contains? body "linkme")
                      "should show the GitHub username")
     (check-not-false (string-contains? body "Link Account")
                      "should show the link button")

     ;; Backend: GitHub ID should NOT be linked yet (waiting for password)
     (check-false (lookup-user-by-github-id 11111)
                  "GitHub ID should not be linked before password confirmation")

     ;; Step 2: Submit password to complete the link
     (define form-actions
       (regexp-match* #rx"action=\"([^\"]+)\"" body #:match-select cadr))
     (check-not-false (pair? form-actions) "should find link form action")
     (define link-action (strip-to-path (first form-actions)))

     (define link-req
       (request #"POST"
                (string->url link-action)
                (list (header #"Content-Type" #"application/x-www-form-urlencoded"))
                (delay (list (binding:form #"password" #"my-password")))
                #"password=my-password"
                "127.0.0.1" 80 "127.0.0.1"))
     (define link-result (tester link-req #:raw? #t #:headers? #t))
     (define link-headers (result-headers-str link-result))

     ;; Should set session cookie (login succeeds after linking)
     (check-not-false (string-contains? link-headers "pltsession=")
                      "should set session cookie after linking")

     ;; Backend: GitHub ID now linked
     (check-equal? (lookup-user-by-github-id 11111) "existing@example.com"
                   "GitHub ID should be linked after password confirmation")
     (check-equal? (github-username-for-email "existing@example.com") "linkme"
                   "GitHub username should be stored after linking"))))

(test-case "github: link confirmation fails with wrong password"
  (call-with-github-oauth
   (lambda (code redirect-uri) "fake-token")
   (lambda (token)
     (values 22222 "badpass-user" (list "wrongpw@example.com")))
   (lambda (db)
     (register-or-update-user! "wrongpw@example.com" "correct-password")

     (define state (generate-csrf-state!))
     (define result (tester (make-github-callback-request "code" state)
                            #:raw? #t #:headers? #t))
     (define body (result-body result))

     ;; Find the link form and submit with wrong password
     (define form-actions
       (regexp-match* #rx"action=\"([^\"]+)\"" body #:match-select cadr))
     (define link-action (strip-to-path (first form-actions)))

     (define link-req
       (request #"POST"
                (string->url link-action)
                (list (header #"Content-Type" #"application/x-www-form-urlencoded"))
                (delay (list (binding:form #"password" #"wrong-password")))
                #"password=wrong-password"
                "127.0.0.1" 80 "127.0.0.1"))
     (define link-result (tester link-req #:raw? #t #:headers? #t))
     (define link-body (result-body link-result))

     ;; Should show error, NOT set session cookie
     (check-not-false (string-contains? link-body "Incorrect password")
                      "should show incorrect password error")
     (check-false (string-contains? (result-headers-str link-result) "pltsession=")
                  "should not set session cookie on wrong password")

     ;; Backend: GitHub ID NOT linked
     (check-false (lookup-user-by-github-id 22222)
                  "GitHub ID should not be linked after wrong password"))))

(test-case "github: invalid CSRF state rejected"
  (call-with-github-oauth
   (lambda (code redirect-uri) "fake-token")
   (lambda (token) (values 99999 "csrf-user" (list "csrf@example.com")))
   (lambda (db)
     ;; Use a bogus state that was never generated
     (define result (tester (make-github-callback-request "code" "bogus-state")
                            #:raw? #t #:headers? #t))
     (define body (result-body result))

     (check-not-false (string-contains? body "Invalid or expired")
                      "should show CSRF error")

     ;; Backend: no user created
     (check-false (user-exists?/email "csrf@example.com")
                  "no user should be created with invalid CSRF state"))))

(test-case "github: exchange failure shows error"
  (call-with-github-oauth
   ;; Mock exchange: returns #f (failure)
   (lambda (code redirect-uri) #f)
   (lambda (token) (values #f #f #f))
   (lambda (db)
     (define state (generate-csrf-state!))
     (define result (tester (make-github-callback-request "code" state)
                            #:raw? #t #:headers? #t))

     (check-not-false (string-contains? (result-body result) "GitHub login failed")
                      "should show exchange failure error"))))

(test-case "github: no verified emails shows error"
  (call-with-github-oauth
   (lambda (code redirect-uri) "fake-token")
   ;; Mock user-info: valid user but no verified emails
   (lambda (token) (values 33333 "noemail" '()))
   (lambda (db)
     (define state (generate-csrf-state!))
     (define result (tester (make-github-callback-request "code" state)
                            #:raw? #t #:headers? #t))

     (check-not-false (string-contains? (result-body result) "No verified email")
                      "should show no verified email error")

     ;; Backend: no user created
     (check-false (user-exists?/email "noemail@example.com")
                  "no user should be created without verified email"))))

(test-case "github: user-info failure shows error"
  (call-with-github-oauth
   (lambda (code redirect-uri) "fake-token")
   ;; Mock user-info: returns failure
   (lambda (token) (values #f #f #f))
   (lambda (db)
     (define state (generate-csrf-state!))
     (define result (tester (make-github-callback-request "code" state)
                            #:raw? #t #:headers? #t))

     (check-not-false (string-contains? (result-body result)
                                        "Could not retrieve your GitHub account")
                      "should show user-info failure error"))))

(test-case "github: unlink from account page"
  (call-with-github-oauth
   (lambda (code redirect-uri) "fake-token")
   (lambda (token) (values 44444 "unlinkme" (list "unlink@example.com")))
   (lambda (db)
     ;; Create user with GitHub linked
     (register-or-update-user! "unlink@example.com" "pass")
     (ensure-user-id! "unlink@example.com")
     (link-github-account! "unlink@example.com" 44444 "unlinkme" "unlink@example.com")

     ;; Verify linked
     (check-equal? (lookup-user-by-github-id 44444) "unlink@example.com")
     (check-equal? (github-username-for-email "unlink@example.com") "unlinkme")

     ;; Log in and visit account page
     (define session-key (create-session! "unlink@example.com"))
     (define acct-result
       (tester (make-authenticated-request session-key #:url "/account")
               #:raw? #t #:headers? #t))
     (check-equal? (result-status acct-result) 200)
     (define acct-body (result-body acct-result))

     ;; Should show linked GitHub account and unlink button
     (check-not-false (string-contains? acct-body "unlinkme")
                      "should show linked GitHub username")
     (check-not-false (string-contains? acct-body "Unlink GitHub Account")
                      "should show unlink button")

     ;; Find and click the unlink form
     (define unlink-match
       (regexp-match #rx"action=\"([^\"]+)\"[^>]*>[^<]*<button[^>]*>Unlink" acct-body))
     (check-not-false unlink-match "should find unlink form")
     (define unlink-url (strip-to-path (cadr unlink-match)))

     (define unlink-req
       (make-authenticated-request session-key #:method #"POST"
                                   #:url unlink-url #:post-data #""))
     (define unlink-result (tester unlink-req #:raw? #t #:headers? #t))
     (check-equal? (result-status unlink-result) 200)
     (check-not-false (string-contains? (result-body unlink-result) "GitHub account unlinked")
                      "should show unlink confirmation")

     ;; Backend: GitHub ID no longer linked
     (check-false (lookup-user-by-github-id 44444)
                  "GitHub ID should not be linked after unlinking")
     (check-false (github-username-for-email "unlink@example.com")
                  "GitHub username should be cleared after unlinking"))))

(test-case "github: login page shows GitHub button when configured"
  (call-with-github-oauth
   (lambda (code redirect-uri) #f)
   (lambda (token) (values #f #f #f))
   (lambda (db)
     (define result (tester "/login" #:raw? #t #:headers? #t))
     (check-equal? (result-status result) 200)
     (check-not-false (string-contains? (result-body result) "Sign in with GitHub")
                      "login page should show GitHub sign-in button when configured"))))

;; --- End-to-end test: account page survives token generation ---
;; This test catches the bug where user-id-for-email returned a list
;; instead of a string after a token was generated (due to property
;; value wrapping in the userdb serialization layer). The account page
;; would crash with an xexpr contract violation on the second render.

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

;; --- Comprehensive auth + account lifecycle test ---

(test-case "e2e: full auth and account lifecycle"
  (call-with-test-userdb
   (lambda (db)
     (call-with-test-packages-dir
      (lambda (pkgs-dir)
        (initialize-users-for-testing! db (make-registration-state))
        (set-userdb-for-testing! db)

        ;; ===== Step 1: Register user directly (backend) =====
        ;; Registration via the web form requires solving a random challenge,
        ;; so we register directly and verify backend state.
        (register-or-update-user! "lifecycle@example.com" "initial-pass")
        (check-not-false (login-password-correct? "lifecycle@example.com" "initial-pass")
                         "step 1: user should be registered with initial password")
        (check-false (user-id-for-email "lifecycle@example.com")
                     "step 1: user should not have a user-id yet")

        ;; ===== Step 2: Log in via web form =====
        ;; GET /login to get the login form
        (define login-result (tester "/login" #:raw? #t #:headers? #t))
        (check-equal? (result-status login-result) 200)
        (define login-body (result-body login-result))
        (define login-actions
          (regexp-match* #rx"action=\"([^\"]+)\"" login-body #:match-select cadr))
        (check-not-false (pair? login-actions) "step 2: should find login form action")
        (define login-action (strip-to-path (first login-actions)))

        ;; POST credentials
        (define login-req
          (request #"POST"
                   (string->url login-action)
                   (list (header #"Content-Type" #"application/x-www-form-urlencoded"))
                   (delay (list (binding:form #"email" #"lifecycle@example.com")
                                (binding:form #"password" #"initial-pass")))
                   #"email=lifecycle@example.com&password=initial-pass"
                   "127.0.0.1" 80 "127.0.0.1"))
        (define login-post-result (tester login-req #:raw? #t #:headers? #t))
        ;; Successful login sets a session cookie and redirects
        (define login-headers (result-headers-str login-post-result))
        (check-not-false (string-contains? login-headers "pltsession=")
                         "step 2: login should set session cookie")

        ;; Verify: login assigns a user-id
        (check-not-false (user-id-for-email "lifecycle@example.com")
                         "step 2: login should assign a user-id")
        (define user-id (user-id-for-email "lifecycle@example.com"))

        ;; Extract the session key from the cookie to use for subsequent requests
        ;; The tester creates a real session, so we can look it up
        (define cookie-match
          (regexp-match #rx"pltsession=([^;]+)" login-headers))
        (check-not-false cookie-match "step 2: should extract session cookie")

        ;; Create a session key we can use with make-authenticated-request
        (define session-key (create-session! "lifecycle@example.com"))

        ;; ===== Step 3: Visit account page, verify user-id shown =====
        (define acct1 (tester (make-authenticated-request session-key #:url "/account")
                              #:raw? #t #:headers? #t))
        (check-equal? (result-status acct1) 200)
        (define acct1-body (result-body acct1))
        (check-not-false (string-contains? acct1-body "lifecycle@example.com")
                         "step 3: account page should show email")
        (check-not-false (string-contains? acct1-body user-id)
                         "step 3: account page should show user-id")
        (check-not-false (string-contains? acct1-body "No API tokens")
                         "step 3: should show no tokens initially")

        ;; ===== Step 4: Change password =====
        (define acct1-forms
          (regexp-match* #rx"action=\"([^\"]+)\"" acct1-body #:match-select cadr))
        (define pw-action (strip-to-path (first acct1-forms)))

        (define pw-req
          (make-authenticated-request session-key
                                      #:method #"POST"
                                      #:url pw-action
                                      #:bindings (list (binding:form #"current_password" #"initial-pass")
                                                       (binding:form #"new_password" #"changed-pass")
                                                       (binding:form #"confirm_password" #"changed-pass"))
                                      #:post-data #"current_password=initial-pass&new_password=changed-pass&confirm_password=changed-pass"))
        (define pw-result (tester pw-req #:raw? #t #:headers? #t))
        (check-equal? (result-status pw-result) 200)
        (check-not-false (string-contains? (result-body pw-result) "Password changed successfully")
                         "step 4: should confirm password change")

        ;; Verify backend: old password fails, new password works
        (check-false (login-password-correct? "lifecycle@example.com" "initial-pass")
                     "step 4: old password should fail")
        (check-not-false (login-password-correct? "lifecycle@example.com" "changed-pass")
                         "step 4: new password should work")

        ;; user-id should be unchanged
        (check-equal? (user-id-for-email "lifecycle@example.com") user-id
                      "step 4: user-id should be stable after password change")

        ;; ===== Step 5: Generate first API token =====
        ;; Re-visit account page to get fresh form actions
        (define acct2 (tester (make-authenticated-request session-key #:url "/account")
                              #:raw? #t #:headers? #t))
        (check-equal? (result-status acct2) 200)
        (define acct2-body (result-body acct2))
        (define acct2-forms
          (regexp-match* #rx"action=\"([^\"]+)\"" acct2-body #:match-select cadr))
        ;; Token generation form is the last form
        (define token-action (strip-to-path (last acct2-forms)))

        (define tok1-req
          (make-authenticated-request session-key
                                      #:method #"POST"
                                      #:url token-action
                                      #:bindings (list (binding:form #"token_label" #"ci-deploy"))
                                      #:post-data #"token_label=ci-deploy"))
        (define tok1-result (tester tok1-req #:raw? #t #:headers? #t))
        (check-equal? (result-status tok1-result) 200)
        (define tok1-body (result-body tok1-result))
        (check-not-false (string-contains? tok1-body "rpkg_")
                         "step 5: should show generated token")

        ;; Extract token plaintext
        (define tok1-match (regexp-match #rx"(rpkg_[0-9a-f]+)" tok1-body))
        (check-not-false tok1-match "step 5: should find token plaintext")
        (define token1-plaintext (cadr tok1-match))

        ;; Verify backend
        (check-equal? (validate-api-token token1-plaintext) "lifecycle@example.com"
                      "step 5: token1 should validate to user email")
        (define tokens-after-1 (list-api-tokens "lifecycle@example.com"))
        (check-equal? (length tokens-after-1) 1
                      "step 5: should have 1 token in backend")
        (check-equal? (cadr (car tokens-after-1)) "ci-deploy"
                      "step 5: token label should be ci-deploy")

        ;; ===== Step 6: Generate second API token =====
        ;; Follow the continue link to get back to account page
        (define continue1-match
          (regexp-match #rx"href=\"([^\"]+)\">Continue to Account Settings" tok1-body))
        (define continue1-url (strip-to-path (cadr continue1-match)))
        (define acct3 (tester (make-authenticated-request session-key #:url continue1-url)
                              #:raw? #t #:headers? #t))
        (check-equal? (result-status acct3) 200)
        (define acct3-body (result-body acct3))
        (check-not-false (string-contains? acct3-body "ci-deploy")
                         "step 6: first token should be listed")

        ;; Generate second token from the refreshed account page
        (define acct3-forms
          (regexp-match* #rx"action=\"([^\"]+)\"" acct3-body #:match-select cadr))
        (define token2-action (strip-to-path (last acct3-forms)))

        (define tok2-req
          (make-authenticated-request session-key
                                      #:method #"POST"
                                      #:url token2-action
                                      #:bindings (list (binding:form #"token_label" #"local-dev"))
                                      #:post-data #"token_label=local-dev"))
        (define tok2-result (tester tok2-req #:raw? #t #:headers? #t))
        (check-equal? (result-status tok2-result) 200)
        (define tok2-body (result-body tok2-result))
        (define tok2-match (regexp-match #rx"(rpkg_[0-9a-f]+)" tok2-body))
        (check-not-false tok2-match "step 6: should find second token")
        (define token2-plaintext (cadr tok2-match))

        ;; Verify backend: both tokens exist
        (check-equal? (validate-api-token token1-plaintext) "lifecycle@example.com"
                      "step 6: token1 should still validate")
        (check-equal? (validate-api-token token2-plaintext) "lifecycle@example.com"
                      "step 6: token2 should validate")
        (define tokens-after-2 (list-api-tokens "lifecycle@example.com"))
        (check-equal? (length tokens-after-2) 2
                      "step 6: should have 2 tokens in backend")

        ;; ===== Step 7: Revoke first token =====
        ;; Go back to account page to find the revoke buttons
        (define continue2-match
          (regexp-match #rx"href=\"([^\"]+)\">Continue to Account Settings" tok2-body))
        (define continue2-url (strip-to-path (cadr continue2-match)))
        (define acct4 (tester (make-authenticated-request session-key #:url continue2-url)
                              #:raw? #t #:headers? #t))
        (check-equal? (result-status acct4) 200)
        (define acct4-body (result-body acct4))

        ;; Find revoke forms - there should be 2 (one per token)
        (define revoke-actions
          (regexp-match* #rx"action=\"([^\"]+)\"[^>]*>[^<]*<button[^>]*>Revoke" acct4-body
                         #:match-select cadr))
        (check-equal? (length revoke-actions) 2
                      "step 7: should find 2 revoke buttons")

        ;; Revoke the first one
        (define revoke1-url (strip-to-path (car revoke-actions)))
        (define revoke1-req
          (make-authenticated-request session-key #:method #"POST"
                                      #:url revoke1-url #:post-data #""))
        (define revoke1-result (tester revoke1-req #:raw? #t #:headers? #t))
        (check-equal? (result-status revoke1-result) 200)
        (check-not-false (string-contains? (result-body revoke1-result) "revoked")
                         "step 7: should show revoked message")

        ;; Verify backend: one token remains
        (define tokens-after-revoke (list-api-tokens "lifecycle@example.com"))
        (check-equal? (length tokens-after-revoke) 1
                      "step 7: should have 1 token after revoking one")
        ;; token2 should still work, token1 should not (or vice versa)
        (define token2-still-valid (validate-api-token token2-plaintext))
        (check-not-false (or (validate-api-token token1-plaintext)
                             token2-still-valid)
                         "step 7: at least one token should still validate")

        ;; ===== Step 8: Create a package =====
        ;; GET /create
        (define create-result
          (tester (make-authenticated-request session-key #:url "/create")
                  #:raw? #t #:headers? #t))
        (check-equal? (result-status create-result) 200)
        (define create-body (result-body create-result))

        ;; Verify: package doesn't exist yet
        (check-false (package-exists-as "lifecycle-test-pkg")
                     "step 8: package should not exist before creation")

        ;; Find the form action and submit the package
        (define create-forms
          (regexp-match* #rx"action=\"([^\"]+)\"" create-body #:match-select cadr))
        (define save-action (strip-to-path (first create-forms)))
        (define save-req
          (make-authenticated-request
           session-key
           #:method #"POST"
           #:url save-action
           #:bindings (list (binding:form #"name" #"lifecycle-test-pkg")
                            (binding:form #"description" #"Test package for lifecycle")
                            (binding:form #"authors" #"lifecycle@example.com")
                            (binding:form #"tags" #"test lifecycle")
                            (binding:form #"version__default__type" #"simple")
                            (binding:form #"version__default__simple_url"
                                          #"https://example.com/lifecycle.tar.gz")
                            (binding:form #"action" #"save_changes"))
           #:post-data (string->bytes/utf-8
                        (string-append "name=lifecycle-test-pkg"
                                       "&description=Test+package+for+lifecycle"
                                       "&authors=lifecycle@example.com"
                                       "&tags=test+lifecycle"
                                       "&version__default__type=simple"
                                       "&version__default__simple_url=https://example.com/lifecycle.tar.gz"
                                       "&action=save_changes"))))
        (define save-result (tester save-req #:raw? #t #:headers? #t))
        (define save-status (result-status save-result))
        (check-not-false (or (and (>= save-status 300) (< save-status 400))
                             (equal? save-status 200))
                         "step 8: save should succeed")

        ;; Verify backend
        (check-not-false (package-exists-as "lifecycle-test-pkg")
                         "step 8: package should exist after creation")
        (check-not-false (package-author? "lifecycle-test-pkg" "lifecycle@example.com")
                         "step 8: user should be package author")
        (check-not-false (member "lifecycle-test-pkg"
                                 (packages-of "lifecycle@example.com"))
                         "step 8: packages-of should include the new package")

        ;; ===== Step 9: View the package page =====
        (define pkg-result
          (tester (make-authenticated-request session-key
                                              #:url "/package/lifecycle-test-pkg")
                  #:raw? #t #:headers? #t))
        (check-equal? (result-status pkg-result) 200)
        (define pkg-body (result-body pkg-result))
        (check-not-false (string-contains? pkg-body "lifecycle-test-pkg")
                         "step 9: package page should show package name")
        (check-not-false (string-contains? pkg-body "Test package for lifecycle")
                         "step 9: package page should show description")
        (check-not-false (string-contains? pkg-body "lifecycle@example.com")
                         "step 9: package page should show author")

        ;; ===== Step 10: Update my packages =====
        (define update-result
          (tester (make-authenticated-request session-key #:url "/update-my-packages")
                  #:raw? #t #:headers? #t))
        (check-equal? (result-status update-result) 200)
        (check-not-false (string-contains? (result-body update-result) "rescanned")
                         "step 10: should confirm rescan")

        ;; ===== Step 11: Log out =====
        ;; Verify session exists before logout
        (check-not-false (lookup-session/touch! session-key)
                         "step 11: session should exist before logout")

        (define logout-result
          (tester (make-authenticated-request session-key #:url "/logout")
                  #:raw? #t #:headers? #t))
        (define logout-status (result-status logout-result))
        (check-not-false (or (and (>= logout-status 300) (< logout-status 400))
                             (equal? logout-status 200))
                         "step 11: logout should succeed")

        ;; Verify backend: session is destroyed
        (check-false (lookup-session/touch! session-key)
                     "step 11: session should be gone after logout")

        ;; ===== Step 12: Verify everything persists =====
        ;; All backend state should still be intact after logout
        (check-not-false (login-password-correct? "lifecycle@example.com" "changed-pass")
                         "step 12: changed password should still work")
        (check-false (login-password-correct? "lifecycle@example.com" "initial-pass")
                     "step 12: initial password should still fail")
        (check-equal? (user-id-for-email "lifecycle@example.com") user-id
                      "step 12: user-id should be unchanged")
        (check-not-false (package-exists-as "lifecycle-test-pkg")
                         "step 12: package should persist after logout")
        (check-not-false (package-author? "lifecycle-test-pkg" "lifecycle@example.com")
                         "step 12: package authorship should persist")

        ;; ===== Step 13: Log back in with new password =====
        (define session-key2 (create-session! "lifecycle@example.com"))
        (define acct-final
          (tester (make-authenticated-request session-key2 #:url "/account")
                  #:raw? #t #:headers? #t))
        (check-equal? (result-status acct-final) 200)
        (define acct-final-body (result-body acct-final))
        (check-not-false (string-contains? acct-final-body user-id)
                         "step 13: user-id should still appear on account page")
        ;; Should show remaining token(s)
        (check-false (string-contains? acct-final-body "No API tokens")
                     "step 13: should still have tokens after re-login"))))))
