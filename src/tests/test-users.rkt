#lang racket/base

(require rackunit
         racket/string
         infrastructure-userdb
         "../users.rkt"
         (submod "../users.rkt" for-testing)
         "test-helpers.rkt")

(test-case "register-or-update-user! creates a user"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "newuser@example.com" "password123")
                           (check-true (user-exists? db "newuser@example.com")))))

(test-case "login-password-correct? returns #t for correct password"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "user@example.com" "correct")
                           (check-true (login-password-correct? "user@example.com" "correct")))))

(test-case "login-password-correct? returns #f for wrong password"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "user@example.com" "correct")
                           (check-false (login-password-correct? "user@example.com" "wrong")))))

(test-case "login-password-correct? returns #f for non-existent user"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (check-false (login-password-correct? "nobody@example.com" "anything")))))

(test-case "password change: new password works, old fails"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "user@example.com" "old-pass")
                           (check-true (login-password-correct? "user@example.com" "old-pass"))
                           (register-or-update-user! "user@example.com" "new-pass")
                           (check-false (login-password-correct? "user@example.com" "old-pass"))
                           (check-true (login-password-correct? "user@example.com" "new-pass")))))

(test-case "registration codes: generate and validate"
  (call-with-test-userdb (lambda (db)
                           (define codes (make-registration-state))
                           (initialize-users-for-testing! db codes)
                           (define code (generate-registration-code! codes "reg@example.com"))
                           (check-pred string? code)
                           (check-true (registration-code-correct? "reg@example.com" code)))))

(test-case "registration codes: wrong code fails"
  (call-with-test-userdb (lambda (db)
                           (define codes (make-registration-state))
                           (initialize-users-for-testing! db codes)
                           (generate-registration-code! codes "reg@example.com")
                           (check-false (registration-code-correct? "reg@example.com"
                                                                    "wrong-code")))))

(test-case "registration codes: wrong email fails"
  (call-with-test-userdb (lambda (db)
                           (define codes (make-registration-state))
                           (initialize-users-for-testing! db codes)
                           (define code (generate-registration-code! codes "reg@example.com"))
                           (check-false (registration-code-correct? "other@example.com" code)))))

(test-case "send-registration-or-reset-email! sends reset for existing user"
  (call-with-test-userdb
   (lambda (db)
     (define codes (make-registration-state))
     (initialize-users-for-testing! db codes)
     (register-or-update-user! "existing@example.com" "pass")
     (define sent-subjects '())
     (parameterize ([current-send-email
                     (lambda (from subject to body)
                       (set! sent-subjects (cons subject sent-subjects)))])
       (send-registration-or-reset-email! "existing@example.com"))
     (check-equal? (length sent-subjects) 1)
     (check-not-false (regexp-match #rx"reset" (car sent-subjects))))))

(test-case "ensure-user-id! generates UUID for user without one"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "user@example.com" "pass")
                           (define id (ensure-user-id! "user@example.com"))
                           (check-pred string? id)
                           ;; UUID v4 format: 8-4-4-4-12 hex chars
                           (check-regexp-match
                            #px"^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$"
                            id))))

(test-case "ensure-user-id! is idempotent"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "user@example.com" "pass")
                           (define id1 (ensure-user-id! "user@example.com"))
                           (define id2 (ensure-user-id! "user@example.com"))
                           (check-equal? id1 id2))))

(test-case "ensure-user-id! errors for non-existent user"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (check-exn exn:fail?
                                      (lambda () (ensure-user-id! "nobody@example.com"))))))

(test-case "user-id-for-email returns #f for user without ID"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "user@example.com" "pass")
                           (check-false (user-id-for-email "user@example.com")))))

(test-case "user-id-for-email returns #f for non-existent user"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (check-false (user-id-for-email "nobody@example.com")))))

(test-case "user-id-for-email returns ID after ensure-user-id!"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "user@example.com" "pass")
                           (define id (ensure-user-id! "user@example.com"))
                           (check-equal? (user-id-for-email "user@example.com") id))))

(test-case "send-registration-or-reset-email! sends registration for new user"
  (call-with-test-userdb
   (lambda (db)
     (define codes (make-registration-state))
     (initialize-users-for-testing! db codes)
     (define sent-subjects '())
     (parameterize ([current-send-email
                     (lambda (from subject to body)
                       (set! sent-subjects (cons subject sent-subjects)))])
       (send-registration-or-reset-email! "newuser@example.com"))
     (check-equal? (length sent-subjects) 1)
     (check-not-false (regexp-match #rx"confirmation" (car sent-subjects))))))

;; GitHub identity tests

(test-case "lookup-user-by-github-id returns #f when no users have GitHub IDs"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "user@example.com" "pass")
                           (check-false (lookup-user-by-github-id 12345)))))

(test-case "link-github-account! stores GitHub identity"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "user@example.com" "pass")
                           (link-github-account! "user@example.com" 12345 "ghuser" "user@example.com")
                           (check-equal? (lookup-user-by-github-id 12345) "user@example.com"))))

(test-case "create-github-user! creates user with GitHub identity"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (create-github-user! "ghuser@example.com" 99999 "ghuser" "ghuser@example.com")
                           (check-true (user-exists?/email "ghuser@example.com"))
                           (check-equal? (lookup-user-by-github-id 99999) "ghuser@example.com"))))

(test-case "create-github-user! user cannot login with guessed password"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (create-github-user! "ghuser@example.com" 99999 "ghuser" "ghuser@example.com")
                           (check-false (login-password-correct? "ghuser@example.com" ""))
                           (check-false (login-password-correct? "ghuser@example.com" "password")))))

(test-case "github-username-for-email returns username after linking"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "user@example.com" "pass")
                           (check-false (github-username-for-email "user@example.com"))
                           (link-github-account! "user@example.com" 12345 "ghuser" "user@example.com")
                           (check-equal? (github-username-for-email "user@example.com") "ghuser"))))

(test-case "unlink-github-account! removes GitHub identity"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "user@example.com" "pass")
                           (link-github-account! "user@example.com" 12345 "ghuser" "user@example.com")
                           (check-equal? (lookup-user-by-github-id 12345) "user@example.com")
                           (unlink-github-account! "user@example.com")
                           (check-false (lookup-user-by-github-id 12345))
                           (check-false (github-username-for-email "user@example.com")))))

;; API token tests

(test-case "generate-api-token! returns token with rpkg_ prefix"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "user@example.com" "pass")
                           (define token (generate-api-token! "user@example.com" "test token"))
                           (check-pred string? token)
                           (check-not-false (string-prefix? token "rpkg_")))))

(test-case "validate-api-token finds valid token"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "user@example.com" "pass")
                           (define token (generate-api-token! "user@example.com" "test"))
                           (check-equal? (validate-api-token token) "user@example.com"))))

(test-case "validate-api-token rejects unknown token"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (check-false (validate-api-token "rpkg_fakefakefake")))))

(test-case "revoke-api-token! invalidates token"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "user@example.com" "pass")
                           (define token (generate-api-token! "user@example.com" "to-revoke"))
                           (check-equal? (validate-api-token token) "user@example.com")
                           (define tokens-info (list-api-tokens "user@example.com"))
                           (define hash-prefix (car (car tokens-info)))
                           (check-equal? (revoke-api-token! "user@example.com" hash-prefix) 1)
                           (check-false (validate-api-token token)))))

(test-case "list-api-tokens returns metadata"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "user@example.com" "pass")
                           (generate-api-token! "user@example.com" "my-token")
                           (define tokens (list-api-tokens "user@example.com"))
                           (check-equal? (length tokens) 1)
                           (define tok (car tokens))
                           (check-equal? (length tok) 3)
                           ;; (hash-prefix label created-seconds)
                           (check-pred string? (car tok))     ; hash prefix
                           (check-equal? (cadr tok) "my-token") ; label
                           (check-pred number? (caddr tok))))) ; created

(test-case "multiple tokens for same user"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "user@example.com" "pass")
                           (define t1 (generate-api-token! "user@example.com" "token-1"))
                           (define t2 (generate-api-token! "user@example.com" "token-2"))
                           (check-equal? (validate-api-token t1) "user@example.com")
                           (check-equal? (validate-api-token t2) "user@example.com")
                           (check-equal? (length (list-api-tokens "user@example.com")) 2))))

;; --- Return type tests ---
;; These verify that functions return the correct types, not wrapped
;; in extra lists from the userdb property storage format.

(test-case "ensure-user-id! returns a string, not a list"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "user@example.com" "pass")
                           (define id (ensure-user-id! "user@example.com"))
                           (check-pred string? id "ensure-user-id! first call should return string"))))

(test-case "ensure-user-id! returns a string on second call"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "user@example.com" "pass")
                           (ensure-user-id! "user@example.com")
                           (define id2 (ensure-user-id! "user@example.com"))
                           (check-pred string? id2 "ensure-user-id! second call should return string"))))

(test-case "user-id-for-email returns a string, not a list"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "user@example.com" "pass")
                           (ensure-user-id! "user@example.com")
                           (define id (user-id-for-email "user@example.com"))
                           (check-pred string? id "user-id-for-email should return string"))))

(test-case "user-id-for-email matches ensure-user-id! on second call"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "user@example.com" "pass")
                           (define id1 (ensure-user-id! "user@example.com"))
                           ;; Call ensure again to exercise the "existing" branch
                           (define id2 (ensure-user-id! "user@example.com"))
                           (define id3 (user-id-for-email "user@example.com"))
                           (check-equal? id1 id2)
                           (check-equal? id1 id3)
                           (check-pred string? id3))))

(test-case "github-username-for-email returns a string, not a list"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "user@example.com" "pass")
                           (link-github-account! "user@example.com" 12345 "myghuser" "user@example.com")
                           (define name (github-username-for-email "user@example.com"))
                           (check-pred string? name "github-username-for-email should return string")
                           (check-equal? name "myghuser"))))

(test-case "lookup-user-by-github-id returns a string email, not a list"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "user@example.com" "pass")
                           (link-github-account! "user@example.com" 12345 "ghuser" "user@example.com")
                           (define email (lookup-user-by-github-id 12345))
                           (check-pred string? email "lookup-user-by-github-id should return string")
                           (check-equal? email "user@example.com"))))

(test-case "list-api-tokens returns list of 3-element lists with correct types"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "user@example.com" "pass")
                           (generate-api-token! "user@example.com" "test-label")
                           (define tokens (list-api-tokens "user@example.com"))
                           (check-pred list? tokens)
                           (check-equal? (length tokens) 1)
                           (define tok (car tokens))
                           (check-pred list? tok)
                           (check-equal? (length tok) 3)
                           (check-pred string? (car tok) "hash prefix should be string")
                           (check-pred string? (cadr tok) "label should be string")
                           (check-pred number? (caddr tok) "created should be number"))))

(test-case "list-api-tokens returns empty list for user with no tokens"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "user@example.com" "pass")
                           (check-equal? (list-api-tokens "user@example.com") '()))))

(test-case "list-api-tokens returns empty list for non-existent user"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (check-equal? (list-api-tokens "nobody@example.com") '()))))

(test-case "validate-api-token returns a string email, not a list"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "user@example.com" "pass")
                           (define token (generate-api-token! "user@example.com" "test"))
                           (define email (validate-api-token token))
                           (check-pred string? email "validate-api-token should return string")
                           (check-equal? email "user@example.com"))))

(test-case "user-exists?/email returns boolean"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (check-false (user-exists?/email "nobody@example.com"))
                           (register-or-update-user! "user@example.com" "pass")
                           (check-not-false (user-exists?/email "user@example.com")))))

;; --- Property stability across multiple saves ---
;; The userdb serialization wraps values on each save, so properties
;; must be correctly unwrapped even after other properties are modified.

(test-case "user-id-for-email returns string after generating a token"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "user@example.com" "pass")
                           (ensure-user-id! "user@example.com")
                           ;; Generate a token, which saves the user with api-tokens property
                           (generate-api-token! "user@example.com" "test-token")
                           ;; user-id should still be a string, not wrapped in a list
                           (define id (user-id-for-email "user@example.com"))
                           (check-pred string? id
                                       "user-id-for-email must return string after token generation"))))

(test-case "user-id-for-email returns string after linking GitHub"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "user@example.com" "pass")
                           (ensure-user-id! "user@example.com")
                           (link-github-account! "user@example.com" 12345 "ghuser" "user@example.com")
                           (define id (user-id-for-email "user@example.com"))
                           (check-pred string? id
                                       "user-id-for-email must return string after GitHub link"))))

(test-case "github-username-for-email returns string after generating a token"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "user@example.com" "pass")
                           (link-github-account! "user@example.com" 12345 "ghuser" "user@example.com")
                           (generate-api-token! "user@example.com" "test-token")
                           (define name (github-username-for-email "user@example.com"))
                           (check-pred string? name
                                       "github-username must return string after token generation")
                           (check-equal? name "ghuser"))))

(test-case "user-id-for-email stable after multiple token operations"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "user@example.com" "pass")
                           (define id (ensure-user-id! "user@example.com"))
                           (generate-api-token! "user@example.com" "t1")
                           (generate-api-token! "user@example.com" "t2")
                           (generate-api-token! "user@example.com" "t3")
                           (define id-after (user-id-for-email "user@example.com"))
                           (check-pred string? id-after)
                           (check-equal? id id-after))))

(test-case "ensure-user-id! returns string after password change"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "user@example.com" "pass")
                           (define id (ensure-user-id! "user@example.com"))
                           ;; Password change re-saves the user
                           (register-or-update-user! "user@example.com" "newpass")
                           (define id2 (ensure-user-id! "user@example.com"))
                           (check-pred string? id2)
                           (check-equal? id id2))))
