#lang racket/base

(require rackunit
         web-server/http/cookie-parse
         "../sessions.rkt"
         (submod "../sessions.rkt" for-testing)
         "test-helpers.rkt")

(test-case "create-session! returns a string key"
  (define key (create-session! "test@example.com"))
  (check-pred string? key)
  (check-true (> (string-length key) 0)))

(test-case "lookup-session/touch! returns session for valid key"
  (define key (create-session! "test@example.com"))
  (define s (lookup-session/touch! key))
  (check-pred session? s)
  (check-equal? (session-email s) "test@example.com"))

(test-case "lookup-session/touch! returns #f for unknown key"
  (check-false (lookup-session/touch! "nonexistent-key-12345")))

(test-case "destroy-session! makes key return #f"
  (define key (create-session! "test@example.com"))
  (check-pred session? (lookup-session/touch! key))
  (destroy-session! key)
  (check-false (lookup-session/touch! key)))

(test-case "session expiry via expire-sessions!"
  ;; Create a session, then replace it in the store with an expired copy
  (define key (create-session! "test@example.com"))
  (define s (lookup-session key))
  (check-pred session? s)
  ;; Prefab structs allow struct-copy; set expiry to the past
  (define expired (struct-copy session s [expiry 0]))
  ;; Overwrite the live session with the expired one directly in the store
  (hash-set! (sessions) key expired)
  (check-pred session? (lookup-session key)) ; still there before expiry sweep
  ;; Now run expiry - should remove the expired session
  (expire-sessions!)
  (check-false (lookup-session key)))

(test-case "current-email returns session email when parameterized"
  (define key (create-session! "user@example.com"))
  (define s (lookup-session/touch! key))
  (check-false (current-email))
  (parameterize ([current-session s])
    (check-equal? (current-email) "user@example.com")))

(test-case "current-email returns #f when no session"
  (parameterize ([current-session #f])
    (check-false (current-email))))

(test-case "session stores curator? and superuser? flags"
  (define key (create-session! "admin@example.com" #:curator? #t #:superuser? #t))
  (define s (lookup-session/touch! key))
  (check-true (session-curator? s))
  (check-true (session-superuser? s))
  (define key2 (create-session! "user@example.com"))
  (define s2 (lookup-session/touch! key2))
  (check-false (session-curator? s2))
  (check-false (session-superuser? s2)))

(test-case "cookie round-trip: extract session key from request cookies"
  (define key (create-session! "cookie@example.com"))
  (define req (make-test-request #:cookies (list (cons "pltsession" key))))
  ;; Extract the session key from the request the same way site.rkt does
  (define session-cookies
    (filter (lambda (c) (equal? (client-cookie-name c) "pltsession")) (request-cookies req)))
  (check-equal? (length session-cookies) 1)
  (define extracted-key (client-cookie-value (car session-cookies)))
  (check-equal? extracted-key key)
  (check-pred session? (lookup-session/touch! extracted-key)))

(test-case "with-test-session helper works"
  (with-test-session "helper@example.com"
                     (lambda ()
                       (check-equal? (current-email) "helper@example.com")
                       (check-pred session? (current-session)))))
