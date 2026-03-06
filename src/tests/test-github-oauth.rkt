#lang racket/base

(require rackunit
         racket/string
         racket/file
         racket/runtime-path
         "../github-oauth.rkt"
         (submod "../github-oauth.rkt" for-testing))

;; Read PAT from environment variable or local file (never committed).
;; Tests that need a real token skip when neither is available.
(define-runtime-path pat-file "../../samth_pat.txt")
(define github-pat
  (or (getenv "GITHUB_PAT")
      (and (file-exists? pat-file)
           (string-trim (file->string pat-file)))))

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
    (check-not-false (regexp-match #rx"@" (car emails))
                     "first email should contain @"))

  (test-case "real GitHub API: samth account identity"
    (define-values (id username emails) (github-get-user-info github-pat))
    (check-equal? username "samth" "PAT belongs to samth"))

  (test-case "real GitHub API: invalid token returns #f values"
    (define-values (id username emails) (github-get-user-info "ghp_invalid_token_value"))
    (check-false id)
    (check-false username)
    (check-false emails)))
