#lang racket/base

(require rackunit
         racket/string
         "../github-oauth.rkt"
         (submod "../github-oauth.rkt" for-testing))

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
