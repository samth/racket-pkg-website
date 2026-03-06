#lang racket/base
;; Coverage check script for CI.
;; Runs raco cover with raw output format, parses the results,
;; and fails if any tracked file falls below its coverage threshold.
;;
;; Usage: racket src/tests/check-coverage.rkt

(require racket/list
         racket/string
         racket/runtime-path
         racket/system)

(define-runtime-path project-root "../..")

;; Files to track with minimum coverage thresholds (percentage).
;; Thresholds are set just below current measured coverage so that
;; regressions are caught but normal fluctuations don't cause failures.
(define thresholds
  '(;; Core auth modules (high coverage)
    ("sessions.rkt"        . 95)
    ("users.rkt"           . 80)
    ("pkg-index/api.rkt"   . 90)
    ("pkg-index/common.rkt" . 60)
    ;; Utility modules
    ("hash-utils.rkt"      . 95)
    ("randomness.rkt"      . 95)
    ("xexpr-utils.rkt"     . 90)
    ("spdx.rkt"            . 85)
    ("http-utils.rkt"      . 80)
    ("bootstrap.rkt"       . 80)
    ("html-utils.rkt"      . 75)
    ("config.rkt"          . 75)
    ;; Feature modules (well-tested pure functions)
    ("version.rkt"         . 95)
    ("challenge.rkt"       . 95)
    ("default.rkt"         . 95)
    ("gravatar.rkt"        . 90)
    ("display-name.rkt"    . 90)
    ("package-source.rkt"  . 85)
    ("rpc.rkt"             . 80)
    ;; Feature modules (harder to test in-process)
    ("packages.rkt"        . 40)
    ("github-oauth.rkt"    . 35)
    ("site.rkt"            . 28)
    ;; Backend modules
    ("pkg-index/dynamic.rkt" . 25)
    ;; Side-effectful, requires external services
    ("send-email.rkt"      . 0)))

;; Test files to run
(define test-files
  '("src/tests/test-sessions.rkt"
    "src/tests/test-users.rkt"
    "src/tests/test-auth-api.rkt"
    "src/tests/test-web-handlers.rkt"
    "src/tests/test-github-oauth.rkt"
    "src/tests/test-smoke.rkt"
    "src/tests/test-utils.rkt"))

;; Source files to instrument
(define source-files
  '("src/sessions.rkt"
    "src/users.rkt"
    "src/github-oauth.rkt"
    "src/site.rkt"
    "src/pkg-index/api.rkt"
    "src/pkg-index/common.rkt"
    "src/pkg-index/dynamic.rkt"
    "src/config.rkt"
    "src/default.rkt"
    "src/bootstrap.rkt"
    "src/hash-utils.rkt"
    "src/html-utils.rkt"
    "src/http-utils.rkt"
    "src/packages.rkt"
    "src/randomness.rkt"
    "src/send-email.rkt"
    "src/xexpr-utils.rkt"
    "src/display-name.rkt"
    "src/gravatar.rkt"
    "src/challenge.rkt"
    "src/package-source.rkt"
    "src/spdx.rkt"
    "src/version.rkt"
    "src/rpc.rkt"))

(define cover-dir (build-path (find-system-path 'temp-dir) "cover-ci"))

(define (run-cover!)
  (printf "Running raco cover...\n")
  (define args
    (append (list (find-executable-path "raco") "cover" "-f" "raw" "-d" (path->string cover-dir))
            (map (lambda (f) (path->string (build-path project-root f)))
                 (append test-files source-files))))
  (apply system* args))

(define (parse-coverage)
  (define data (with-input-from-file (build-path cover-dir "coverage.rktl") read))
  (for/hash ([(file entries) (in-hash data)]
             #:when (pair? entries))
    (define covered (count (lambda (e) (car e)) entries))
    (define total (length entries))
    (define suffix (regexp-replace #rx".*/src/" file ""))
    (values suffix (* 100.0 (/ covered total)))))

(define (check-thresholds coverage)
  (define failures '())
  (for ([entry (in-list thresholds)])
    (define file (car entry))
    (define min-pct (cdr entry))
    (define actual (hash-ref coverage file #f))
    (cond
      [(not actual)
       (printf "WARNING: ~a not found in coverage data\n" file)
       (set! failures (cons file failures))]
      [(< actual min-pct)
       (printf "FAIL: ~a coverage ~a% < threshold ~a%\n" file (real->decimal-string actual 1) min-pct)
       (set! failures (cons file failures))]
      [else
       (printf "OK: ~a coverage ~a% >= threshold ~a%\n"
               file
               (real->decimal-string actual 1)
               min-pct)]))
  failures)

(module+ main
  (run-cover!)
  (define coverage (parse-coverage))
  (printf "\n--- Coverage Report ---\n")
  (define failures (check-thresholds coverage))
  (unless (null? failures)
    (printf "\nCoverage check failed for: ~a\n" (string-join failures ", "))
    (exit 1))
  (printf "\nAll coverage checks passed.\n"))
