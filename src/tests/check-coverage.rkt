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

;; Files to track with minimum coverage thresholds (percentage)
(define thresholds
  '(("sessions.rkt" . 95) ("users.rkt" . 80)
                          ("pkg-index/common.rkt" . 60)
                          ("pkg-index/api.rkt" . 90)))

;; Test files to run
(define test-files
  '("src/tests/test-sessions.rkt" "src/tests/test-users.rkt"
                                  "src/tests/test-auth-api.rkt"
                                  "src/tests/test-web-handlers.rkt"))

;; Source files to instrument
(define source-files
  '("src/sessions.rkt" "src/users.rkt" "src/pkg-index/common.rkt" "src/pkg-index/api.rkt"))

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
