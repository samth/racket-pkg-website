#lang racket/base

;; HTTP smoke tests against a running server instance.
;; Starts the full server on a random port with test configuration,
;; makes real HTTP requests, and verifies responses.
;; All tests share a single server instance for efficiency.

(require rackunit
         racket/port
         racket/file
         racket/string
         racket/tcp
         racket/runtime-path
         net/http-easy
         infrastructure-userdb)

(define-runtime-path project-root "../..")
(define-runtime-path main-rkt "../main.rkt")

;; Find a free TCP port
(define (find-free-port)
  (define listener (tcp-listen 0 5 #t "127.0.0.1"))
  (define-values (_local-ip local-port _remote-ip _remote-port) (tcp-addresses listener #t))
  (tcp-close listener)
  local-port)

;; Wait for the server to start accepting connections
(define (wait-for-server host port #:timeout [timeout 30])
  (let loop ([elapsed 0])
    (cond
      [(>= elapsed timeout) (error 'wait-for-server "timed out waiting for ~a:~a" host port)]
      [(with-handlers ([exn:fail? (lambda (_) #f)])
         (define-values (in out) (tcp-connect host port))
         (close-input-port in)
         (close-output-port out)
         #t)
       #t]
      [else
       (sleep 0.5)
       (loop (+ elapsed 0.5))])))

;; Resolve the racket executable from the repo root symlink
(define racket-exe
  (let ([link (build-path project-root ".racket")])
    (if (link-exists? link)
        (path->string (resolve-path link))
        (path->string (find-executable-path "racket")))))

;; Set up test infrastructure: temp dirs, config, userdb, server process
(define test-port (find-free-port))
(define backend-port (find-free-port))
(define tmp-root (make-temporary-directory))
(define tmp-users (build-path tmp-root "users.new"))
(make-directory* tmp-users)

;; Create a test user
(define db (userdb-config (path->string tmp-users) #t))
(save-user! db (user-password-set (make-user "test@example.com" "testpass") "testpass"))

;; Write the config module
(define config-file (build-path tmp-root "test-config.rkt"))
(display-to-file (format (string-append "#lang racket/base\n"
                                        "(provide config)\n"
                                        "(define config\n"
                                        "  (hash 'port ~a\n"
                                        "        'pkg-index-port ~a\n"
                                        "        'ssl? #f\n"
                                        "        'root ~s\n"
                                        "        'user-directory ~s\n"
                                        "        'pkg-index (hash)))\n")
                         test-port
                         backend-port
                         (path->string tmp-root)
                         (path->string tmp-users))
                 config-file)

;; Start the server subprocess (suppress stdout/stderr noise)
(define dev-null-out (open-output-file "/dev/null" #:exists 'append))
(define dev-null-err (open-output-file "/dev/null" #:exists 'append))
(define-values (proc proc-stdout proc-stdin proc-stderr)
  (subprocess dev-null-out
              #f
              dev-null-err
              racket-exe
              "-y"
              (path->string main-rkt)
              "--config"
              (path->string config-file)))

;; Clean up on exit
(define (cleanup!)
  (subprocess-kill proc #t)
  (subprocess-wait proc)
  (when proc-stdout
    (close-input-port proc-stdout))
  (when proc-stdin
    (close-output-port proc-stdin))
  (close-output-port dev-null-out)
  (close-output-port dev-null-err)
  (delete-directory/files tmp-root #:must-exist? #f))

(void (plumber-add-flush! (current-plumber) (lambda (_) (cleanup!))))

;; Wait for server to be ready
(void (wait-for-server "127.0.0.1" test-port))

(define (test-url path)
  (format "http://127.0.0.1:~a~a" test-port path))

;; --- Tests ---

(test-case "smoke: GET / returns 200"
  (define resp (get (test-url "/") #:timeouts (make-timeout-config #:request 10)))
  (check-equal? (response-status-code resp) 200)
  (response-close! resp))

(test-case "smoke: GET /login returns 200 with login form"
  (define resp (get (test-url "/login") #:timeouts (make-timeout-config #:request 10)))
  (check-equal? (response-status-code resp) 200)
  (define body (bytes->string/utf-8 (response-body resp)))
  (check-not-false (string-contains? body "Log in"))
  (response-close! resp))

(test-case "smoke: GET /ping returns 200"
  (define resp (get (test-url "/ping") #:timeouts (make-timeout-config #:request 10)))
  (check-equal? (response-status-code resp) 200)
  (response-close! resp))

(test-case "smoke: GET /search returns 200"
  (define resp (get (test-url "/search") #:timeouts (make-timeout-config #:request 10)))
  (check-equal? (response-status-code resp) 200)
  (response-close! resp))

(test-case "smoke: GET /package/nonexistent-test-pkg returns a page"
  (define resp
    (get (test-url "/package/nonexistent-test-pkg") #:timeouts (make-timeout-config #:request 10)))
  ;; Should render (maybe 200 with "not found" content, or 404)
  (check-not-false (member (response-status-code resp) '(200 404)))
  (response-close! resp))
