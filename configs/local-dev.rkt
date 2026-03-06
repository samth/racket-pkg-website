#lang racket/base
;; Local development configuration with GitHub OAuth support.
;;
;; Prerequisites:
;;   1. Create a GitHub OAuth App at https://github.com/settings/developers
;;      - Application name: anything (e.g. "Racket Pkg Dev")
;;      - Homepage URL: http://localhost:7080
;;      - Authorization callback URL: http://localhost:7080/auth/github/callback
;;
;;   2. Set environment variables before running:
;;        export GITHUB_LOGIN_CLIENT_ID=<your client id>
;;        export GITHUB_LOGIN_CLIENT_SECRET=<your client secret>
;;
;;   3. Seed test data (optional but recommended):
;;        racket scripts/seed-local-dev.rkt
;;
;;   4. Run:  CONFIG=local-dev ./run
;;      Or:   CONFIG=local-dev make run

(require "../src/main.rkt"
         "../src/command-line.rkt")

(define port 7080)

(handle-command-line (lambda (config)
                       (main (hash-set* config
                                        'port
                                        port
                                        'ssl?
                                        #f
                                        'disable-cache?
                                        #t
                                        'dynamic-urlprefix
                                        (format "http://localhost:~a" port)
                                        'github-login-client-id
                                        (getenv "GITHUB_LOGIN_CLIENT_ID")
                                        'github-login-client-secret
                                        (getenv "GITHUB_LOGIN_CLIENT_SECRET")
                                        'pkg-index
                                        (hash 'ssl? #f 's3-bucket #f)
                                        'backup
                                        #f))))
