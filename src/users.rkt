#lang racket/base
;; User management - userdb, plus registration and emailing

(provide login-password-correct?
         send-registration-or-reset-email!
         registration-code-correct?
         register-or-update-user!
         ensure-user-id!
         user-id-for-email
         lookup-user-by-github-id
         link-github-account!
         unlink-github-account!
         create-github-user!
         github-username-for-email
         user-exists?/email
         initialize-users!)

(module+ for-testing
  (provide initialize-users-for-testing!
           current-send-email))

(require reloadable)
(require infrastructure-userdb)
(require racket/random)
(require file/sha1)
(require "config.rkt")
(require "hash-utils.rkt")
(require "default.rkt")
(require (prefix-in real: "send-email.rkt"))

;; Indirection for testing: allows tests to replace the email sender
(define current-send-email (make-parameter real:send-email))

(define-logger racket-pkg-website/users)

;; State variables, initialized lazily or via initialize-users! / initialize-users-for-testing!
(define userdb #f)
(define *codes* #f)

(define (ensure-initialized!)
  (unless userdb
    (initialize-users!)))

(define (initialize-users!)
  (set! userdb
        (userdb-config (config-path (or (@ (config) user-directory)
                                        (default-users (or (@ (config) root) default-root))))
                       #t ;; writeable!
                       ))
  (set! *codes* (make-persistent-state '*codes* (lambda () (make-registration-state))))
  (log-racket-pkg-website/users-info "Will use sender address ~v" (sender-address)))

(define (initialize-users-for-testing! test-userdb test-codes-state)
  (set! userdb test-userdb)
  (set! *codes* (lambda () test-codes-state))
  (hash-clear! github-id-cache)
  (set! github-id-cache-built? #f))

(define (login-password-correct? email given-password)
  (ensure-initialized!)
  (log-racket-pkg-website/users-info "Checking password for ~v" email)
  (user-password-correct? (lookup-user userdb email) given-password))

(define (send-registration-or-reset-email! email)
  (ensure-initialized!)
  (if (user-exists? userdb email)
      (send-password-reset-email! email)
      (send-account-registration-email! email)))

(define (sender-address)
  (or (@ (config) email-sender-address) "pkgs@racket-lang.org"))

(define (send-password-reset-email! email)
  (log-racket-pkg-website/users-info "Sending password reset email to ~v" email)
  ((current-send-email)
   (sender-address)
   "Account password reset for Racket Package Catalog"
   (list email)
   (list
    "Someone tried to login with your email address for an account on the Racket Package Catalog, but failed."
    "If this was you, please use this code to reset your password:"
    ""
    (generate-registration-code! (*codes*) email)
    ""
    "This code will expire, so if it is not available, you'll have to try again.")))

(define (send-account-registration-email! email)
  (log-racket-pkg-website/users-info "Sending account registration email to ~v" email)
  ((current-send-email)
   (sender-address)
   "Account confirmation for Racket Package Catalog"
   (list email)
   (list "Someone tried to register your email address for an account on the Racket Package Catalog."
         "If you want to proceed, use this code:"
         ""
         (generate-registration-code! (*codes*) email)
         ""
         "This code will expire, so if it is not available, you'll have to try to register again.")))

(define (registration-code-correct? email given-code)
  (ensure-initialized!)
  (log-racket-pkg-website/users-info "Checking registration code for ~v" email)
  (check-registration-code (*codes*) email given-code (lambda () #t) (lambda () #f)))

(define (register-or-update-user! email password)
  (ensure-initialized!)
  (log-racket-pkg-website/users-info "Updating user record ~v" email)
  (save-user! userdb
              (user-password-set (or (lookup-user userdb email) (make-user email password))
                                 password)))

(define (generate-user-id)
  (define bs (crypto-random-bytes 16))
  ;; Format as UUID v4: set version bits (byte 6) and variant bits (byte 8)
  (bytes-set! bs 6 (bitwise-ior #x40 (bitwise-and #x0f (bytes-ref bs 6))))
  (bytes-set! bs 8 (bitwise-ior #x80 (bitwise-and #x3f (bytes-ref bs 8))))
  (define h (bytes->hex-string bs))
  (format "~a-~a-~a-~a-~a"
          (substring h 0 8)
          (substring h 8 12)
          (substring h 12 16)
          (substring h 16 20)
          (substring h 20 32)))

;; user-property values survive a save/load round-trip as single-element lists
;; because the serialization format uses (list key value) pairs.
;; This helper normalizes either form to a plain value.
(define (unwrap-property v)
  (if (and (pair? v) (null? (cdr v)))
      (car v)
      v))

(define (ensure-user-id! email)
  (ensure-initialized!)
  (define u (lookup-user userdb email))
  (unless u (error 'ensure-user-id! "user ~a does not exist" email))
  (define existing (user-property u 'user-id #f))
  (cond
    [existing (unwrap-property existing)]
    [else
     (define id (generate-user-id))
     (save-user! userdb (user-property-set u 'user-id id))
     id]))

(define (user-id-for-email email)
  (ensure-initialized!)
  (define u (lookup-user userdb email (lambda _ #f)))
  (define v (and u (user-property u 'user-id #f)))
  (and v (unwrap-property v)))

;; GitHub identity storage

;; In-memory cache: github-id (number) -> email (string)
(define github-id-cache (make-hash))
(define github-id-cache-built? #f)

(define (rebuild-github-id-cache!)
  (hash-clear! github-id-cache)
  (for ([email (in-list (list-users userdb))])
    (define u (lookup-user userdb email (lambda _ #f)))
    (when u
      (define raw (user-property u 'github-id #f))
      (define gid (and raw (unwrap-property raw)))
      (when gid
        (hash-set! github-id-cache gid email))))
  (set! github-id-cache-built? #t))

(define (lookup-user-by-github-id github-id)
  (ensure-initialized!)
  (unless github-id-cache-built?
    (rebuild-github-id-cache!))
  (hash-ref github-id-cache github-id #f))

(define (link-github-account! email github-id github-username github-email)
  (ensure-initialized!)
  (define u (lookup-user userdb email))
  (unless u (error 'link-github-account! "user ~a does not exist" email))
  (define old-gid (user-property u 'github-id #f))
  (when old-gid
    (define old-gid* (unwrap-property old-gid))
    (when (and old-gid* (not (equal? old-gid* github-id)))
      (hash-remove! github-id-cache old-gid*)))
  (save-user! userdb
              (user-property-set
               (user-property-set
                (user-property-set u 'github-id github-id)
                'github-username github-username)
               'github-email github-email))
  (hash-set! github-id-cache github-id email))

(define (create-github-user! email github-id github-username github-email)
  (ensure-initialized!)
  (define random-password (bytes->hex-string (crypto-random-bytes 32)))
  (define u (make-user email random-password))
  (save-user! userdb
              (user-property-set
               (user-property-set
                (user-property-set u 'github-id github-id)
                'github-username github-username)
               'github-email github-email))
  (hash-set! github-id-cache github-id email))

(define (unlink-github-account! email)
  (ensure-initialized!)
  (define u (lookup-user userdb email))
  (unless u (error 'unlink-github-account! "user ~a does not exist" email))
  (define gid (user-property u 'github-id #f))
  (when gid (hash-remove! github-id-cache (unwrap-property gid)))
  (save-user! userdb
              (user-property-set
               (user-property-set
                (user-property-set u 'github-id #f)
                'github-username #f)
               'github-email #f)))

(define (github-username-for-email email)
  (ensure-initialized!)
  (define u (lookup-user userdb email (lambda _ #f)))
  (define v (and u (user-property u 'github-username #f)))
  (and v (unwrap-property v)))

(define (user-exists?/email email)
  (ensure-initialized!)
  (user-exists? userdb email))
