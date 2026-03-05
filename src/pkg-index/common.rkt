#lang racket/base
(require racket/file
         racket/runtime-path
         racket/match
         racket/list
         racket/date
         racket/string
         pkg/private/stage
         plt-service-monitor/beat
         infrastructure-userdb
         "config.rkt"
         "../default.rkt")

;; This f o f^-1 is applied because it throws an error if file is not
;; a single path element. This causes things like "../../etc/passwd"
;; to throw errors and thus be protected.
(define (build-path^ base file)
  (build-path base (path-element->string (string->path-element file))))

(define-runtime-path src* ".")

;; State variables, initialized by initialize!
(define src #f)
(define root #f)
(define users.new-path #f)
(define userdb #f)
(define cache-path #f)
(define SUMMARY-NAME "summary.rktd")
(define SUMMARY-PATH #f)
(define pkgs-path #f)
(define static.src-path #f)
(define static-path #f)
(define notice-path #f)
(define s3-bucket #f)
(define s3-bucket-region #f)
(define beat-s3-bucket #f)

(define (initialize!)
  (set! src (get-config src src*))
  (set! root (get-config root default-root))
  (make-directory* root)
  (set! users.new-path (get-config users.new-path (default-users root)))
  (set! userdb
        (userdb-config users.new-path
                       #f ;; write not permitted. The racket-pkg-website does all writes.
                       ))
  ;; Since package downloads don't normally use the GitHub API anymore,
  ;; allow the GitHub options to be #f and make the default load strings
  ;; only if default files exist
  (let ([check+load-file (lambda (filename)
                           (if (file-exists? filename)
                               (file->string filename)
                               (begin
                                 #;(raise-user-error 'pkg-index "Cannot find file ~a" filename)
                                 #f)))])
    (github-client_id (get-config github-client_id (check+load-file (build-path root "client_id"))))
    (github-client_secret (get-config github-client_secret
                                      (check+load-file (build-path root "client_secret")))))

  (set! cache-path (get-config cache-path (build-path root "cache")))
  (make-directory* cache-path)
  (set! SUMMARY-PATH (build-path cache-path SUMMARY-NAME))

  (set! pkgs-path (get-config pkgs-path (build-path root "pkgs")))
  (make-directory* pkgs-path)

  (set! static.src-path (get-config static.src-path (build-path src "static")))
  (set! static-path (get-config static-path default-static-gen))
  (set! notice-path (get-config notice-path (build-path static-path "notice.json")))
  (make-directory* static-path)

  (set! s3-bucket (get-config s3-bucket #f))
  (set! s3-bucket-region (get-config s3-bucket-region #f))
  (set! beat-s3-bucket (get-config beat-s3-bucket #f)))

(define (package-list)
  (sort (map path->string (directory-list pkgs-path)) string-ci<=?))

(define (package-exists? pkg-name)
  (file-exists? (build-path^ pkgs-path pkg-name)))

(define (read-package-info pkg-name)
  (with-handlers ([exn:fail? (λ (x)
                               ((error-display-handler) (exn-message x) x)
                               (hasheq))])
    (define p (build-path^ pkgs-path pkg-name))
    (define v
      (if (package-exists? pkg-name)
          (file->value p)
          (hasheq)))
    (define ht
      (if (hash? v)
          v
          (hasheq)))
    ht))

(define (package-info pkg-name #:version [version #f])
  (define ht (read-package-info pkg-name))
  (define no-version (hash-set ht 'name pkg-name))
  (cond
    [(and version
          (hash-has-key? no-version 'versions)
          (hash? (hash-ref no-version 'versions #f))
          (hash-has-key? (hash-ref no-version 'versions) version)
          (hash? (hash-ref (hash-ref no-version 'versions) version #f)))
     =>
     (λ (version-ht) (hash-merge version-ht no-version))]
    [else no-version]))

(define (package-ref pkg-info key)
  (hash-ref
   pkg-info
   key
   (λ ()
     (match key
       [(or 'author 'source)
        (error 'pkg "Package ~e is missing a required field: ~e" (hash-ref pkg-info 'name) key)]
       ['checksum ""]
       ['ring 2]
       ['checksum-error #f]
       ['tags empty]
       ['versions (hash)]
       [(or 'last-checked 'last-edit 'last-updated) -inf.0]))))

(define (package-info-set! pkg-name i)
  (call-with-atomic-output-file (build-path^ pkgs-path pkg-name) (lambda (out path) (write i out))))

(define (hash-merge from to)
  (for/fold ([to to]) ([(k v) (in-hash from)])
    (hash-set to k v)))

(define (author->list as)
  (string-split as))

(define (valid-name? t)
  (not (regexp-match #rx"[^a-zA-Z0-9_\\-]" t)))

(define (valid-author? a)
  (not (regexp-match #rx"[ :]" a)))

(define valid-tag? valid-name?)

(define (log!* args suffix)
  (parameterize ([date-display-format 'iso-8601])
    (printf "~a: ~a~a" (date->string (current-date) #t) (apply format args) suffix)
    (flush-output)))

(define (log! . args)
  (log!* args "\n"))

(define (log!/no-newline . args)
  (log!* args ""))

(define (run! f args)
  (log! "START ~a ~v" f args)
  (f args)
  (log! "END ~a ~v" f args))

(define (safe-run! run-sema t)
  (thread (λ ()
            (call-with-semaphore
             run-sema
             (λ ()
               (with-handlers ([exn:fail? (λ (x) ((error-display-handler) (exn-message x) x))])
                 (t)))))))

(define (heartbeat task)
  (when beat-s3-bucket
    (beat beat-s3-bucket task)))

;; For testing: override state for test isolation
(define (set-pkgs-path-for-testing! path)
  (set! pkgs-path (if (path? path) (path->string path) path)))

(define (set-userdb-for-testing! db)
  (set! userdb db))

(define (initialize-for-testing! #:pkgs-path test-pkgs-path
                                 #:userdb test-userdb
                                 #:static-path [test-static-path #f])
  (define tmp-static (or test-static-path (make-temporary-directory)))
  (set! pkgs-path (if (path? test-pkgs-path) (path->string test-pkgs-path) test-pkgs-path))
  (set! userdb test-userdb)
  (set! static-path (if (path? tmp-static) (path->string tmp-static) tmp-static))
  (make-directory* static-path)
  (set! notice-path (build-path static-path "notice.json"))
  (set! cache-path (build-path static-path "cache"))
  (make-directory* cache-path)
  (set! SUMMARY-PATH (build-path cache-path SUMMARY-NAME))
  (set! static.src-path (build-path static-path "static-src"))
  (make-directory* static.src-path))

(provide (except-out (all-defined-out)
                     set-pkgs-path-for-testing!
                     set-userdb-for-testing!
                     initialize-for-testing!))
(provide (all-from-out "config.rkt"))

(module+ for-testing
  (provide set-pkgs-path-for-testing!
           set-userdb-for-testing!
           initialize-for-testing!))
