#lang racket/base

(provide go)

(require json
         net/url
         racket/file
         racket/list
         racket/match
         web-server/dispatch
         web-server/http
         web-server/http/basic-auth
         web-server/servlet-env
         (only-in infrastructure-userdb
                  user-password-correct?
                  lookup-user)
         "api.rkt"
         "basic.rkt"
         "build-update.rkt"
         "common.rkt"
         "notify.rkt"
         "static.rkt"
         "update.rkt"
         "../default.rkt")

(module+ test
  (require rackunit))

(define (hash-deep-merge ht more-ht)
  (for/fold ([ht ht]) ([(k new-v) (in-hash more-ht)])
    (hash-update ht
                 k
                 (λ (old-v)
                   (cond
                     [(not old-v) new-v]
                     [(hash? old-v) (hash-deep-merge old-v new-v)]
                     [else new-v]))
                 #f)))
(module+ test
  (check-equal? (hash-deep-merge (hasheq 'source "http://aws" 'descript "DrRacket")
                                 (hasheq 'source "http://github"))
                (hasheq 'source "http://github" 'descript "DrRacket")))

(define (api/upload req)
  (define req-data (read (open-input-bytes (or (request-post-data/raw req) #""))))
  (match-define (list email given-password pis) req-data)
  (cond
    [(not (user-password-correct? (lookup-user userdb email) (bytes->string/utf-8 given-password)))
     (log! "api/upload! failed pass, email was ~v" email)
     (response/sexpr #f)]
    [(not (curation-administrator? email))
     (log! "api/upload! not curator, email was ~v" email)
     (response/sexpr #f)]
    [else
     (log! "receiving api/upload!, email is ~v" email)
     (cond
       [(for/or ([p (in-hash-keys pis)])
          (define old-p (package-exists-as p))
          (and old-p
               (not (equal? p old-p))
               (begin
                 (log! "case mismatch for package name ~s" p)
                 #t)))
        (response/sexpr #f)]
       [else
        (for ([(p more-pi) (in-hash pis)])
          (log! "received api/upload for ~a" p)
          (define pi
            (cond
              [(package-exists-as p) (package-info p)]
              [else #hash()]))
          (define new-pi (hash-deep-merge pi more-pi))
          (define updated-pi
            (hash-remove (let ([now (current-seconds)])
                           (for/fold ([pi new-pi])
                                     ([k (in-list '(last-edit last-checked last-updated))])
                             (hash-set pi k now)))
                         'checksum))
          (log! "api/upload old ~v more ~v new ~v updated ~v"
                (hash-ref pi 'source #f)
                (hash-ref more-pi 'source #f)
                (hash-ref new-pi 'source #f)
                (hash-ref updated-pi 'source #f))
          (package-info-set! p updated-pi))
        (signal-update! (hash-keys pis))
        (response/sexpr #t)])]))

(define redirect-to-static
  (get-config redirect-to-static-proc
              (lambda (req)
                (redirect-to
                 (url->string
                  (struct-copy url
                               (request-uri req)
                               [scheme (get-config redirect-to-static-scheme "http")]
                               [host (get-config redirect-to-static-host "pkgs.racket-lang.org")]
                               [port (get-config redirect-to-static-port 80)]))))))

(define (ensure-authenticate req body-fun)
  (match (request->basic-credentials req)
    [(cons email passwd)
     (ensure-authenticate/email+passwd (bytes->string/utf-8 email)
                                       (bytes->string/utf-8 passwd)
                                       body-fun)]
    ;; TODO: things are structured awkwardly at the moment, but it'd
    ;; be nice to have this generate 401 Authentication Required with
    ;; a use of `make-basic-auth-header` to request credentials.
    [_ "authentication-required"]))

(define *cors-headers*
  (list (header #"Access-Control-Allow-Origin" #"*")
        (header #"Access-Control-Allow-Methods" #"POST, OPTIONS")
        (header #"Access-Control-Allow-Headers" #"content-type, authorization")))

(define (response/json o)
  (response/output (lambda (p) (write-json o p))
                   #:headers *cors-headers*
                   #:mime-type #"application/json"))

(define (wrap-with-cors-handler dispatcher)
  (lambda (req)
    (if (string-ci=? (bytes->string/latin-1 (request-method req)) "options")
        (response/output void #:headers *cors-headers*)
        (dispatcher req))))

(define (api/authenticate req)
  (define raw (request-post-data/raw req))
  (define req-data (read-json (open-input-bytes (or raw #""))))
  (response/json (and (hash? req-data)
                      (authenticate-user (hash-ref req-data 'email #f)
                                         (hash-ref req-data 'passwd #f)))))

(define (authenticated-json-post-rpc-service req handler)
  (response/json (ensure-authenticate
                  req
                  (lambda ()
                    (handler (read-json (open-input-bytes (or (request-post-data/raw req) #""))))))))

(define-syntax-rule (define-authenticated-json-post-rpc-service function-name
                                                                [(pat ...) body ...] ...)
  (define (function-name req)
    (authenticated-json-post-rpc-service req
                                         (match-lambda
                                           [(hash-table pat ...)
                                            body ...] ...))))

(define (api/package/modify-all req)
  (authenticated-json-post-rpc-service
   req
   (lambda (req-data)
     (and
      (hash? req-data)
      (let ([pkg (hash-ref req-data 'pkg #f)]
            [name (hash-ref req-data 'name #f)]
            [description (hash-ref req-data 'description #f)]
            [source (hash-ref req-data 'source #f)]
            [tags (hash-ref req-data 'tags #f)]
            [authors (hash-ref req-data 'authors #f)]
            [versions (hash-ref req-data 'versions #f)])
        (and (string? pkg)
             (string? name)
             (string? description)
             (string? source)
             (or (not tags) (and (list? tags) (andmap valid-tag? tags)))
             (or (not authors) (and (list? authors) (pair? authors) (andmap valid-author? authors)))
             (or (not versions) (and (list? versions) (andmap valid-versions-list-entry? versions)))
             (save-package! #:old-name pkg
                            #:new-name name
                            #:description description
                            #:source source
                            #:tags tags
                            #:authors authors
                            #:versions versions)))))))

(define-authenticated-json-post-rpc-service api/package/del
                                            [(['pkg pkg])
                                             (ensure-package-author pkg
                                                                    (λ ()
                                                                      (package-remove! pkg)
                                                                      (signal-static! empty)
                                                                      #f))])

(define-authenticated-json-post-rpc-service
 api/package/curate
 [(['package-names package-name-strings] ['ring proposed-new-ring])
  (curate-packages! package-name-strings proposed-new-ring)]
 [(['pkg pkg] ['ring ring]) (curate-packages! (list pkg) ring)])

(define-authenticated-json-post-rpc-service api/update [() (update-user-packages! (current-user))])

(define (api/notice req)
  (response/json (file->string notice-path)))

(define-values (main-dispatch main-url)
  ;;---------------------------------------------------------------------------
  ;; User management
  (dispatch-rules [("api" "authenticate") #:method "post" api/authenticate]
                  ;;---------------------------------------------------------------------------
                  ;; Wholesale package update of one kind or another
                  [("api" "upload") #:method "post" api/upload]
                  [("api" "update") #:method "post" api/update]
                  ;;---------------------------------------------------------------------------
                  ;; Individual package management
                  [("api" "package" "del") #:method "post" api/package/del]
                  [("api" "package" "modify-all") #:method "post" api/package/modify-all]
                  [("api" "package" "curate") #:method "post" api/package/curate]
                  ;;---------------------------------------------------------------------------
                  ;; Retrieve backend status message (no longer needed?)
                  [("api" "notice") api/notice]
                  ;;---------------------------------------------------------------------------
                  ;; Static resources
                  [else redirect-to-static]))

(define (go)
  (initialize!)
  (define port (get-config port default-pkg-index-port))
  (define ssl? (get-config ssl? #t))
  (log! "launching on port ~v" port)
  (signal-static! empty)
  (thread (λ ()
            (let loop ([initial? #t])
              (log! "update-t: Running scheduled build update.")
              (signal-build-update!)
              (log! "update-t: Running scheduled update.")
              (signal-update!/beat (if initial? 'all 'all/force))
              (log! "update-t: sleeping for 1 hour")
              (sleep (* 1 60 60))
              (loop #f))))
  (serve/servlet (wrap-with-cors-handler main-dispatch)
                 #:command-line? #t
                 ;; xxx I am getting strange behavior on some connections... maybe
                 ;; this will help?
                 #:connection-close? #t
                 #:listen-ip #f
                 #:ssl? ssl?
                 #:ssl-cert (and ssl? (build-path root "server-cert.pem"))
                 #:ssl-key (and ssl? (build-path root "private-key.pem"))
                 #:extra-files-paths empty
                 #:servlet-regexp #rx""
                 #:port port
                 #:log-file (current-output-port)))

(module+ main
  (go))
