#lang racket/base

(provide call-with-test-userdb
         create-test-user!
         make-test-request
         with-test-session
         call-with-test-packages-dir)

(require racket/file
         racket/string
         racket/promise
         net/url
         infrastructure-userdb
         web-server/http/request-structs
         "../sessions.rkt"
         "../pkg-index/common.rkt"
         (submod "../pkg-index/common.rkt" for-testing))

;; Run thunk with a temporary userdb directory.
;; The thunk receives the userdb as its argument.
;; Cleans up the temp directory after.
(define (call-with-test-userdb thunk)
  (define tmp (make-temporary-directory))
  (dynamic-wind void
                (lambda ()
                  (define db (userdb-config (path->string tmp) #t))
                  (thunk db))
                (lambda () (delete-directory/files tmp))))

;; Create a test user in the given userdb with the given email and password.
(define (create-test-user! db email password)
  (save-user! db (user-password-set (make-user email password) password)))

;; Build a request struct for testing.
;; cookies: alist of (name . value) strings, added as Cookie headers
(define (make-test-request #:method [method #"GET"]
                           #:url [url-string "/"]
                           #:headers [extra-headers '()]
                           #:cookies [cookies '()]
                           #:bindings [bindings '()]
                           #:post-data [post-data #f])
  (define cookie-headers
    (if (null? cookies)
        '()
        (list (header #"Cookie"
                      (string->bytes/utf-8
                       (string-join (map (lambda (c) (format "~a=~a" (car c) (cdr c))) cookies)
                                    "; "))))))
  (request method
           (string->url (string-append "http://localhost" url-string))
           (append extra-headers cookie-headers)
           (delay
             bindings)
           post-data
           "127.0.0.1"
           80
           "127.0.0.1"))


;; Run thunk with a test session set up for the given email.
(define (with-test-session email thunk #:curator? [curator? #f] #:superuser? [superuser? #f])
  (define key (create-session! email #:curator? curator? #:superuser? superuser?))
  (define s (lookup-session/touch! key))
  (parameterize ([current-session s])
    (thunk)))

;; Run thunk with a temporary packages directory and related state
;; (notice-path, static-path, cache-path) initialized for testing.
(define (call-with-test-packages-dir thunk)
  (define tmp (make-temporary-directory))
  (define tmp-static (make-temporary-directory))
  (dynamic-wind void
                (lambda ()
                  (initialize-for-testing! #:pkgs-path tmp
                                           #:userdb #f
                                           #:static-path tmp-static)
                  (thunk tmp))
                (lambda ()
                  (delete-directory/files tmp)
                  (delete-directory/files tmp-static))))
