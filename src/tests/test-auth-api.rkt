#lang racket/base

(require rackunit
         racket/file
         infrastructure-userdb
         "../pkg-index/common.rkt"
         "../pkg-index/dynamic.rkt"
         "../users.rkt"
         "test-helpers.rkt")

;; Helper: run a thunk with both test userdb (for users.rkt writes)
;; and test packages dir (for common.rkt package operations).
;; Also sets up the backend's read-only userdb to point at the same dir.
(define (call-with-test-env thunk)
  (call-with-test-userdb (lambda (db)
                           (call-with-test-packages-dir
                            (lambda (pkgs-dir)
                              ;; Set up users.rkt for writing
                              (initialize-users-for-testing! db (make-registration-state))
                              ;; Set up common.rkt's read-only userdb to same directory
                              ;; (both point at same dir so auth checks work)
                              (set-userdb-for-testing! db)
                              (thunk db pkgs-dir))))))

(test-case "curation-administrator? recognizes known curators"
  (check-not-false (curation-administrator? "jay.mccarthy@gmail.com"))
  (check-not-false (curation-administrator? "samth@ccs.neu.edu"))
  (check-false (curation-administrator? "random@example.com")))

(test-case "superuser? recognizes known superusers"
  (check-not-false (superuser? "mflatt@cs.utah.edu"))
  (check-false (superuser? "random@example.com")))

(test-case "authenticate with correct credentials"
  (call-with-test-env
   (lambda (db pkgs-dir)
     (register-or-update-user! "auth@example.com" "goodpass")
     (define result (ensure-authenticate/email+passwd "auth@example.com" "goodpass" (lambda () 'ok)))
     (check-equal? result 'ok))))

(test-case "authenticate with wrong credentials"
  (call-with-test-env
   (lambda (db pkgs-dir)
     (register-or-update-user! "auth@example.com" "goodpass")
     (define result (ensure-authenticate/email+passwd "auth@example.com" "badpass" (lambda () 'ok)))
     (check-equal? result "not-authenticated"))))

(test-case "save-package! creates a new package"
  (call-with-test-env (lambda (db pkgs-dir)
                        (register-or-update-user! "author@example.com" "pass")
                        (parameterize ([current-user "author@example.com"])
                          (define result
                            (save-package! #:old-name ""
                                           #:new-name "test-pkg"
                                           #:description "A test package"
                                           #:source "https://github.com/test/test-pkg.git"
                                           #:tags '("test")
                                           #:authors '("author@example.com")
                                           #:versions #f))
                          (check-true result)
                          (check-true (package-exists? "test-pkg"))
                          (define info (package-info "test-pkg"))
                          (check-equal? (hash-ref info 'description) "A test package")))))

(test-case "package-author? checks authorship"
  (call-with-test-env (lambda (db pkgs-dir)
                        (register-or-update-user! "author@example.com" "pass")
                        (parameterize ([current-user "author@example.com"])
                          (save-package! #:old-name ""
                                         #:new-name "my-pkg"
                                         #:description "My package"
                                         #:source "https://github.com/test/my-pkg.git"
                                         #:tags #f
                                         #:authors '("author@example.com")
                                         #:versions #f))
                        (check-not-false (package-author? "my-pkg" "author@example.com"))
                        (check-false (package-author? "my-pkg" "other@example.com")))))

(test-case "non-author cannot modify package"
  (call-with-test-env (lambda (db pkgs-dir)
                        (register-or-update-user! "author@example.com" "pass")
                        (register-or-update-user! "other@example.com" "pass")
                        (parameterize ([current-user "author@example.com"])
                          (save-package! #:old-name ""
                                         #:new-name "owned-pkg"
                                         #:description "Owned"
                                         #:source "https://github.com/test/owned.git"
                                         #:tags #f
                                         #:authors '("author@example.com")
                                         #:versions #f))
                        (parameterize ([current-user "other@example.com"])
                          (define result
                            (save-package! #:old-name "owned-pkg"
                                           #:new-name "owned-pkg"
                                           #:description "Hijacked!"
                                           #:source "https://github.com/evil/owned.git"
                                           #:tags #f
                                           #:authors #f
                                           #:versions #f))
                          (check-false result)))))

(test-case "superuser can modify any package"
  (call-with-test-env (lambda (db pkgs-dir)
                        (register-or-update-user! "author@example.com" "pass")
                        (parameterize ([current-user "author@example.com"])
                          (save-package! #:old-name ""
                                         #:new-name "any-pkg"
                                         #:description "Original"
                                         #:source "https://github.com/test/any.git"
                                         #:tags #f
                                         #:authors '("author@example.com")
                                         #:versions #f))
                        ;; Use a known superuser
                        (parameterize ([current-user "jay.mccarthy@gmail.com"])
                          (define result
                            (save-package! #:old-name "any-pkg"
                                           #:new-name "any-pkg"
                                           #:description "Updated by superuser"
                                           #:source "https://github.com/test/any.git"
                                           #:tags #f
                                           #:authors #f
                                           #:versions #f))
                          (check-true result)))))

(test-case "curate-packages! changes ring for curator"
  (call-with-test-env (lambda (db pkgs-dir)
                        (register-or-update-user! "author@example.com" "pass")
                        (parameterize ([current-user "author@example.com"])
                          (save-package! #:old-name ""
                                         #:new-name "curate-pkg"
                                         #:description "To curate"
                                         #:source "https://github.com/test/curate.git"
                                         #:tags #f
                                         #:authors '("author@example.com")
                                         #:versions #f))
                        (parameterize ([current-user "jay.mccarthy@gmail.com"])
                          (define result (curate-packages! (list "curate-pkg") 1))
                          (check-true result)
                          (define info (package-info "curate-pkg"))
                          (check-equal? (hash-ref info 'ring) 1)))))

(test-case "curate-packages! fails for non-curator"
  (call-with-test-env (lambda (db pkgs-dir)
                        (register-or-update-user! "author@example.com" "pass")
                        (parameterize ([current-user "author@example.com"])
                          (save-package! #:old-name ""
                                         #:new-name "curate-pkg2"
                                         #:description "To curate"
                                         #:source "https://github.com/test/curate2.git"
                                         #:tags #f
                                         #:authors '("author@example.com")
                                         #:versions #f))
                        (parameterize ([current-user "author@example.com"])
                          (define result (curate-packages! (list "curate-pkg2") 0))
                          (check-false result)))))
