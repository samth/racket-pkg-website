#lang racket/base

(require rackunit
         racket/file
         infrastructure-userdb
         "../pkg-index/common.rkt"
         (submod "../pkg-index/common.rkt" for-testing)
         "../pkg-index/dynamic.rkt"
         "../users.rkt"
         (submod "../users.rkt" for-testing)
         "test-helpers.rkt")

;; Helper: run a thunk with both test userdb (for users.rkt writes)
;; and test packages dir (for common.rkt package operations).
;; Sets up all test state including notice-path etc. so background
;; threads from signal-update! don't error.
(define (call-with-test-env thunk)
  (call-with-test-userdb (lambda (db)
                           (call-with-test-packages-dir
                            (lambda (pkgs-dir)
                              ;; Set up users.rkt for writing
                              (initialize-users-for-testing! db (make-registration-state))
                              ;; Set backend's userdb to the same test db
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

(test-case "save-package! fails for duplicate name"
  (call-with-test-env (lambda (db pkgs-dir)
                        (register-or-update-user! "author@example.com" "pass")
                        (parameterize ([current-user "author@example.com"])
                          (save-package! #:old-name ""
                                         #:new-name "dup-pkg"
                                         #:description "First"
                                         #:source "https://github.com/test/dup.git"
                                         #:tags #f
                                         #:authors '("author@example.com")
                                         #:versions #f)
                          ;; Creating again with same name should fail
                          (define result
                            (save-package! #:old-name ""
                                           #:new-name "dup-pkg"
                                           #:description "Second"
                                           #:source "https://github.com/test/dup2.git"
                                           #:tags #f
                                           #:authors '("author@example.com")
                                           #:versions #f))
                          (check-false result)))))

(test-case "save-package! fails for invalid name"
  (call-with-test-env (lambda (db pkgs-dir)
                        (register-or-update-user! "author@example.com" "pass")
                        (parameterize ([current-user "author@example.com"])
                          (define result
                            (save-package! #:old-name ""
                                           #:new-name "bad name!"
                                           #:description "Invalid"
                                           #:source "https://github.com/test/bad.git"
                                           #:tags #f
                                           #:authors '("author@example.com")
                                           #:versions #f))
                          (check-false result)))))

(test-case "save-package! updates existing package"
  (call-with-test-env (lambda (db pkgs-dir)
                        (register-or-update-user! "author@example.com" "pass")
                        (parameterize ([current-user "author@example.com"])
                          (save-package! #:old-name ""
                                         #:new-name "upd-pkg"
                                         #:description "Original desc"
                                         #:source "https://github.com/test/upd.git"
                                         #:tags #f
                                         #:authors '("author@example.com")
                                         #:versions #f)
                          (define result
                            (save-package! #:old-name "upd-pkg"
                                           #:new-name "upd-pkg"
                                           #:description "Updated desc"
                                           #:source "https://github.com/test/upd.git"
                                           #:tags #f
                                           #:authors #f
                                           #:versions #f))
                          (check-true result)
                          (define info (package-info "upd-pkg"))
                          (check-equal? (hash-ref info 'description) "Updated desc")))))

(test-case "package-remove! deletes a package"
  (call-with-test-env (lambda (db pkgs-dir)
                        (register-or-update-user! "author@example.com" "pass")
                        (parameterize ([current-user "author@example.com"])
                          (save-package! #:old-name ""
                                         #:new-name "del-pkg"
                                         #:description "To delete"
                                         #:source "https://github.com/test/del.git"
                                         #:tags #f
                                         #:authors '("author@example.com")
                                         #:versions #f)
                          (check-true (package-exists? "del-pkg"))
                          (package-remove! "del-pkg")
                          (check-false (package-exists? "del-pkg"))))))

(test-case "packages-of lists packages by author"
  (call-with-test-env (lambda (db pkgs-dir)
                        (register-or-update-user! "alice@example.com" "pass")
                        (register-or-update-user! "bob@example.com" "pass")
                        (parameterize ([current-user "alice@example.com"])
                          (save-package! #:old-name ""
                                         #:new-name "alice-pkg"
                                         #:description "Alice's"
                                         #:source "https://github.com/test/alice.git"
                                         #:tags #f
                                         #:authors '("alice@example.com")
                                         #:versions #f))
                        (parameterize ([current-user "bob@example.com"])
                          (save-package! #:old-name ""
                                         #:new-name "bob-pkg"
                                         #:description "Bob's"
                                         #:source "https://github.com/test/bob.git"
                                         #:tags #f
                                         #:authors '("bob@example.com")
                                         #:versions #f))
                        (define alice-pkgs (packages-of "alice@example.com"))
                        (check-not-false (member "alice-pkg" alice-pkgs))
                        (check-false (member "bob-pkg" alice-pkgs)))))

(test-case "package-list returns all packages sorted"
  (call-with-test-env (lambda (db pkgs-dir)
                        (register-or-update-user! "author@example.com" "pass")
                        (parameterize ([current-user "author@example.com"])
                          (save-package! #:old-name "" #:new-name "z-pkg" #:description "Z"
                                         #:source "https://github.com/test/z.git"
                                         #:tags #f #:authors '("author@example.com") #:versions #f)
                          (save-package! #:old-name "" #:new-name "a-pkg" #:description "A"
                                         #:source "https://github.com/test/a.git"
                                         #:tags #f #:authors '("author@example.com") #:versions #f))
                        (define pkgs (package-list))
                        (check-equal? pkgs '("a-pkg" "z-pkg")))))

(test-case "package-ref returns defaults for missing keys"
  (call-with-test-env (lambda (db pkgs-dir)
                        (register-or-update-user! "author@example.com" "pass")
                        (parameterize ([current-user "author@example.com"])
                          (save-package! #:old-name "" #:new-name "ref-pkg" #:description "Test"
                                         #:source "https://github.com/test/ref.git"
                                         #:tags #f #:authors '("author@example.com") #:versions #f))
                        (define info (package-info "ref-pkg"))
                        (check-equal? (package-ref info 'checksum) "")
                        (check-equal? (package-ref info 'ring) 2)
                        (check-equal? (package-ref info 'tags) '())
                        (check-equal? (package-ref info 'checksum-error) #f))))

(test-case "notice file is written after package operations"
  (call-with-test-env (lambda (db pkgs-dir)
                        (register-or-update-user! "author@example.com" "pass")
                        (parameterize ([current-user "author@example.com"])
                          (save-package! #:old-name ""
                                         #:new-name "notice-pkg"
                                         #:description "Test notice"
                                         #:source "https://github.com/test/notice.git"
                                         #:tags #f
                                         #:authors '("author@example.com")
                                         #:versions #f))
                        ;; Wait briefly for background thread to write notice
                        (sleep 1)
                        (check-true (file-exists? notice-path)
                                    "notice file should exist after package save"))))
