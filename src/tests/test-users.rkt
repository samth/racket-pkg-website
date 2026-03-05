#lang racket/base

(require rackunit
         infrastructure-userdb
         "../users.rkt"
         (submod "../users.rkt" for-testing)
         "test-helpers.rkt")

(test-case "register-or-update-user! creates a user"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "newuser@example.com" "password123")
                           (check-true (user-exists? db "newuser@example.com")))))

(test-case "login-password-correct? returns #t for correct password"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "user@example.com" "correct")
                           (check-true (login-password-correct? "user@example.com" "correct")))))

(test-case "login-password-correct? returns #f for wrong password"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "user@example.com" "correct")
                           (check-false (login-password-correct? "user@example.com" "wrong")))))

(test-case "login-password-correct? returns #f for non-existent user"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (check-false (login-password-correct? "nobody@example.com" "anything")))))

(test-case "password change: new password works, old fails"
  (call-with-test-userdb (lambda (db)
                           (initialize-users-for-testing! db (make-registration-state))
                           (register-or-update-user! "user@example.com" "old-pass")
                           (check-true (login-password-correct? "user@example.com" "old-pass"))
                           (register-or-update-user! "user@example.com" "new-pass")
                           (check-false (login-password-correct? "user@example.com" "old-pass"))
                           (check-true (login-password-correct? "user@example.com" "new-pass")))))

(test-case "registration codes: generate and validate"
  (call-with-test-userdb (lambda (db)
                           (define codes (make-registration-state))
                           (initialize-users-for-testing! db codes)
                           (define code (generate-registration-code! codes "reg@example.com"))
                           (check-pred string? code)
                           (check-true (registration-code-correct? "reg@example.com" code)))))

(test-case "registration codes: wrong code fails"
  (call-with-test-userdb (lambda (db)
                           (define codes (make-registration-state))
                           (initialize-users-for-testing! db codes)
                           (generate-registration-code! codes "reg@example.com")
                           (check-false (registration-code-correct? "reg@example.com"
                                                                    "wrong-code")))))

(test-case "registration codes: wrong email fails"
  (call-with-test-userdb (lambda (db)
                           (define codes (make-registration-state))
                           (initialize-users-for-testing! db codes)
                           (define code (generate-registration-code! codes "reg@example.com"))
                           (check-false (registration-code-correct? "other@example.com" code)))))

(test-case "send-registration-or-reset-email! sends reset for existing user"
  (call-with-test-userdb
   (lambda (db)
     (define codes (make-registration-state))
     (initialize-users-for-testing! db codes)
     (register-or-update-user! "existing@example.com" "pass")
     (define sent-subjects '())
     (parameterize ([current-send-email
                     (lambda (from subject to body)
                       (set! sent-subjects (cons subject sent-subjects)))])
       (send-registration-or-reset-email! "existing@example.com"))
     (check-equal? (length sent-subjects) 1)
     (check-not-false (regexp-match #rx"reset" (car sent-subjects))))))

(test-case "send-registration-or-reset-email! sends registration for new user"
  (call-with-test-userdb
   (lambda (db)
     (define codes (make-registration-state))
     (initialize-users-for-testing! db codes)
     (define sent-subjects '())
     (parameterize ([current-send-email
                     (lambda (from subject to body)
                       (set! sent-subjects (cons subject sent-subjects)))])
       (send-registration-or-reset-email! "newuser@example.com"))
     (check-equal? (length sent-subjects) 1)
     (check-not-false (regexp-match #rx"confirmation" (car sent-subjects))))))
