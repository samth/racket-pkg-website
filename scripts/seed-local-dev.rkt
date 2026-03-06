#lang racket/base
;; Seed the local dev instance with sample packages and user accounts.
;;
;; Usage: racket scripts/seed-local-dev.rkt
;;
;; Creates:
;;  - 3 user accounts (alice, bob, curator) with known passwords
;;  - ~10 sample packages with realistic metadata
;;
;; Run this before starting the server. The server uses compiled/root/
;; by default for all state.

(require racket/file
         infrastructure-userdb)

(define root "compiled/root")
(define pkgs-dir (build-path root "pkgs"))
(define users-dir (build-path root "users.new"))

;; Ensure directories exist
(make-directory* pkgs-dir)
(make-directory* users-dir)

;; ---- Users ----

(define userdb (userdb-config (path->string users-dir) #t))

(define (create-user! email password)
  (if (user-exists? userdb email)
      (printf "  user ~a already exists, skipping\n" email)
      (begin
        (save-user! userdb
                    (user-property-set
                     (user-password-set (make-user email password) password)
                     'has-password #t))
        (printf "  created user ~a (password: ~a)\n" email password))))

(printf "Creating users...\n")
(create-user! "alice@example.com" "alice123")
(create-user! "bob@example.com" "bob123")
;; Jay is a hardcoded curator/superuser in the codebase
(create-user! "jay.mccarthy@gmail.com" "curator123")

;; ---- Packages ----

(define now (current-seconds))

(define (write-package! name info)
  (define path (build-path pkgs-dir name))
  (if (file-exists? path)
      (printf "  package ~a already exists, skipping\n" name)
      (begin
        (call-with-output-file path (lambda (out) (write info out)))
        (printf "  created package ~a\n" name))))

(define sample-packages
  `(("hello-world" . ,(hash 'name
                            "hello-world"
                            'source
                            "https://github.com/racket/racket.git?path=pkgs/racket-test"
                            'description
                            "A simple hello world package for testing"
                            'authors
                            '("alice@example.com")
                            'tags
                            '("hello" "example" "tutorial")
                            'ring
                            2
                            'last-edit
                            now
                            'last-checked
                            now
                            'last-updated
                            now
                            'checksum
                            ""
                            'versions
                            (hash)))
    ("web-utils" . ,(hash 'name
                          "web-utils"
                          'source
                          "https://github.com/racket/web-server.git"
                          'description
                          "Utilities for building web applications in Racket"
                          'authors
                          '("alice@example.com")
                          'tags
                          '("web" "http" "server" "utilities")
                          'ring
                          1
                          'last-edit
                          now
                          'last-checked
                          now
                          'last-updated
                          now
                          'checksum
                          ""
                          'versions
                          (hash)))
    ("data-structures" . ,(hash 'name
                                "data-structures"
                                'source
                                "https://github.com/racket/racket.git?path=pkgs/racket-lib"
                                'description
                                "Advanced data structures: red-black trees, tries, and more"
                                'authors
                                '("bob@example.com")
                                'tags
                                '("data-structures" "algorithms" "collections")
                                'ring
                                2
                                'last-edit
                                now
                                'last-checked
                                now
                                'last-updated
                                now
                                'checksum
                                ""
                                'versions
                                (hash)))
    ("plot-extras" . ,(hash 'name
                            "plot-extras"
                            'source
                            "https://github.com/racket/plot.git"
                            'description
                            "Extra plotting backends and themes for the plot library"
                            'authors
                            '("bob@example.com")
                            'tags
                            '("plot" "visualization" "graphics")
                            'ring
                            2
                            'last-edit
                            now
                            'last-checked
                            now
                            'last-updated
                            now
                            'checksum
                            ""
                            'versions
                            (hash)))
    ("json-schema" . ,(hash 'name
                            "json-schema"
                            'source
                            "https://github.com/greghendershott/json-pointer.git"
                            'description
                            "JSON Schema validation for Racket"
                            'authors
                            '("alice@example.com" "bob@example.com")
                            'tags
                            '("json" "schema" "validation" "web")
                            'ring
                            2
                            'last-edit
                            now
                            'last-checked
                            now
                            'last-updated
                            now
                            'checksum
                            ""
                            'versions
                            (hash)))
    ("markdown-parser" . ,(hash 'name
                                "markdown-parser"
                                'source
                                "https://github.com/greghendershott/markdown.git"
                                'description
                                "CommonMark-compliant Markdown parser"
                                'authors
                                '("alice@example.com")
                                'tags
                                '("markdown" "parser" "documentation")
                                'ring
                                2
                                'last-edit
                                now
                                'last-checked
                                now
                                'last-updated
                                now
                                'checksum
                                ""
                                'versions
                                (hash)))
    ("testing-framework"
     . ,(hash 'name
              "testing-framework"
              'source
              "https://github.com/racket/rackunit.git"
              'description
              "Extended testing framework with property-based testing"
              'authors
              '("bob@example.com")
              'tags
              '("testing" "rackunit" "property-based")
              'ring
              1
              'last-edit
              now
              'last-checked
              now
              'last-updated
              now
              'checksum
              ""
              'versions
              (hash "8.0" (hash 'source "https://github.com/racket/rackunit.git" 'checksum ""))))
    ("sql-connect" . ,(hash 'name
                            "sql-connect"
                            'source
                            "https://github.com/racket/db.git"
                            'description
                            "Database connectivity for PostgreSQL, MySQL, and SQLite"
                            'authors
                            '("alice@example.com")
                            'tags
                            '("database" "sql" "postgresql" "mysql" "sqlite")
                            'ring
                            2
                            'last-edit
                            now
                            'last-checked
                            now
                            'last-updated
                            now
                            'checksum
                            ""
                            'versions
                            (hash)))
    ("crypto-utils" . ,(hash 'name
                             "crypto-utils"
                             'source
                             "https://github.com/rmculpepper/crypto.git"
                             'description
                             "Cryptographic primitives and utilities"
                             'authors
                             '("bob@example.com")
                             'tags
                             '("crypto" "security" "encryption")
                             'ring
                             2
                             'last-edit
                             now
                             'last-checked
                             now
                             'last-updated
                             now
                             'checksum
                             ""
                             'versions
                             (hash)))
    ("gui-widgets" . ,(hash 'name
                            "gui-widgets"
                            'source
                            "https://github.com/racket/gui.git"
                            'description
                            "Custom GUI widgets and layout helpers"
                            'authors
                            '("alice@example.com" "bob@example.com")
                            'tags
                            '("gui" "widgets" "ui" "racket/gui")
                            'ring
                            2
                            'last-edit
                            now
                            'last-checked
                            now
                            'last-updated
                            now
                            'checksum
                            ""
                            'versions
                            (hash)))))

(printf "Creating packages...\n")
(for ([entry (in-list sample-packages)])
  (write-package! (car entry) (cdr entry)))

(printf "\nDone. Local dev data is in ~a/\n" root)
(printf "\nTest accounts:\n")
(printf "  alice@example.com / alice123\n")
(printf "  bob@example.com   / bob123\n")
(printf "  jay.mccarthy@gmail.com / curator123  (curator + superuser)\n")
(printf "\nTo start the server:\n")
(printf "  export GITHUB_LOGIN_CLIENT_ID=<your id>\n")
(printf "  export GITHUB_LOGIN_CLIENT_SECRET=<your secret>\n")
(printf "  CONFIG=local-dev ./run\n")
(printf "\nThen visit http://localhost:7080\n")
