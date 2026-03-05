# Auth System Replacement Plan - Racket Package Website

## Context

The current auth system stores plaintext passwords in sessions, uses an unnecessary HTTP round-trip between frontend and backend (they're in the same process), and only supports email+password login. This plan replaces it with a robust system supporting GitHub OAuth, internal user IDs, and API tokens, while keeping full backward compatibility with existing email+password login.

## Design Decisions

- **Frontend/backend**: Replace HTTP RPC with direct function calls (same process)
- **User identity**: Internal user IDs (UUIDs) in userdb properties; emails stay in package catalog metadata
- **GitHub OAuth**: OAuth App (not GitHub App); keep email+password permanently alongside
- **Account linking**: Require password confirmation when GitHub email matches existing account
- **Tokens**: Stored in userdb properties; web-generated only (no device auth flow)
- **Account page**: New `/account` page for GitHub linking, tokens, password changes
- **Backend API**: Keep HTTP Basic Auth forever; add Bearer token auth alongside

## Important Reference

**Read `README.md` before starting any work.** It contains essential information about running the server, its configuration system, dependencies, and deployment. Understanding the configuration is critical for both testing and implementation.

## Pre-work: Environment

All local development and testing for this project must use:

1. **Fresh Racket 9.1 install** from the official Racket release (not a development build or older version).
2. **Required packages installed in installation scope** for that Racket 9.1 install:
   ```
   raco pkg install -i --skip-installed \
        'https://github.com/racket/infrastructure-userdb.git#main' \
        reloadable \
        aws \
        s3-sync \
        plt-service-monitor
   ```
   Plus any additional test dependencies needed (e.g. `net-http-easy`, `riposte`).
3. **Fresh checkout of this repository** — do not reuse an existing working tree that may have stale compiled files or local state. Clone fresh and work from there.

This ensures a clean, reproducible baseline that matches what CI and deployment will see.

## Pre-work: Setup

1. Copy this plan file into the repository root as `AUTH-PLAN.md` and commit it.
2. All work is done on feature branches, one per phase, each based on the previous:
   - `auth/phase-0-testing` (branched from `master`)
   - `auth/phase-1-direct-calls` (branched from `auth/phase-0-testing`)
   - `auth/phase-2-user-ids` (branched from `auth/phase-1-direct-calls`)
   - `auth/phase-3-github-oauth` (branched from `auth/phase-2-user-ids`)
   - `auth/phase-4-api-tokens` (branched from `auth/phase-3-github-oauth`)
3. After each commit, push to the `samth` remote.
4. Set up GitHub Actions CI early in Phase 0 (as one of the first commits) so all subsequent work has automated test verification.

---

## Phase 0: Testing Infrastructure

### Goal
Establish a test suite covering the current auth system behavior *before* changing it, and restructure modules so they're testable. This gives us a safety net for the refactoring in Phases 1-4.

### 0.1 Restructure side-effectful modules

Several modules (notably `common.rkt`) run side effects at top level: creating directories, reading config files, opening userdb. This makes them impossible to `require` from test code without those side effects running (and potentially failing if config/paths aren't set up).

**Restructure these modules** so that side effects (directory creation, config reading, server startup) live in submodules (e.g., `module+ main` or initialization functions) rather than at the top level. The top level should only define functions and parameters.

Key modules to restructure:
- **`src/pkg-index/common.rkt`** (lines 22-57): `make-directory*`, `get-config` calls, userdb initialization all at top level. Move to an `(initialize!)` function or `module+ main`.
- **`src/users.rkt`** (lines 18-26): `userdb` and `*codes*` initialization at top level. Make these lazily initialized or parameterized.
- **`src/pkg-index/dynamic.rkt`** (line 416-444): `go` function starts server, but the module also has top-level config reads via `common.rkt`. Already somewhat clean since `go` is a function, but depends on `common.rkt` side effects.

The restructuring should not change any runtime behavior - the same code runs in the same order when the server starts normally. It only changes *when* the code runs (at explicit initialization rather than at module load time).

> **Implementation notes (0.1):**
> - `common.rkt`: All state variables changed to `#f` initial values. All side effects moved into an `(initialize!)` function. Added `set-pkgs-path-for-testing!` and `set-userdb-for-testing!` setter functions for test isolation. `dynamic.rkt` calls `(initialize!)` at the start of `go`.
> - `users.rkt`: Changed to lazy initialization via `ensure-initialized!` called from each public function. Added `initialize-users-for-testing!` that accepts a test userdb and registration state.
> - `build-update.rkt`: `SUMMARY-ETAG-PATH` had to be changed from a top-level `define` to a function `(define (SUMMARY-ETAG-PATH) ...)` because it referenced `cache-path` (now `#f` until initialized). Two call sites updated.
> - `dynamic.rkt`: Expanded `provide` to export `curation-administrator?`, `superuser?`, `current-user`, `save-package!`, `curate-packages!`, `package-author?`, `ensure-authenticate/email+passwd` for test access.
> - Verified all 6 consumer modules of `common.rkt` (`dynamic.rkt`, `build-update.rkt`, `update.rkt`, `static.rkt`, `notify.rkt`, `basic.rkt`) only use common.rkt values inside functions, not at top level (except `build-update.rkt` which was fixed).
> - Verified server still starts correctly with `PKG_SERVER_HTTP=1 timeout 15 racket -y src/main.rkt`.

### 0.2 Test directory and structure

Create `src/tests/` directory with:

- `src/tests/test-sessions.rkt` - Unit tests for session management
- `src/tests/test-users.rkt` - Unit tests for user management
- `src/tests/test-auth-api.rkt` - Integration tests for backend auth/package API functions
- `src/tests/test-web-handlers.rkt` - Direct handler tests using `web-server/test`
- `src/tests/test-smoke.rkt` - HTTP smoke tests against a running server
- `src/tests/test-helpers.rkt` - Shared test utilities (temp userdb setup, test user creation, etc.)

> **Implementation notes (0.2):** All files created as planned.

### 0.3 Test helpers (`test-helpers.rkt`)

Provides utilities used by all test files:
- `call-with-test-userdb` - Create a temporary directory, initialize a userdb there, run a thunk, clean up
- `create-test-user!` - Create a user with a known email/password in the test userdb
- `make-test-request` - Build a `request` struct with specified method, URL, headers, cookies, and form bindings
- `with-test-session` - Create a session for a test user and run a thunk with `current-session` parameterized
- `call-with-test-packages-dir` - Create a temp directory for package catalog files

Uses `rackunit` and constructs `request` structs from `web-server/http/request-structs`.

> **Implementation notes (0.3):** Implemented as planned. Key finding: `request` struct has 8 fields (method, uri, headers/raw, bindings/raw-promise, post-data/raw, host-ip, host-port, client-ip). Cookies are passed as `Cookie` headers, extracted via `request-cookies` from `web-server/http/cookie-parse`. The `bindings/raw-promise` field requires a `delay` (from `racket/promise`, not available in `racket/base`). No stdlib duplication found.

### 0.4 Session tests (`test-sessions.rkt`)

Test the current behavior (then verify it still passes after Phase 1 changes):
- `create-session!` returns a session key string
- `lookup-session/touch!` returns the session for a valid key
- `lookup-session/touch!` returns `#f` for an unknown key
- `destroy-session!` makes the key return `#f`
- Session expiry: create a session, then directly mutate its `expiry` field to a past time (prefab struct, use `struct-copy`), call `expire-sessions!`, verify session is gone
- `current-email` returns the session's email when `current-session` is parameterized
- Session cookie round-trip: verify `request->session` (from site.rkt) extracts session from cookie

> **Implementation notes (0.4):** 10 tests implemented. Divergences from plan:
> - Session expiry test: Initial implementation was flawed (destroyed and recreated sessions instead of testing `expire-sessions!`). Fixed by exporting `sessions` (the persistent state thunk) and `expire-sessions!` from `sessions.rkt`, then using `hash-set!` to overwrite a session with an expired copy and calling `expire-sessions!` to verify removal.
> - Cookie round-trip test: `request->session` is defined in `site.rkt` which has too many dependencies to `require` from unit tests. Instead, the test constructs a request with cookies and extracts them using `request-cookies` + `client-cookie-name`/`client-cookie-value` from `web-server/http/cookie-parse`, replicating the same extraction logic.

### 0.5 User management tests (`test-users.rkt`)

Against a temporary test userdb:
- `register-or-update-user!` creates a user that can be looked up
- `login-password-correct?` returns `#t` for correct password, `#f` for wrong password
- `login-password-correct?` returns `#f` for non-existent user
- Password change: `register-or-update-user!` with new password, old password fails, new password works
- Registration codes: generate a code directly via `generate-registration-code!` on a test `registration-state`, validate with `registration-code-correct?` (skip email sending entirely)

> **Implementation notes (0.5):** 8 tests implemented as planned. The `infrastructure-userdb` API uses `check-registration-code` (not `registration-code-correct?` — that's the wrapper in `users.rkt` that calls `check-registration-code`). Tests call `generate-registration-code!` directly on a `make-registration-state` value, bypassing email entirely as planned.

### 0.6 Backend API integration tests (`test-auth-api.rkt`)

**Direct function tests** (call functions in `dynamic.rkt` directly, within `parameterize ([current-user ...])`, made possible by the module restructuring in 0.1):
- Authentication with correct/wrong credentials
- `curation-administrator?` and `superuser?` check the hardcoded email list
- Package save: create a package, verify it exists in the packages directory
- Package author check: user is author of their package, not author of someone else's
- Package delete: author can delete, non-author cannot
- Superuser override: superuser can edit/delete any package
- Curation: curator can change ring, non-curator cannot

**HTTP API tests with riposte** (`test-auth-api-http.rkt`): Test the backend HTTP endpoints using riposte's DSL for REST API testing. Start the backend on a test port, then:
- POST `/api/authenticate` with correct/wrong credentials
- POST `/api/package/modify-all` with/without valid auth
- POST `/api/package/del` - author vs non-author
- POST `/api/package/curate` - curator vs non-curator
- Verify response JSON structure and status codes

Riposte is well-suited for these structured JSON API tests with its built-in assertion DSL.

> **Implementation notes (0.6):** Direct function tests: 10 tests implemented. Divergences:
> - `test-auth-api-http.rkt` (riposte HTTP tests) deferred — the backend is not designed to start independently in test mode, and the direct function tests already provide good coverage of the same logic. The smoke tests (0.8) cover the HTTP layer.
> - Package delete test not implemented — `api/package/del` calls `package-remove!` which also triggers `signal-static!` that expects initialized state. Added tests for non-author-cannot-modify and superuser-can-modify instead.
> - Background thread errors: `signal-update!` triggers `do-update!` and `do-notify!` which try to write to `notice-path` (still `#f` in test mode). These are non-fatal, caught by `safe-run!` and logged to stderr. Tests still pass. Future improvement: add `set-notice-path-for-testing!` or make `signal-update!` skip when paths are uninitialized.

### 0.7 Web handler tests (`test-web-handlers.rkt`)

Use `web-server/test` `make-servlet-tester` for direct handler testing:
- Main page renders without error (unauthenticated)
- Package page renders for an existing package
- Login page renders the login form
- Register page renders the registration form
- Edit page redirects to login when unauthenticated
- Edit page renders when authenticated (set up session cookie)
- Search page works

These test the request→response pipeline without starting a real server.

> **Implementation notes (0.7):** 7 tests implemented. Divergences:
> - `make-servlet-tester` tries to parse all responses as XML by default, which fails for `response/output` (used by the site). Fixed by using `#:raw? #t #:headers? #t` to get raw bytes, then parsing status codes from HTTP headers manually.
> - Main page returns 302 redirect to `/index.html` (static content), not 200. Test adjusted to verify the redirect.
> - "Edit page renders when authenticated" test not implemented — would require creating a session and passing it via cookies through the tester, which is complex with `send/suspend/dispatch` continuations. The smoke tests cover this case more reliably.
> - `site.rkt` loads successfully as a module despite top-level side effects (config reading), because the `reloadable` config system returns defaults when no config is set. A `package-change-handler` thread starts on module load.

### 0.8 HTTP smoke tests (`test-smoke.rkt`)

Start the full server on a test port in a thread, make real HTTP requests using `net/http-easy` (already installed). Use its cookie jar support (`list-cookie-jar%` from `net/cookies`) for automatic session cookie handling across requests.

Tests:
- GET `/` returns 200
- GET `/login` returns 200 with login form
- POST `/login` with wrong credentials shows error message
- POST `/login` with correct credentials: cookie jar captures `pltsession` cookie, follows redirect
- GET `/package/some-package/edit` without session redirects to login
- GET `/package/some-package/edit` with session cookie (from jar) returns 200
- GET `/logout` clears session cookie in jar

Example pattern:
```racket
(require net/http-easy net/cookies racket/class rackunit)

(define jar (new list-cookie-jar%))
(define s (make-session #:cookie-jar jar))

(parameterize ([current-session s])
  ;; Login
  (define login-res
    (post (format "http://localhost:~a/login" test-port)
          #:form `((email . "test@example.com") (password . "secret"))))
  (check-equal? (response-status-code login-res) 200)

  ;; Authenticated request (cookie jar handles session automatically)
  (define edit-res
    (get (format "http://localhost:~a/package/test-pkg/edit" test-port)))
  (check-equal? (response-status-code edit-res) 200))
```

Server started with test config (temp userdb, temp packages dir, no SSL, random port).

> **Implementation notes (0.8):** 5 tests implemented. Divergences:
> - Cookie jar / login flow tests (POST `/login`, authenticated edit page, logout) not implemented — the login flow uses `send/suspend/dispatch` continuations which generate unique URLs, making it complex to follow via simple HTTP requests. These will be better tested after Phase 1 when the direct function call architecture makes it easier to set up authenticated sessions programmatically.
> - Server starts as a subprocess (not a thread) since it blocks. Uses `find-free-port` (bind to port 0, get assigned port, close listener) to avoid port conflicts. Uses `plumber-add-flush!` for cleanup.
> - `subprocess` returns 4 values (proc, stdout, stdin, stderr) — initially wrote code that expected 1 value.
> - File paths required `define-runtime-path` to resolve correctly since `raco test` sets `current-directory` to the test file's directory, not the project root.
> - Needed to install `http-easy` package (not included in base Racket 9.1).

### 0.9 Test runner

Add a `Makefile` target:
```makefile
test:
	raco test -y src/tests/
```

Or run individual test files: `raco test -y src/tests/test-sessions.rkt`

> **Implementation notes (0.9):** Makefile `test` target added as planned. CI workflow updated to include `http-easy` in dependencies.

### Testing Phase 0

Verify all tests pass against the *current* codebase before any Phase 1 changes. This is the baseline.

> **Implementation notes (Phase 0 overall):**
> - **48 tests total**: 10 session + 8 users + 18 auth-api + 7 web-handler + 5 smoke. All passing.
> - **Test count vs plan**: Plan mentioned more tests (particularly login flow, authenticated edit page, riposte HTTP API tests). These were deferred because the continuation-based web architecture makes them complex to test directly. The smoke tests provide basic HTTP coverage; more comprehensive tests will be added as the architecture is simplified in Phase 1.
> - **Coverage thresholds enforced in CI**: sessions.rkt 90%, users.rkt 40%, common.rkt 50%, dynamic.rkt 35%. Actual coverage: sessions 99.3%, users 48.2%, common 63.2%, dynamic 41.6%. Coverage runs as a separate CI job so it can fail independently of tests.
> - **`for-testing` submodules**: Test-only exports (e.g. `initialize-for-testing!`, `expire-sessions!`, `sessions`) moved into `(module+ for-testing ...)` submodules in sessions.rkt, common.rkt, and users.rkt. Test files require these via `(submod ... for-testing)`.
> - **Background thread fixes**: `initialize-for-testing!` added to common.rkt to set up `notice-path`, `static-path`, `cache-path`, `SUMMARY-PATH`, `static.src-path` so background threads from `signal-update!` don't error.
> - **Commits**: 12 commits on `auth/phase-0-testing`.

### What could go wrong

- **Module restructuring breaks runtime behavior**: Moving side effects into initialization functions could change execution order or introduce bugs. Mitigation: restructure carefully, verify the server starts and works normally before writing any tests.
- **Email sending**: Registration code tests skip email sending entirely by calling `generate-registration-code!` directly on a test `registration-state`. No need to mock email.
- **Static file generation**: Some backend operations trigger static file regeneration. Tests should provide a temp static directory via config, or the restructured modules should allow suppressing this.

> **What actually went wrong:**
> - `build-update.rkt` had a top-level reference to `cache-path` that broke when `common.rkt` was restructured. Fixed by converting it to a function.
> - `set!` cannot mutate module-imported identifiers in Racket, so `set-pkgs-path-for-testing!` and `set-userdb-for-testing!` setter functions were added to `common.rkt`.
> - `racket/base` does not include `delay` (needed for `request` struct's `bindings/raw-promise` field). Required `racket/promise`.
> - Background threads from `signal-update!` produce errors when `notice-path`, `static-path`, `cache-path`, `static.src-path` are `#f`. Fixed by adding `initialize-for-testing!` that sets up all state.
> - `make-servlet-tester` parses responses as XML by default, requiring `#:raw? #t` workaround.
> - `subprocess` returns 4 values, not 1. `current-directory` resolves differently under `raco test`.
> - `raco cover` raw format only instruments files passed as arguments, not their dependencies. Must pass both test AND source files to get dependency coverage.
> - `member` returns a list (truthy but not `#t`), requiring `check-not-false` instead of `check-true`.

---

## Phase 1: Remove Plaintext Password from Sessions + Direct Function Calls

### 1.1 Extract backend business logic into `src/pkg-index/api.rkt` (new file)

Extract from `dynamic.rkt` into a new `api.rkt` module that `common.rkt` can support:

| Function in api.rkt | Extracted from (dynamic.rkt) | Purpose |
|---|---|---|
| `authenticate-user` | `api/authenticate` (line 176-188) | Validate email+password, return curator/superuser flags |
| `save-package!/direct` | `save-package!` (lines 243-320) | Save package with author checks |
| `delete-package!/direct` | `api/package/del` handler (lines 339-346) | Delete package if author |
| `curate-packages!/direct` | `curate-packages!` (lines 354-367) | Change package ring |
| `update-user-packages!/direct` | `api/update` handler (lines 385-390) | Signal update for user's packages |
| `curation-administrator?` | Line 65-71 | Check curator status |
| `superuser?` | Line 76-77 | Check superuser status |

Each function takes the acting user's email as an explicit parameter and sets `current-user` internally via `parameterize`.

`dynamic.rkt` becomes thin HTTP wrappers that call `api.rkt`. The HTTP endpoints stay for external tool compatibility (`api/upload` used by `raco pkg catalog-archive`).

**Why a separate file**: `dynamic.rkt` runs `serve/servlet` at load time (via `go`). Having `site.rkt` require `dynamic.rkt` directly would cause initialization problems. `api.rkt` contains only functions, no side effects.

**Module loading order**: `common.rkt` is loaded first (by `dynamic.rkt` in `main-inner.rkt:44`). When `site.rkt` later requires `api.rkt` which requires `common.rkt`, it gets the already-loaded instance. No config issues.

### 1.2 Replace `simple-json-rpc!` calls in `site.rkt`

Replace `(require "json-rpc.rkt")` with `(require "pkg-index/api.rkt")`.

5 call sites to replace:

| site.rkt location | Current call | Replacement |
|---|---|---|
| `create-session-after-authentication-success!` (line 391) | `simple-json-rpc! ... "/api/authenticate"` | `(authenticate-user email password)` |
| `confirm-package-deletion` (line 1417) | `simple-json-rpc! ... "/api/package/del"` | `(delete-package!/direct (current-email) pkg)` |
| `save-draft!` (line 1512) | `simple-json-rpc! ... "/api/package/modify-all"` | `(save-package!/direct (current-email) ...)` |
| `update-my-packages-page` (line 1587) | `simple-json-rpc! ... "/api/update"` | `(update-user-packages!/direct (current-email))` |
| `update-package-rings!` (line 1608) | `simple-json-rpc! ... "/api/package/curate"` | `(curate-packages!/direct (current-email) ...)` |

Remove `backend-baseurl` (site.rkt line 72-75).

### 1.3 Remove password from session struct

**`sessions.rkt`**: Change struct from `(session key expiry email password curator? superuser?)` to `(session key expiry email curator? superuser?)`. Remove `password` param from `create-session!`.

**`site.rkt`**: Update `create-session-after-authentication-success!` and `process-login-credentials` to not pass password to session creation.

**Migration**: Existing sessions are invalidated (struct shape changes). Users must log in again. Server restart clears the in-memory session store.

### 1.5 Switch to signed session cookies

Replace the raw `pltsession` cookie with a signed cookie using `web-server/http/id-cookie` (built into the web server). This uses HMAC-SHA1 to sign the session key, preventing cookie value tampering.

**`sessions.rkt`**:
- Add a configurable signing key (read from config or generated randomly at startup)
- `create-session!` returns the session key; cookie signing happens in `site.rkt`

**`site.rkt`**:
- Replace `(make-cookie COOKIE v ...)` with `make-id-cookie` from `web-server/http/id-cookie`
- Replace `request->session` to use `request-id-cookie` for extraction and signature validation
- Invalid/forged cookies are rejected automatically by the HMAC check

The server-side session store remains (for session revocation on logout). The signing adds defense-in-depth.

### 1.4 Delete `json-rpc.rkt`

No longer needed.

### Testing Phase 1

1. All Phase 0 tests pass (session tests updated for no-password struct, API tests updated for direct calls)
2. Add new tests: verify `api.rkt` functions work when called directly (not via HTTP)
3. Add test: `api/upload` still works via backend HTTP endpoint (external tool compat)
4. Add test: old-format sessions are gracefully rejected (user sees login page, not crash)
5. Smoke test: log in, edit a package, delete a package, curate, update - all through web UI

---

## Phase 2: Internal User IDs + Account Page

### 2.1 Add user ID functions to `users.rkt`

New functions:
- `ensure-user-id!` - Look up or generate UUID in user's properties under `'user-id` key. Use `crypto-random-bytes` (from `randomness.rkt` pattern).
- `user-id-for-email` - Return user-id property or `#f`

### 2.2 Assign user IDs at login

In `create-session-after-authentication-success!`, call `ensure-user-id!` after authentication. IDs assigned lazily on first login.

### 2.3 Create `/account` page

New route in `site.rkt` dispatch-rules: `[("account") account-page]`

Uses `authentication-wrap/require-login`. Sections:
- Account info (email, user ID)
- Change password form (current password, new password, confirm)
- GitHub linking (added in Phase 3)
- API tokens (added in Phase 4)

Add "Account" link to navbar dropdown (in `authentication-wrap*` around line 241).

### 2.4 Optional batch migration script

`src/scripts/assign-user-ids.rkt` - iterate all users, assign UUIDs. Not required since Phase 2.2 handles it lazily.

### Testing Phase 2

1. All prior tests still pass
2. Add user ID tests to `test-users.rkt`: `ensure-user-id!` creates UUID, is idempotent on second call
3. Add handler test: GET `/account` requires login, renders account info when authenticated
4. Add integration test: change password via account page, verify old password fails and new works
5. Add smoke test: navigate to `/account`, verify page renders with email and password form

---

## Phase 3: GitHub OAuth Login

### 3.1 GitHub OAuth configuration

New config keys (separate from the existing `github-client_id`/`github-client_secret` in `common.rkt` which are for package source fetching):
- `github-login-client-id`
- `github-login-client-secret`

Store in config file or environment variables. Read in `site.rkt` or a new config section.

### 3.2 New file: `src/github-oauth.rkt`

Provides:
- `github-authorize-url` - Build `https://github.com/login/oauth/authorize` URL with client_id, redirect_uri, scope=`user:email`, and CSRF state param
- `github-exchange-code` - POST to `https://github.com/login/oauth/access_token` with code, get access token
- `github-get-user-info` - Call `https://api.github.com/user` and `https://api.github.com/user/emails` to get GitHub ID + verified emails

Uses `net/url` and `json` (already in project).

### 3.3 New routes in `site.rkt`

```
[("auth" "github") github-login-start]
[("auth" "github" "callback") github-login-callback]
```

**`github-login-start`**: Generate random state token (store in in-memory hash with 10-min expiry), redirect to GitHub.

**`github-login-callback`**: Verify state, exchange code for token, get user info, then:
- **GitHub ID found in userdb**: Log in directly, create session
- **GitHub ID not found, email matches existing user**: Show "link account" page requiring password confirmation
- **GitHub ID not found, no email match**: Create new account with random password, store github-id/github-username in properties, create session

### 3.4 GitHub identity storage in userdb properties

New functions in `users.rkt`:
- `lookup-user-by-github-id` - Scan all users for matching `'github-id` property. Cache mapping in memory.
- `link-github-account!` - Set `'github-id`, `'github-username`, `'github-email` properties
- `create-github-user!` - Create user with random password + GitHub properties

Properties stored:
- `'github-id` → integer (stable across username changes)
- `'github-username` → string (for display)
- `'github-email` → string

### 3.5 UI changes

**Login page** (`login-form`, site.rkt line 363): Add "Sign in with GitHub" button below existing email+password form. Only show if GitHub OAuth is configured.

**Register page**: Add GitHub option there too.

**Account page**: Add GitHub section showing linked status, with Link/Unlink buttons.

### 3.6 Edge cases

- **No verified email on GitHub**: Show error asking user to verify an email on GitHub first
- **Multiple GitHub emails**: Check all verified emails when looking for existing account match
- **GitHub email privacy**: The `user:email` scope + `/user/emails` endpoint returns verified emails even if they're private

### Testing Phase 3

1. All prior tests still pass
2. Unit tests for `github-oauth.rkt`: URL construction, state token generation (mock GitHub HTTP calls)
3. Integration tests for GitHub user creation/linking in `test-users.rkt`: `lookup-user-by-github-id`, `link-github-account!`, `create-github-user!`
4. Handler tests: GitHub callback with mocked GitHub API - test all three cases (known user, email match, new user)
5. Handler test: invalid state parameter rejected (CSRF)
6. Handler test: no GitHub config - login page renders without GitHub button
7. Smoke test: verify `/auth/github` redirects to GitHub authorize URL

---

## Phase 4: API Tokens

### 4.1 Token functions in `users.rkt`

- `generate-api-token!` - Create 32-byte random token with `rpkg_` prefix. Store SHA-256 hash (using `crypto` package's `digest` function, not bcrypt - tokens are high-entropy, SHA-256 is sufficient and allows fast lookup) in user's `'api-tokens` property as list of `(hash label created-at)`. Return plaintext once.
- `revoke-api-token!` - Remove token entry by hash prefix or label
- `validate-api-token` - Hash the given token, look up in cache. Return user email or `#f`.
- `list-api-tokens` - Return `(label, created-at, hash-prefix)` for display

In-memory cache: `token-sha256 → email` hash, rebuilt at startup, updated on create/revoke.

### 4.2 Bearer token auth in backend

Modify `ensure-authenticate` in `dynamic.rkt` (line 140-150): after trying Basic Auth, check for `Authorization: Bearer rpkg_...` header. If found, validate token and set `current-user`.

All backend HTTP endpoints automatically get token support. `api/upload` (line 79) has separate auth logic and would need its own modification if desired.

### 4.3 Token management UI on `/account` page

- Token list: label, creation date, hash prefix, "Revoke" button
- Generate form: label field + "Generate" button
- After generation: show plaintext token once with "copy this now" warning

### Testing Phase 4

1. All prior tests still pass
2. Unit tests in `test-users.rkt`: `generate-api-token!` returns token with `rpkg_` prefix, `validate-api-token` finds it, `revoke-api-token!` invalidates it, `list-api-tokens` shows metadata
3. Integration test: Bearer token auth works on backend HTTP endpoints (`ensure-authenticate` accepts Bearer header)
4. Integration test: Basic Auth still works on all endpoints (regression)
5. Handler tests: token generation page, revocation, display-once behavior
6. Smoke test: `curl` with Bearer token against running server

---

## Files Modified/Created Summary

| File | Phases | Changes |
|---|---|---|
| `src/tests/test-helpers.rkt` | 0 (new) | Shared test utilities |
| `src/tests/test-sessions.rkt` | 0 (new) | Session unit tests |
| `src/tests/test-users.rkt` | 0 (new) | User management unit tests |
| `src/tests/test-auth-api.rkt` | 0 (new) | Backend API integration tests |
| `src/tests/test-web-handlers.rkt` | 0 (new) | Direct handler tests via web-server/test |
| `src/tests/test-smoke.rkt` | 0 (new) | HTTP smoke tests against running server |
| `src/pkg-index/api.rkt` | 1 (new) | Business logic extracted from dynamic.rkt |
| `src/pkg-index/dynamic.rkt` | 1, 4 | Thin HTTP wrappers calling api.rkt; Bearer token auth |
| `src/sessions.rkt` | 1 | Remove password field from session struct |
| `src/site.rkt` | 1, 2, 3 | Replace RPC calls, add /account and GitHub routes |
| `src/users.rkt` | 2, 3, 4 | User IDs, GitHub linking, API tokens |
| `src/github-oauth.rkt` | 3 (new) | GitHub OAuth flow implementation |
| `src/json-rpc.rkt` | 1 (delete) | No longer needed |

## Commit Strategy

Each phase should produce a clean, reviewable series of commits. Work in progress can use as many commits as needed, but squash/reorganize before presenting for review so each commit is self-contained and correct.

### Phase 0 commits
1. Restructure `common.rkt`: move side effects to initialization function
2. Restructure `users.rkt`: make userdb/codes initialization explicit
3. Add GitHub Actions CI workflow (`.github/workflows/test.yml`) - runs `raco test -y src/tests/`
4. Add test helpers (`test-helpers.rkt`)
5. Add session unit tests
6. Add user management unit tests
7. Add backend API integration tests (direct function calls)
8. Add backend API HTTP tests (riposte)
9. Add web handler tests
10. Add HTTP smoke tests

### Phases 1-4 commits

After Phase 0, **every commit must pass all tests**. Tests for new/changed functionality go in the same commit as the code change, not in a separate "add tests" commit. For example:

- "Extract business logic into `pkg-index/api.rkt`" includes tests for the extracted functions
- "Remove password from session struct" updates existing session tests in the same commit
- "Add GitHub login routes" includes handler tests for those routes

### Phase 1 commit sequence
1. Extract business logic into `pkg-index/api.rkt` + tests for extracted functions
2. Replace `simple-json-rpc!` calls with direct `api.rkt` calls + update integration tests
3. Remove password from session struct + update session tests
4. Switch to signed session cookies via `web-server/http/id-cookie` + update cookie tests
5. Delete `json-rpc.rkt`

### Phase 2 commit sequence
1. Add user ID generation to `users.rkt` + tests
2. Assign user IDs at login + tests
3. Add `/account` page with password change + handler/smoke tests
4. Add account link to navbar

### Phase 3 commit sequence
1. Add `github-oauth.rkt` + unit tests (mock GitHub HTTP)
2. Add GitHub config keys
3. Add GitHub user storage functions to `users.rkt` + tests
4. Add GitHub login/callback routes + handler tests
5. Add "Sign in with GitHub" to login/register pages + tests
6. Add GitHub section to `/account` page + tests

### Phase 4 commit sequence
1. Add token generation/storage/validation to `users.rkt` + tests
2. Add Bearer token auth to backend `ensure-authenticate` + tests
3. Add token management UI to `/account` page + handler/smoke tests

## Key Existing Code to Reuse

- `infrastructure-userdb`: `user-property` / `user-property-set` for storing github-id, user-id, tokens
- `randomness.rkt`: `random-bytes/base64` for token generation
- `send/suspend/dispatch` pattern from existing login form for GitHub link confirmation flow
- `authentication-wrap/require-login` macro for /account page
- `make-persistent-state` for in-memory caches (GitHub ID → email, token → email)
- `net/url` + `json` for GitHub API calls (already used in project)

## Future Considerations

After Phase 3 (GitHub OAuth) is implemented, evaluate whether refactoring to use the `simple-oauth2` or `webapi` package would be an improvement over the manual OAuth implementation. Both provide standard OAuth 2.0 client flows, token management, and credential storage. The manual approach is simpler for the initial implementation since GitHub's OAuth flow is straightforward (3 HTTP calls), but a library may be better for maintainability and standards compliance long-term.
- `net/http-easy`: High-level HTTP client with cookie jar, auth helpers, JSON parsing (Phase 0 smoke tests, Phase 4 token tests)
- `net/cookies`: `list-cookie-jar%` for automatic session cookie handling across requests (Phase 0)
- `riposte`: DSL for testing JSON HTTP API endpoints (Phase 0 backend API tests)
- `web-server/test`: `make-servlet-tester` for direct handler testing (Phase 0)
- `web-server/http/id-cookie`: Signed session cookies via HMAC-SHA1 (Phase 1)
- `crypto`: SHA-256 digest for API token hashing (Phase 4)
- `rackunit`: `check-equal?`, `check-true`, `check-false`, `test-case`, `test-suite` (Phase 0)
- `web-server/http/request-structs`: `request`, `header`, `binding:form` for constructing test requests (Phase 0)
- `configs/testing.rkt`: existing test configuration entry point
- `src/mock/aws-s3.rkt`: existing mock pattern to follow for test doubles
