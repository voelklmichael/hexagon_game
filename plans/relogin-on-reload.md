# Plan: Auto-relogin on Page Reload

## Problem

The backend uses `axum-login` + `tower-sessions` with cookie-based sessions. After a browser
reload the session cookie is still present and the backend session is still valid — but the
frontend starts fresh with `logged_in_as: None` (field is `#[serde(skip)]`), so it shows the
user as logged out even though they aren't.

## Solution

Add a `GET /user_login/whoami` endpoint that the frontend calls once on startup. If the browser's
session cookie is still valid the backend returns the current user; the frontend restores its
logged-in state without asking for a password.

---

## Steps

### 1 — Backend: add `WhoAmI` struct and `GET /whoami` route

**File:** `hexagon_backend/src/user/user_login_routes.rs`

- Add a `#[derive(Serialize, Deserialize)]` struct:
  ```rust
  pub struct WhoAmI {
      pub user_id: Uuid,
      pub username: String,
  }
  ```
- Add `.route("/whoami", get(self::get::whoami))` to `login_router()`.
- Add the handler in `mod get`:
  ```rust
  pub async fn whoami(auth_session: AuthSession) -> Result<Json<WhoAmI>, StatusCode> {
      match auth_session.user {
          Some(user) => Ok(Json(WhoAmI { user_id: user.id(), username: user.username })),
          None => Err(StatusCode::UNAUTHORIZED),
      }
  }
  ```
  (`user.username` is a private field on `User` — expose it via a getter or make the field
  `pub(super)` as needed.)

---

### 2 — Frontend reqwest client: add `WhoAmI` + `whoami()` method

**File:** `hexagon_frontend_reqwest/src/lib.rs`

- Mirror the struct (Deserialize only):
  ```rust
  #[derive(Debug, serde::Deserialize)]
  pub struct WhoAmI {
      pub user_id: Uuid,
      pub username: String,
  }
  ```
- Add the method to `ApiClient`:
  ```rust
  pub async fn whoami(&self) -> Result<Option<WhoAmI>, ApiError> {
      let res = self.client.get(self.url("/user_login/whoami")).send().await?;
      if res.status() == reqwest::StatusCode::UNAUTHORIZED {
          return Ok(None);
      }
      if res.status().is_success() {
          return Ok(Some(res.json().await?));
      }
      let code = res.status().as_u16();
      let body = res.text().await.unwrap_or_default();
      Err(ApiError::Status { status: code, body })
  }
  ```

---

### 3 — `BackendReqwest`: add `whoami_task` and `check_session()`

**File:** `hexagon_egui/src/app/backend_reqwest.rs`

- Add two fields:
  ```rust
  pub whoami_task: Bind<Option<hexagon_frontend_reqwest::WhoAmI>, ApiError>,
  session_checked: bool,
  ```
- Add the method (the `session_checked` guard ensures it only fires once per page load):
  ```rust
  pub fn check_session(&mut self) {
      if self.session_checked { return; }
      self.session_checked = true;
      let Some(client) = self.get_client() else { return };
      self.whoami_task.request(async move { client.whoami().await });
  }
  ```

---

### 4 — `UserLogin`: add `user_id` field

**File:** `hexagon_egui/src/panels/user_login.rs`

- Add to the struct (skipped from persistence — always restored via whoami):
  ```rust
  #[serde(skip)]
  pub user_id: Option<uuid::Uuid>,
  ```
  This unblocks the already-staged reference in `app.rs:417`.

- Also set `user_id` when a manual login succeeds:
  ```rust
  // in the login_user_task result handler
  Ok(info) => {
      user.logged_in_as = Some(info.username.clone());
      user.user_id = Some(info.user_id);
  }
  ```
  (Requires `login_user_task` to return `WhoAmI` instead of just `String` — adjust
  `BackendReqwest::log_in` and the `ApiClient::login` return type accordingly, or keep them
  separate and do a follow-up whoami after a fresh login.)

---

### 5 — `user_login::show`: drive the check and handle the result

**File:** `hexagon_egui/src/panels/user_login.rs`

At the top of `show()`, before any UI:

```rust
// Kick off the one-time session check (no-op after first call).
client.check_session();

// Handle whoami result.
if let Some(result) = client.whoami_task.take() {
    match result {
        Ok(Some(info)) => {
            user.logged_in_as = Some(info.username);
            user.user_id = Some(info.user_id);
        }
        Ok(None) => {} // not logged in — show the form as normal
        Err(e) => tracing::warn!("Session check failed: {e}"),
    }
}
```

---

## Caveat: server restarts invalidate sessions

The backend uses `MemoryStore` (in-memory session store). Sessions are lost on server restart.
After a restart `whoami` returns `401`, the frontend gracefully shows "not logged in", and the
user must log in again. This is expected. Persisting sessions across restarts would require a
DB-backed session store — a separate change.
