mod user_login_routes;
use axum_login::{AuthUser, AuthnBackend, UserId};
use hexagon_db::{DBUserPassword, ExposeSecret, SecretString, SqlxError, Uuid};
use password_auth::verify_password;
use serde::Deserialize;
use tokio::task;
pub use user_login_routes::login_router;

use crate::routes::AppState;

#[derive(Debug, Clone, Deserialize)]
pub struct User {
    id: Uuid,
    username: String,
    password_hash: SecretString,
}

impl AuthUser for User {
    type Id = Uuid;

    fn id(&self) -> Self::Id {
        self.id
    }

    fn session_auth_hash(&self) -> &[u8] {
        self.password_hash.expose_secret().as_bytes()
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct Credentials {
    pub username: String,
    pub password: String,
    pub next: Option<String>,
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Sqlx(#[from] SqlxError),
    #[error(transparent)]
    TaskJoin(#[from] task::JoinError),
}

impl AuthnBackend for AppState {
    type User = User;
    type Credentials = Credentials;
    type Error = Error;

    async fn authenticate(
        &self,
        creds: Self::Credentials,
    ) -> Result<Option<Self::User>, Self::Error> {
        tracing::info!("User auth for username: {}", &creds.username);
        let Some(user) = self.db.fetch_user_password_by_name(&creds.username).await? else {
            return Ok(None);
        };
        let DBUserPassword {
            id,
            user_name,
            password_hash,
        } = user;

        // Verifying the password is blocking and potentially slow, so we'll do so via
        // `spawn_blocking`.
        task::spawn_blocking(move || {
            // We're using password-based authentication--this works by comparing our form
            // input with an argon2 password hash.
            let is_verified = verify_password(creds.password, &password_hash).is_ok();
            Ok(is_verified.then(|| User {
                id,
                username: user_name,
                password_hash: password_hash.into(),
            }))
        })
        .await?
    }

    async fn get_user(&self, user_id: &UserId<Self>) -> Result<Option<Self::User>, Self::Error> {
        let Some(user) = self.db.fetch_user_password_by_id(*user_id).await? else {
            return Ok(None);
        };
        let DBUserPassword {
            id,
            user_name,
            password_hash,
        } = user;

        Ok(Some(User {
            id,
            username: user_name,
            password_hash: password_hash.into(),
        }))
    }
}

pub type AuthSession = axum_login::AuthSession<AppState>;
