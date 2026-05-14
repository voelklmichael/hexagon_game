use axum::{
    Router,
    http::StatusCode,
    response::IntoResponse,
    routing::{get, post},
};
use axum_messages::Messages;
use serde::{Deserialize, Serialize};

use crate::routes::AppState;

use super::{AuthSession, Credentials};

pub fn login_router() -> Router<AppState> {
    Router::new()
        .route("/login", post(self::post::login))
        .route("/logout", get(self::get::logout))
        .route("/create", post(self::post::create))
}

#[derive(Debug, Serialize, Deserialize)]
pub struct NextUrl {
    next: Option<String>,
}
mod post {
    use axum::{Json, extract::State};
    use uuid::Uuid;

    use super::*;

    pub async fn login(
        mut auth_session: AuthSession,
        messages: Messages,
        Json(creds): Json<Credentials>,
    ) -> Result<Json<NextUrl>, StatusCode> {
        tracing::info!("Logging in user: {:?}", &creds.email);
        let user = match auth_session.authenticate(creds.clone()).await {
            Ok(Some(user)) => user,
            Ok(None) => {
                tracing::info!("Invalid credentials for user: {}", &creds.email);
                messages.error("Invalid credentials");

                return Err(StatusCode::INTERNAL_SERVER_ERROR);
            }
            Err(_) => return Err(StatusCode::INTERNAL_SERVER_ERROR),
        };

        if auth_session.login(&user).await.is_err() {
            return Err(StatusCode::INTERNAL_SERVER_ERROR);
        }

        messages.success(format!(
            "Successfully logged in as {}, {}",
            user.email, user.id
        ));

        Ok(Json(NextUrl { next: creds.next }))
    }

    #[derive(Deserialize)]
    pub struct UserCreation {
        id: Uuid,
        password: String,
        name: String,
        email: String,
    }

    pub async fn create(
        State(state): State<AppState>,
        Json(user): Json<UserCreation>,
    ) -> Result<Json<Uuid>, StatusCode> {
        tracing::info!("Creating user: {}, {}", &user.email, user.id);
        let UserCreation {
            id,
            password,
            name,
            email,
        } = user;
        let password_hash = {
            let salt = argon2::password_hash::SaltString::generate(
                &mut argon2::password_hash::rand_core::OsRng,
            );

            // Argon2 with default params (Argon2id v19)
            let argon2 = argon2::Argon2::default();

            // Hash password to PHC string ($argon2id$v=19$...)
            use argon2::PasswordHasher;
            match argon2.hash_password(password.as_bytes(), &salt) {
                Ok(hash) => hash.to_string(),
                Err(e) => {
                    tracing::warn!("Failed to hash password for user: {email}. Error: {e}");
                    return Err(StatusCode::INTERNAL_SERVER_ERROR);
                }
            }
        };

        match state
            .db
            .create_user(id, &password_hash, &name, &email)
            .await
        {
            Ok(user_id) => Ok(Json(user_id)),
            Err(e) => {
                tracing::warn!("Failed to create user in database {email}. Error: {e}");
                Err(StatusCode::INTERNAL_SERVER_ERROR)
            }
        }
    }
}

mod get {
    use super::*;

    pub async fn logout(mut auth_session: AuthSession) -> impl IntoResponse {
        tracing::info!("Logging out");

        match auth_session.logout().await {
            Ok(_) => StatusCode::NO_CONTENT.into_response(), //Redirect::to("/login").into_response(),
            Err(_) => StatusCode::INTERNAL_SERVER_ERROR.into_response(),
        }
    }
}
