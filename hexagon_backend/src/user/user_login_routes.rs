use axum::{
    Router,
    http::StatusCode,
    response::IntoResponse,
    routing::{get, post},
};
use axum_messages::Messages;
use hexagon_types::PasswordResetRequest;
use serde::Deserialize;

use crate::routes::AppState;

use super::{AuthSession, Credentials};

pub fn login_router() -> Router<AppState> {
    Router::new()
        .route("/login", post(post::login))
        .route("/logout", get(get::logout))
        .route(
            "/request_password_reset",
            post(post::request_password_reset),
        )
        .route("/create", post(post::create))
        .route("/me", get(get::me))
}

use hexagon_types::LoginResponse;
mod post {
    use axum::{Json, extract::State};
    use uuid::Uuid;

    use super::*;

    pub async fn request_password_reset(
        State(state): State<AppState>,
        Json(req): Json<PasswordResetRequest>,
    ) -> StatusCode {
        tracing::info!("POST /user_login/request_password_reset for {}", req.email);

        match state.db.create_password_reset_token(&req.email).await {
            Ok(_) => StatusCode::NO_CONTENT, // worker picks it up; don't reveal whether user exists
            Err(e) => {
                tracing::warn!("create_password_reset_token failed: {e}");
                StatusCode::INTERNAL_SERVER_ERROR
            }
        }
    }
    pub async fn login(
        mut auth_session: AuthSession,
        messages: Messages,
        Json(creds): Json<Credentials>,
    ) -> Result<Json<LoginResponse>, (StatusCode, &'static str)> {
        tracing::info!("Logging in user: {:?}", &creds.email);
        let user = match auth_session.authenticate(creds.clone()).await {
            Ok(Some(user)) => user,
            Ok(None) => {
                tracing::info!("Invalid credentials for user: {}", &creds.email);
                return Err((StatusCode::UNAUTHORIZED, "Invalid email or password"));
            }
            Err(_) => return Err((StatusCode::INTERNAL_SERVER_ERROR, "Authentication error")),
        };

        if auth_session.login(&user).await.is_err() {
            return Err((
                StatusCode::INTERNAL_SERVER_ERROR,
                "Failed to create session",
            ));
        }

        messages.success(format!(
            "Successfully logged in as {}, {}",
            user.email, user.id
        ));

        Ok(Json(LoginResponse {
            next: creds.next,
            user_id: user.id,
            name: user.name,
        }))
    }

    #[derive(Deserialize)]
    pub struct UserCreation {
        password: String,
        name: String,
        email: String,
    }

    pub async fn create(
        State(state): State<AppState>,
        Json(user): Json<UserCreation>,
    ) -> Result<Json<Uuid>, (StatusCode, &'static str)> {
        tracing::info!("Creating user: {}", &user.email);
        let UserCreation {
            password,
            name,
            email,
        } = user;

        let valid = |s: &str| s.len() >= 5 && !s.contains(char::is_whitespace);
        if !valid(&name) || !valid(&email) {
            return Err((
                StatusCode::UNPROCESSABLE_ENTITY,
                "Name and email must be at least 5 characters and contain no whitespace",
            ));
        }
        if password.len() < 5 {
            return Err((
                StatusCode::UNPROCESSABLE_ENTITY,
                "Password must be at least 5 characters",
            ));
        }
        let password_hash = {
            let salt = argon2::password_hash::SaltString::generate(
                &mut argon2::password_hash::rand_core::OsRng,
            );
            let argon2 = argon2::Argon2::default();
            use argon2::PasswordHasher;
            match argon2.hash_password(password.as_bytes(), &salt) {
                Ok(hash) => hash.to_string(),
                Err(e) => {
                    tracing::warn!("Failed to hash password for user: {email}. Error: {e}");
                    return Err((StatusCode::INTERNAL_SERVER_ERROR, "Failed to hash password"));
                }
            }
        };

        match state.db.create_user(&password_hash, &name, &email).await {
            Ok(user_id) => Ok(Json(user_id)),
            Err(e) => {
                tracing::warn!("Failed to create user in database {email}. Error: {e}");
                Err((StatusCode::INTERNAL_SERVER_ERROR, "Failed to create user"))
            }
        }
    }
}

mod get {
    use axum::Json;
    use axum_login::AuthUser;
    use hexagon_types::MeResponse;

    use super::*;

    pub async fn logout(mut auth_session: AuthSession) -> impl IntoResponse {
        tracing::info!("Logging out");

        match auth_session.logout().await {
            Ok(_) => StatusCode::NO_CONTENT.into_response(),
            Err(_) => StatusCode::INTERNAL_SERVER_ERROR.into_response(),
        }
    }

    pub async fn me(auth_session: AuthSession) -> Result<Json<MeResponse>, StatusCode> {
        tracing::info!("GET /user_login/me");
        match auth_session.user {
            Some(user) => Ok(Json(MeResponse {
                user_id: user.id(),
                name: user.name.clone(),
            })),
            None => Err(StatusCode::UNAUTHORIZED),
        }
    }
}
