use std::sync::Arc;

use axum::{
    Json, Router,
    extract::{Path, State},
    http::{HeaderValue, StatusCode, Uri},
    routing::{get, post},
};
use axum_login::{AuthManagerLayerBuilder, AuthUser};
use axum_messages::MessagesManagerLayer;
use hexagon_db::{DB, DBHighscore, DBHighscorePeak};
use tower_http::cors::CorsLayer;
use tower_sessions::{MemoryStore, SessionManagerLayer};
use uuid::Uuid;

#[derive(Clone)]
pub struct AppState {
    pub db: Arc<DB>,
}

pub fn router(state: AppState) -> Router {
    let session_layer = SessionManagerLayer::new(MemoryStore::default());
    let auth_layer = AuthManagerLayerBuilder::new(state.clone(), session_layer).build();

    let cors = CorsLayer::new()
        .allow_origin([
            "http://localhost:8080".parse::<HeaderValue>().unwrap(),
            "https://voelklmichael.github.io"
                .parse::<HeaderValue>()
                .unwrap(),
        ])
        .allow_methods([
            axum::http::Method::GET,
            axum::http::Method::POST,
            axum::http::Method::PUT,
            axum::http::Method::DELETE,
        ])
        .allow_headers([
            axum::http::header::CONTENT_TYPE,
            axum::http::header::AUTHORIZATION,
        ])
        .allow_credentials(true);

    Router::new()
        .route("/healthz", get(healthz))
        .route("/highscore", post(upsert_highscore))
        .route(
            "/highscore/overall/{mission_id}",
            get(fetch_highscore_overall),
        )
        .route(
            "/highscore/user/{user_id}/{mission_id}",
            get(fetch_highscore_user),
        )
        .route(
            "/highscore/user/{user_id}/missions",
            get(fetch_won_missions),
        )
        .nest("/user_login", crate::user::login_router())
        .fallback(fallback)
        .layer(MessagesManagerLayer)
        .layer(auth_layer)
        .layer(cors)
        .with_state(state)
}

/// Checks that a session is active and belongs to `user_id`.
/// Returns `UNAUTHORIZED` if not logged in, `FORBIDDEN` if the IDs don't match.
fn require_own_user(
    auth_session: &crate::user::AuthSession,
    user_id: Uuid,
) -> Result<(), StatusCode> {
    let Some(user) = &auth_session.user else {
        return Err(StatusCode::UNAUTHORIZED);
    };
    if user.id() != user_id {
        return Err(StatusCode::FORBIDDEN);
    }
    Ok(())
}

async fn healthz() -> StatusCode {
    StatusCode::NO_CONTENT
}

async fn upsert_highscore(
    auth_session: crate::user::AuthSession,
    State(state): State<AppState>,
    Json(score): Json<DBHighscore>,
) -> Result<StatusCode, StatusCode> {
    require_own_user(&auth_session, score.user_id)?;
    match state.db.upsert_highscore(&score).await {
        Ok(()) => Ok(StatusCode::NO_CONTENT),
        Err(_) => Err(StatusCode::INTERNAL_SERVER_ERROR),
    }
}

async fn fetch_highscore_user(
    auth_session: crate::user::AuthSession,
    State(state): State<AppState>,
    Path((user_id, mission_id)): Path<(Uuid, Uuid)>,
) -> Result<Json<DBHighscorePeak>, StatusCode> {
    require_own_user(&auth_session, user_id)?;
    match state.db.fetch_highscore_user(user_id, mission_id).await {
        Ok(Some(peak)) => Ok(Json(peak)),
        Ok(None) => Err(StatusCode::NOT_FOUND),
        Err(_) => Err(StatusCode::INTERNAL_SERVER_ERROR),
    }
}

async fn fetch_highscore_overall(
    State(state): State<AppState>,
    Path(mission_id): Path<Uuid>,
) -> Result<Json<DBHighscorePeak>, StatusCode> {
    match state.db.fetch_highscore_overall(mission_id).await {
        Ok(peak) => Ok(Json(peak)),
        Err(_) => Err(StatusCode::INTERNAL_SERVER_ERROR),
    }
}

async fn fetch_won_missions(
    auth_session: crate::user::AuthSession,
    State(state): State<AppState>,
    Path(user_id): Path<Uuid>,
) -> Result<Json<Vec<Uuid>>, StatusCode> {
    require_own_user(&auth_session, user_id)?;
    match state.db.fetch_won_missions(user_id).await {
        Ok(ids) => Ok(Json(ids)),
        Err(_) => Err(StatusCode::INTERNAL_SERVER_ERROR),
    }
}

async fn fallback(uri: Uri, body: String) -> StatusCode {
    dbg!(uri, body);
    StatusCode::NOT_FOUND
}
