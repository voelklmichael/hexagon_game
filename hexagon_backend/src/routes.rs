use std::sync::Arc;

use axum::{
    Json, Router,
    extract::{Path, State},
    http::{StatusCode, Uri},
    routing::{get, post},
};
use hexagon_db::{DB, DBHighscore, DBHighscorePeak};
use uuid::Uuid;

#[derive(Clone)]
pub struct AppState {
    pub db: Arc<DB>,
}

pub fn router(state: AppState) -> Router {
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
        .fallback(fallback)
        .with_state(state)
}

async fn healthz() -> StatusCode {
    StatusCode::NO_CONTENT
}

async fn upsert_highscore(
    State(state): State<AppState>,
    Json(score): Json<DBHighscore>,
) -> StatusCode {
    match state.db.upsert_highscore(&score).await {
        Ok(()) => StatusCode::NO_CONTENT,
        Err(_) => StatusCode::INTERNAL_SERVER_ERROR,
    }
}

async fn fetch_highscore_user(
    State(state): State<AppState>,
    Path((user_id, mission_id)): Path<(Uuid, Uuid)>,
) -> Result<Json<DBHighscorePeak>, StatusCode> {
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

async fn fallback(uri: Uri, body: String) -> StatusCode {
    dbg!(uri, body);
    StatusCode::NOT_FOUND
}
