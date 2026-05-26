use std::sync::Arc;

use axum::{
    Json, Router,
    extract::{Path, State},
    http::{StatusCode, Uri},
    routing::{get, post},
};
use axum_login::{AuthManagerLayerBuilder, AuthUser};
use axum_messages::MessagesManagerLayer;
use hexagon_db::{DB, DBHighscore, DBHighscorePeak, MissionEntry};
use hexagon_types::{PreviousGame, SavePreviousGameRequest};
use tower_http::cors::{AllowOrigin, CorsLayer};
use tower_sessions::SessionManagerLayer;
use tower_sessions_sqlx_store::PostgresStore;
use uuid::Uuid;

#[derive(Clone)]
pub struct AppState {
    pub db: Arc<DB>,
}

pub async fn router(state: AppState) -> Router {
    let session_store = PostgresStore::new(state.db.pool().clone());
    session_store
        .migrate()
        .await
        .expect("session store migration failed");

    #[cfg(debug_assertions)]
    let session_layer = SessionManagerLayer::new(session_store);
    #[cfg(not(debug_assertions))]
    let session_layer = SessionManagerLayer::new(session_store)
        .with_same_site(tower_sessions::cookie::SameSite::None)
        .with_secure(true);
    let auth_layer = AuthManagerLayerBuilder::new(state.clone(), session_layer).build();

    let allow_origin = AllowOrigin::predicate(|origin, _| {
        let b = origin.as_bytes();
        b == b"https://voelklmichael.github.io"
            || b == b"https://hexagon-game.no"
            || b.starts_with(b"http://localhost:")
            || b.starts_with(b"http://127.0.0.1:")
    });
    let cors = CorsLayer::new()
        .allow_origin(allow_origin)
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
        .route("/missions", get(fetch_all_missions))
        .route("/previous_games", post(save_previous_game))
        .route("/previous_games", get(fetch_previous_games))
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
    tracing::debug!("GET /healthz");
    StatusCode::NO_CONTENT
}

async fn fetch_highscore_user(
    auth_session: crate::user::AuthSession,
    State(state): State<AppState>,
    Path((user_id, mission_id)): Path<(Uuid, Uuid)>,
) -> Result<Json<DBHighscorePeak>, StatusCode> {
    tracing::info!("GET /highscore/user/{user_id}/{mission_id}");
    require_own_user(&auth_session, user_id)?;
    match state.db.fetch_highscore_user(user_id, mission_id).await {
        Ok(Some(peak)) => Ok(Json(peak)),
        Ok(None) => Err(StatusCode::NOT_FOUND),
        Err(e) => {
            tracing::warn!("fetch_highscore_user failed: {e}");
            Err(StatusCode::INTERNAL_SERVER_ERROR)
        }
    }
}

async fn fetch_highscore_overall(
    State(state): State<AppState>,
    Path(mission_id): Path<Uuid>,
) -> Result<Json<DBHighscorePeak>, StatusCode> {
    tracing::info!("GET /highscore/overall/{mission_id}");
    match state.db.fetch_highscore_overall(mission_id).await {
        Ok(peak) => Ok(Json(peak)),
        Err(e) => {
            tracing::warn!("fetch_highscore_overall failed: {e}");
            Err(StatusCode::INTERNAL_SERVER_ERROR)
        }
    }
}

async fn fetch_won_missions(
    auth_session: crate::user::AuthSession,
    State(state): State<AppState>,
    Path(user_id): Path<Uuid>,
) -> Result<Json<Vec<Uuid>>, StatusCode> {
    tracing::info!("GET /highscore/user/{user_id}/missions");
    require_own_user(&auth_session, user_id)?;
    match state.db.fetch_won_missions(user_id).await {
        Ok(ids) => Ok(Json(ids)),
        Err(e) => {
            tracing::warn!("fetch_won_missions failed: {e}");
            Err(StatusCode::INTERNAL_SERVER_ERROR)
        }
    }
}

async fn fetch_all_missions(
    State(state): State<AppState>,
) -> Result<Json<Vec<MissionEntry>>, StatusCode> {
    tracing::info!("GET /missions");
    match state.db.fetch_all_missions().await {
        Ok(missions) => Ok(Json(missions)),
        Err(e) => {
            tracing::warn!("fetch_all_missions failed: {e}");
            Err(StatusCode::INTERNAL_SERVER_ERROR)
        }
    }
}

async fn save_previous_game(
    auth_session: crate::user::AuthSession,
    State(state): State<AppState>,
    Json(req): Json<SavePreviousGameRequest>,
) -> Result<Json<i64>, StatusCode> {
    tracing::info!(
        "POST /previous_games user_id={} mission_id={}",
        req.user_id,
        req.mission_id
    );
    require_own_user(&auth_session, req.user_id)?;
    let id = match state
        .db
        .insert_previous_game(req.user_id, req.mission_id, &req.game_state)
        .await
    {
        Ok(id) => id,
        Err(e) => {
            tracing::warn!("insert_previous_game failed: {e}");
            return Err(StatusCode::INTERNAL_SERVER_ERROR);
        }
    };
    let score = DBHighscore {
        user_id: req.user_id,
        mission_id: req.mission_id,
        game_id: id,
        players: req.players,
    };
    if let Err(e) = state.db.upsert_highscore(&score).await {
        tracing::warn!("upsert_highscore failed: {e}");
        return Err(StatusCode::INTERNAL_SERVER_ERROR);
    }
    Ok(Json(id))
}

async fn fetch_previous_games(
    State(state): State<AppState>,
) -> Result<Json<Vec<PreviousGame>>, StatusCode> {
    tracing::info!("GET /previous_games");
    match state.db.fetch_previous_games().await {
        Ok(games) => Ok(Json(games)),
        Err(e) => {
            tracing::warn!("fetch_previous_games failed: {e}");
            Err(StatusCode::INTERNAL_SERVER_ERROR)
        }
    }
}

async fn fallback(uri: Uri, body: String) -> StatusCode {
    tracing::warn!("unmatched route: {uri} body={body:?}");
    StatusCode::NOT_FOUND
}
