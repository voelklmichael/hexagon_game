mod config;
mod email_queue;
mod routes;
mod user;

use std::sync::Arc;

use crate::config::{BackendConfig, ServerConfig};
use crate::routes::AppState;
use tokio_util::sync::CancellationToken;

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt::init();
    let config: BackendConfig = figment::Figment::new()
        .merge(figment::providers::Env::raw())
        .extract()
        .unwrap();
    tracing::info!("Config: {config:?}");
    let BackendConfig { db, server, smtp } = config;

    let db = Arc::new(db.connect().await.unwrap());

    let cancellation_token = CancellationToken::new();
    email_queue::spawn_email_worker(db.clone(), smtp, cancellation_token.clone());

    let app = routes::router(AppState { db }).await;

    let ServerConfig {
        server_host,
        server_port,
    } = server;
    let listener = tokio::net::TcpListener::bind(format!("{server_host}:{server_port}"))
        .await
        .unwrap();
    tracing::info!("Server starting");
    axum::serve(listener, app)
        .with_graceful_shutdown(async move {
            tokio::signal::ctrl_c().await.ok();
            tracing::info!("Shutdown signal received");
            cancellation_token.cancel();
        })
        .await
        .unwrap();
}
