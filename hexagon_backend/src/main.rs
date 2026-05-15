mod config;
mod routes;
mod user;

use crate::config::{BackendConfig, ServerConfig};
use crate::routes::AppState;
use std::sync::Arc;

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt::init();
    let config: BackendConfig = figment::Figment::new()
        .merge(figment::providers::Env::raw())
        .extract()
        .unwrap();
    tracing::info!("Config: {config:?}");
    let BackendConfig { db, server } = config;

    let db = Arc::new(db.connect().await.unwrap());

    let app = routes::router(AppState { db }).await;
    let ServerConfig {
        server_host,
        server_port,
    } = server;
    let listener = tokio::net::TcpListener::bind(format!("{server_host}:{server_port}"))
        .await
        .unwrap();
    tracing::info!("Server starting");
    axum::serve(listener, app).await.unwrap();
}
