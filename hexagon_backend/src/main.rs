mod config;
mod routes;

use crate::config::{BackendConfig, ServerConfig};
use crate::routes::AppState;
use std::sync::Arc;

#[tokio::main]
async fn main() {
    let config: BackendConfig = figment::Figment::new()
        .merge(figment::providers::Env::raw())
        .extract()
        .unwrap();
    dbg!(&config);
    let BackendConfig { db, server } = config;

    let db = Arc::new(db.connect().await.unwrap());

    let app = routes::router(AppState { db });
    let ServerConfig {
        server_host,
        server_port,
    } = server;
    let listener = tokio::net::TcpListener::bind(format!("{server_host}:{server_port}"))
        .await
        .unwrap();
    dbg!("Server starting");
    axum::serve(listener, app).await.unwrap();
}
