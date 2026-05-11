#[derive(Debug, Clone, serde::Deserialize)]
pub struct BackendConfig {
    #[serde(flatten)]
    pub db: hexagon_db::DBConfig,
    #[serde(flatten)]
    pub server: ServerConfig,
}

#[derive(Debug, Clone, serde::Deserialize)]
pub struct ServerConfig {
    pub server_port: u16,
    pub server_host: String,
}
