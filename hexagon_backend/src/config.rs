#[derive(Debug, Clone, serde::Deserialize)]
pub struct BackendConfig {
    #[serde(flatten)]
    pub db: hexagon_db::DBConfig,
    #[serde(flatten)]
    pub server: ServerConfig,
    /// Resend API key for sending password-reset emails (optional).
    pub resend_api_key: Option<secrecy::SecretString>,
    /// The "From" address used in outgoing emails, e.g. `"Game <noreply@example.com>"`.
    pub email_from: Option<String>,
}

#[derive(Debug, Clone, serde::Deserialize)]
pub struct ServerConfig {
    pub server_port: u16,
    pub server_host: String,
}
