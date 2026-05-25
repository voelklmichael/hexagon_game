#[derive(Debug, Clone, serde::Deserialize)]
pub struct BackendConfig {
    #[serde(flatten)]
    pub db: hexagon_db::DBConfig,
    #[serde(flatten)]
    pub server: ServerConfig,
    #[serde(flatten)]
    pub smtp: SmtpConfig,
}

#[derive(Debug, Clone, serde::Deserialize)]
pub struct ServerConfig {
    pub server_port: u16,
    pub server_host: String,
}

#[derive(Debug, Clone, serde::Deserialize)]
pub struct SmtpConfig {
    pub smtp_host: String,
    pub smtp_port: u16,
    pub smtp_username: String,
    pub smtp_password: secrecy::SecretString,
    /// The "From" address used in outgoing emails, e.g. `"Game <noreply@example.com>"`.
    pub email_from: String,
    /// Base URL of the app used to build links in emails, e.g. `"https://hexagon-game.no"`.
    pub base_url: String,
}
