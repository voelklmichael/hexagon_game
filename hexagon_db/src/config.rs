use secrecy::{ExposeSecret, SecretString};
use sqlx::postgres::PgPoolOptions;

use super::DB;

#[derive(Debug, Clone, serde::Deserialize, serde::Serialize)]
pub struct DBConfig {
    pub host: String,
    pub port: u16,
    pub user: String,
    #[serde(serialize_with = "redact")]
    pub password: SecretString,
    pub dbname: String,
}

fn redact<S: serde::Serializer>(_: &SecretString, s: S) -> Result<S::Ok, S::Error> {
    s.serialize_str("[REDACTED]")
}

impl DBConfig {
    pub async fn connect(&self) -> Result<DB, sqlx::Error> {
        let url = format!(
            "postgres://{}:{}@{}:{}/{}",
            self.user,
            self.password.expose_secret(),
            self.host,
            self.port,
            self.dbname,
        );
        let pool = PgPoolOptions::new().connect(&url).await?;
        sqlx::migrate!("./migrations").run(&pool).await?;
        Ok(DB(pool))
    }
}
