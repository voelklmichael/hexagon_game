use secrecy::{ExposeSecret, SecretString};
use sqlx::postgres::PgPoolOptions;

use super::DB;

#[derive(Debug, Clone, serde::Deserialize)]
pub struct DBConfig {
    pub db_host: String,
    pub db_port: u16,
    pub db_user: String,
    pub db_password: SecretString,
    pub db_name: String,
}

impl DBConfig {
    pub fn local_docker() -> Self {
        Self {
            db_host: "localhost".into(),
            db_port: 5432,
            db_user: "postgres".into(),
            db_password: "postgres".to_string().into(),
            db_name: "postgres".into(),
        }
    }
    pub async fn connect(&self) -> Result<DB, sqlx::Error> {
        let url = format!(
            "postgres://{}:{}@{}:{}/{}",
            self.db_user,
            urlencoding::encode(&self.db_password.expose_secret()),
            self.db_host,
            self.db_port,
            self.db_name,
        );
        let pool = PgPoolOptions::new().connect(&url).await?;
        sqlx::migrate!("./migrations").run(&pool).await?;
        Ok(DB(pool))
    }
}
