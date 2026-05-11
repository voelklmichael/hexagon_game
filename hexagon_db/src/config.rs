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
