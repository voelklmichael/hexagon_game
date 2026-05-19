mod config;
mod highscore;
mod mission;
mod previous_games;
mod user;
pub use config::DBConfig;
pub use hexagon_types::{DBHighscore, DBHighscorePeak, MissionEntry};
pub use user::*;

pub struct DB(sqlx::PgPool);

impl DB {
    pub fn pool(&self) -> &sqlx::PgPool {
        &self.0
    }
}
pub use secrecy::{ExposeSecret, SecretString};
pub use sqlx::Error as SqlxError;
pub use uuid::Uuid;
