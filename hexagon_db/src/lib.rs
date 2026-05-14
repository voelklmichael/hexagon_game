mod config;
mod highscore;
mod user_password;
pub use config::DBConfig;
pub use hexagon_types::{DBHighscore, DBHighscorePeak};
pub use user_password::*;

pub struct DB(sqlx::PgPool);
pub use secrecy::{ExposeSecret, SecretString};
pub use sqlx::Error as SqlxError;
pub use uuid::Uuid;
