mod config;
mod highscore;
mod user;
pub use config::DBConfig;
pub use hexagon_types::{DBHighscore, DBHighscorePeak};
pub use user::*;

pub struct DB(sqlx::PgPool);
pub use secrecy::{ExposeSecret, SecretString};
pub use sqlx::Error as SqlxError;
pub use uuid::Uuid;
