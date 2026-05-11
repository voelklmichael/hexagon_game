mod config;
mod highscore;
pub use config::DBConfig;
pub use highscore::{DBHighscore, DBHighscorePeak};

pub struct DB(sqlx::PgPool);
