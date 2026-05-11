mod config;
mod highscore;
pub use config::*;

pub struct DB(sqlx::PgPool);
