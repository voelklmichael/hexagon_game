use std::collections::HashMap;
use uuid::Uuid;

pub mod board_types;
pub use board_types::*;
pub mod game_options;
pub mod rng;
pub use rng::RandomNumberGenerator;
pub use game_options::*;
pub mod missions;
pub use missions::*;

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct LoginResponse {
    pub next: Option<String>,
    pub user_id: Uuid,
    pub name: String,
}

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct MeResponse {
    pub user_id: Uuid,
    pub name: String,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct PlayerStats {
    pub max_velocity: i64,
    pub total_distance: i64,
}

/// Per-run statistics for all players, sent from the frontend after a mission ends.
/// Keys are player indices (0–9).
#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct DBHighscore {
    pub user_id: Uuid,
    pub mission_id: Uuid,
    pub players: HashMap<u8, PlayerStats>,
}

/// Best recorded values per player for a given (user, mission) or across all users.
/// Keys are player indices (0–9); only players with at least one non-zero stat are included.
#[derive(Debug, Default, serde::Serialize, serde::Deserialize)]
pub struct DBHighscorePeak {
    pub players: HashMap<u8, PlayerStats>,
}
