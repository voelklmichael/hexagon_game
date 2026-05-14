use uuid::Uuid;

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct DBHighscore {
    pub user_id: Uuid,
    pub mission_id: Uuid,
    pub max_velocity: i64,
    pub total_distance: i64,
}

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct DBHighscorePeak {
    pub max_velocity: i64,
    pub total_distance: i64,
}
