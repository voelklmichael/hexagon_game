use uuid::Uuid;

use crate::board_types::{Board, ConnectorId, Tile};
use crate::{WinningConditionHighscoreV1, WinningConditionHighscoreV2};

// --- highscore ---
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MissionHighscoreV1 {
    pub board: Board,
    pub starting_point: ConnectorId,
    pub random_seed: u32,
    pub starting_hand: Vec<Tile>,
    pub winning_condition: WinningConditionHighscoreV1,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MissionHighscoreV2 {
    pub board: Board,
    pub starting_points: Vec<ConnectorId>,
    pub random_seed: u32,
    pub starting_hand: Vec<Tile>,
    pub winning_condition: WinningConditionHighscoreV2,
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum MissionHighscore {
    V1(MissionHighscoreV1),
    V2(MissionHighscoreV2),
}

// --- unified ---

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum Mission {
    HighScore(Box<MissionHighscore>),
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize, strum::VariantArray,
)]
pub enum MissionTag {
    Tutorial,
    Deliviery,
}
#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct MissionEntry {
    pub id: Uuid,
    pub tag: MissionTag,
    pub name: String,
    pub description: String,
    pub number: u32,
    pub json: Mission,
}
