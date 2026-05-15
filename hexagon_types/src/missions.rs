use uuid::Uuid;

use crate::board_types::{Board, Tile};
use crate::game_options::{OuterConnectors, WinningCondition};

// --- delivery ---

#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
#[serde(default)]
pub struct MissionDeliveryV1 {
    pub board_radius: u32,
    pub outer_connectors: OuterConnectors,
    pub random_seed: u32,
    pub npc_count: u32,
    pub player_has_target: bool,
    pub hand_size: u32,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum MissionDelivery {
    V1(MissionDeliveryV1),
}

// --- highscore ---

#[derive(Debug, Clone, Default, serde::Serialize, serde::Deserialize)]
#[serde(default)]
pub struct MissionHighscoreV1 {
    pub board: Board,
    pub random_seed: u32,
    pub starting_hand: Vec<Tile>,
    pub winning_condition: WinningCondition,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum MissionHighscore {
    V1(MissionHighscoreV1),
}

// --- unified ---

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, strum::EnumDiscriminants)]
#[strum_discriminants(name(MissionKind), derive(serde::Serialize, serde::Deserialize, strum::VariantArray))]
pub enum Mission {
    HighScore(MissionHighscore),
    Delivery(MissionDelivery),
}

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct MissionEntry {
    pub id: Uuid,
    pub kind: MissionKind,
    pub name: String,
    pub description: String,
    pub number: u32,
    pub json: Mission,
}
