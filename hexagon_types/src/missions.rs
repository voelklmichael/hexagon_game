use uuid::Uuid;

use crate::WinningConditionHighscoreV1;
use crate::board_types::{Board, ConnectorId, Tile};
use crate::game_options::OuterConnectors;

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
    pub starting_point: Vec<ConnectorId>,
    pub random_seed: u32,
    pub starting_hand: Vec<Tile>,
    pub winning_condition: WinningConditionHighscoreV1,
}
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum MissionHighscore {
    V1(MissionHighscoreV1),
    V2(MissionHighscoreV2),
}

// --- unified ---

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, strum::EnumDiscriminants)]
#[strum_discriminants(
    name(MissionKind),
    derive(serde::Serialize, serde::Deserialize, strum::VariantArray)
)]
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
