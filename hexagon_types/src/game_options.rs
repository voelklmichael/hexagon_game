use std::collections::HashMap;

use crate::{ConnectorId, player::PlayerId};

#[derive(
    Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize, strum::EnumIter, Default,
)]
pub enum OuterConnectors {
    #[default]
    OnlyDeathEnds,
    ReducedDeathEnds,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize, Default)]
pub enum CollisionMode {
    #[default]
    PassThrough,
    BothDie,
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize, Default)]
pub struct WinningConditionHighscoreV1 {
    pub min_velocity: Option<u32>,
    pub min_distance: Option<u32>,
    pub target: Option<ConnectorId>,
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize, Default)]
pub struct WinningConditionHighscoreV2 {
    pub min_velocity: HashMap<PlayerId, u32>,
    pub min_distance: HashMap<PlayerId, u32>,
    pub target: HashMap<PlayerId, ConnectorId>,
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize, Default)]
pub enum WinningCondition {
    LastManStanding,
    #[default]
    LongestWay,
    HighestVelocity,
    Highscore(WinningConditionHighscoreV1),
}
