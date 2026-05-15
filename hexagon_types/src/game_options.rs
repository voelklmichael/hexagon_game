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
pub struct WinningConditionHighscore {
    pub min_velocity: Option<u32>,
    pub min_distance: Option<u32>,
    pub target: Option<crate::board_types::ConnectorId>,
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize, Default)]
pub enum WinningCondition {
    LastManStanding,
    #[default]
    LongestWay,
    HighestVelocity,
    Highscore(WinningConditionHighscore),
}
