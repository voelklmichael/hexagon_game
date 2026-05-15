#[derive(
    Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize, strum::EnumIter, Default,
)]
pub enum OuterConnectors {
    OnlyDeathEnds,
    #[default]
    ReducedDeathEnds,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize, Default)]
pub enum CollisionMode {
    #[default]
    PassThrough,
    BothDie,
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize, Default)]
pub enum WinningCondition {
    LastManStanding,
    #[default]
    LongestWay,
    HighestVelocity,
    Highscore {
        min_velocity: Option<u32>,
        min_distance: Option<u32>,
        target: Option<crate::board_types::ConnectorId>,
    },
}
