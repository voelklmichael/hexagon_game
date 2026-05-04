pub struct GameOptionsStandard {
    pub board_radius: usize,
    pub outer_connectors: OuterConnectors,
    pub random_seed: u32,
    pub player_count: usize,
    pub collision_mode: CollisionMode,
    pub winning_condition: WinningConditionStandard,
    pub hand_size: usize,
}

pub enum OuterConnectors {
    OnlyDeathEnds,
    ReducedDeathEnds,
}

pub enum CollisionMode {
    PassThrough,
    BothDie,
}

pub enum WinningConditionStandard {
    LastManStanding,
    LongestWay,
    HighestVelocity,
}
pub struct GameOptionsDelivery {
    pub board_radius: usize,
    pub outer_connectors: OuterConnectors,
    pub random_seed: u32,
    pub npc_count: usize,
    pub player_has_target: bool,
    pub hand_size: usize,
}

pub enum GameOptions {
    Delivery(GameOptionsDelivery),
    Standard(GameOptionsStandard),
}
