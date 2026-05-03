use super::board_types::{ConnectorId, ConnectorPosition, Tile};

#[derive(PartialEq, Eq, Hash)]
pub struct PlayerId(u32);

pub struct Player {
    pub id: PlayerId,
    pub current_position: ConnectorPosition,
    pub target: Option<ConnectorPosition>,
    pub history: Vec<PlayerHistorySingleTurn>,
    pub is_npc: bool,
    pub is_active: bool,
    pub hand: Vec<Tile>,
}

pub struct PlayerHistorySingleTurn {
    pub connectors: Vec<ConnectorId>,
}
