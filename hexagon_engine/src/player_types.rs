use super::board_types::{ConnectorEnd, ConnectorId, ConnectorPosition, Tile};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PlayerId(pub u32);

pub struct Player {
    pub id: PlayerId,
    pub current_position: (ConnectorId, ConnectorEnd),
    pub target: Option<ConnectorId>,
    pub history: Vec<PlayerHistorySingleTurn>,
    pub is_npc: bool,
    pub is_active: bool,
    pub hand: Vec<Tile>,
}

pub struct PlayerHistorySingleTurn {
    pub connectors: Vec<(ConnectorId, ConnectorEnd)>,
}
impl PlayerHistorySingleTurn {
    pub(crate) fn new_from_start(start: &ConnectorId) -> Vec<PlayerHistorySingleTurn> {
        [PlayerHistorySingleTurn {
            connectors: [(start.clone(), ConnectorEnd::StartedAtA)].into(),
        }]
        .into()
    }
}
