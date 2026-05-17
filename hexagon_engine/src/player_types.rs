use hexagon_types::player::PlayerId;
use serde::{Deserialize, Serialize};

use super::board_types::{ConnectorEnd, ConnectorId, ConnectorPosition, Tile};


#[derive(Clone, Serialize, Deserialize)]
pub struct Player {
    pub id: PlayerId,
    pub current_position: (ConnectorId, ConnectorEnd),
    pub target: Option<ConnectorId>,
    pub history: Vec<PlayerHistorySingleTurn>,
    pub is_npc: bool,
    pub is_active: bool,
    pub hand: Vec<Tile>,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct HistoryConnector {
    pub id: ConnectorId,
    pub end: ConnectorEnd,
    pub weight: u32,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct PlayerHistorySingleTurn {
    pub connectors: Vec<HistoryConnector>,
}
impl PlayerHistorySingleTurn {
    pub(crate) fn new_from_start(start: &ConnectorId) -> Vec<PlayerHistorySingleTurn> {
        [PlayerHistorySingleTurn {
            connectors: [HistoryConnector {
                id: *start,
                end: ConnectorEnd::StartedAtA,
                weight: 1,
            }]
            .into(),
        }]
        .into()
    }
}
