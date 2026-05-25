use hexagon_types::player::PlayerId;
use serde::{Deserialize, Serialize};

use super::{ConnectorEnd, ConnectorId, Tile};

#[derive(Clone, Copy, Serialize, Deserialize)]
pub struct PlayerPosition {
    pub connector: ConnectorId,
    pub end: ConnectorEnd,
    /// Fraction [0, 1] along the connector from the entry end where the player stopped due to a
    /// collision. `None` when the player is at the normal exit of the connector.
    pub hit_fraction: Option<f32>,
}

impl PlayerPosition {
    pub fn new(connector: ConnectorId, end: ConnectorEnd) -> Self {
        Self {
            connector,
            end,
            hit_fraction: None,
        }
    }
}

#[derive(Clone, Serialize, Deserialize)]
pub struct Player {
    pub id: PlayerId,
    pub current_position: PlayerPosition,
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
