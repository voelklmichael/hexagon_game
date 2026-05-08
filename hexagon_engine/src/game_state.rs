use serde::{Deserialize, Serialize};

use crate::Board;
use crate::board_types::{
    Connector, ConnectorEnd, ConnectorId, ConnectorKind, ConnectorOnHex, ConnectorPosition,
    HexagonPosition, Tile, TileRotationDirection,
};
use crate::game_options::{GameOptions, WinningConditionStandard};
use crate::player_types::{HistoryConnector, PlayerHistorySingleTurn};
use crate::random_number_generator::RandomNumberGenerator;
use crate::statistics::Statistics;
use crate::{Player, PlayerId};

#[derive(Serialize, Deserialize)]
pub enum GameResult {
    Win(Vec<PlayerId>),
    Draw(Vec<PlayerId>),
    Loss,
}

#[derive(Serialize, Deserialize)]
pub struct GameState {
    pub board: Board,
    pub players: Vec<Player>,
    pub current_player: PlayerId,
    pub rng: RandomNumberGenerator,
    pub options: GameOptions,
    pub statistics: Statistics,
    pub result: Option<GameResult>,
}

impl GameState {
    pub fn current_player_hexagon(&self) -> Option<HexagonPosition> {
        let player = self.players.iter().find(|p| p.id == self.current_player)?;
        let (connector_id, end) = player.current_position;
        let kind = &self.board.connectors.iter().find(|c| c.id == connector_id)?.kind;
        let hexagon = match kind {
            ConnectorKind::DeadEnd(c) => c.position.hexagon,
            ConnectorKind::HexToHex(c) | ConnectorKind::Outside(c) => match end {
                ConnectorEnd::StartedAtA => c.connector_a.hexagon,
                ConnectorEnd::StartedAtB => c.connector_b.hexagon,
            },
            ConnectorKind::OnHex(c) => c.hexagon,
        };
        Some(hexagon)
    }

    pub fn rotate_tile(&mut self, tile: usize, direction: TileRotationDirection) {
        let Some(player) = self
            .players
            .iter_mut()
            .find(|p| p.id == self.current_player)
        else {
            tracing::error!("No player found");
            return;
        };
        let Some(tile) = player.hand.get_mut(tile) else {
            tracing::error!("No tile found");
            return;
        };
        tile.rotate(direction);
    }

    pub fn play_tile(&mut self, tile: usize) {
        // step 1: add connectors from tile to board
        {
            let Some(player) = self
                .players
                .iter_mut()
                .find(|p| p.id == self.current_player)
            else {
                tracing::error!("No player found");
                return;
            };
            if tile >= player.hand.len() {
                tracing::error!("No tile found");
                return;
            }
            let (connector, end) = player.current_position;
            let connector_kind = &self
                .board
                .connectors
                .iter()
                .find(|x| x.id == connector)
                .expect("No connector found")
                .kind;
            let position = match connector_kind {
                ConnectorKind::DeadEnd(c) => c.position.clone(),
                ConnectorKind::HexToHex(c) | ConnectorKind::Outside(c) => match end {
                    ConnectorEnd::StartedAtA => c.connector_a.clone(),
                    ConnectorEnd::StartedAtB => c.connector_b.clone(),
                },
                ConnectorKind::OnHex(c) => {
                    let edge_sub = match end {
                        ConnectorEnd::StartedAtA => c.edge_sub.a.clone(),
                        ConnectorEnd::StartedAtB => c.edge_sub.b.clone(),
                    };
                    ConnectorPosition {
                        hexagon: c.hexagon,
                        edge_sub,
                    }
                }
            };

            if let Some(connector) = self.board.connectors.iter().find_map(|x| match &x.kind {
                ConnectorKind::OnHex(connector_on_hex) => {
                    (connector_on_hex.hexagon == position.hexagon).then_some(&x.id)
                }
                _ => None,
            }) {
                tracing::error!("There is already a connector at this spot.");
                return;
            };

            let tile = player.hand.remove(tile);
            let mut offset = self
                .board
                .connectors
                .iter()
                .map(|x| x.id.0)
                .max()
                .unwrap_or(0)
                + 1;
            tile.inner_connectors
                .into_iter()
                .enumerate()
                .for_each(|(i, connector)| {
                    self.board.connectors.push(Connector {
                        id: ConnectorId(offset + i as u32),
                        kind: ConnectorKind::OnHex(ConnectorOnHex {
                            hexagon: position.hexagon,
                            edge_sub: connector,
                        }),
                        weight: 1_000,
                    });
                });

            player
                .hand
                .push(Tile::create_fully_connected(&mut self.rng)); //TODO: this should depend on the game options
        }
        // step 2: move players along the connectors
        {
            // first, compute for each player
            let possible_paths = self
                .players
                .iter()
                .map(|p| {
                    (
                        p.id,
                        self.board.compute_path_starting_from(&p.current_position),
                    )
                })
                .collect::<Vec<_>>();
            // check if any two path overlap
            // if so, compute a hit point by the corresponding velocity (total distance)
            {
                'outer: for (lindex, (lid, left)) in possible_paths.iter().enumerate() {
                    if self.options.collision_mode() == crate::game_options::CollisionMode::BothDie
                    {
                        for (rindex, (rid, right)) in
                            possible_paths.iter().enumerate().skip(lindex + 1)
                        {
                            if left.iter().any(|l| right.iter().any(|r| l.0 == r.0)) {
                                // crash
                                // note: only two players can crash, triple crashes are not possible
                                // the crash point can be in the middle of a connector (or anywhere along it, actually)
                                let left_velocity: u32 = left.iter().map(|x| x.2).sum();
                                let right_velocity: u32 = right.iter().map(|x| x.2).sum();

                                todo!("Crash not yet implemented");

                                break 'outer;
                            }
                        }
                    }
                    // no crash - move player along the path
                    let player = self
                        .players
                        .iter_mut()
                        .find(|p| &p.id == lid)
                        .expect("No player found");
                    if let Some((last, end, _)) = left.last() {
                        if let Some(c) = self.board.connectors.iter().find(|x| &x.id == last) {
                            match &c.kind {
                                ConnectorKind::DeadEnd(_) => {
                                    player.current_position = (*last, *end);
                                    player.is_active = false;
                                }
                                ConnectorKind::HexToHex(_) | ConnectorKind::Outside(_) => {
                                    player.current_position = (
                                        *last,
                                        if *end == ConnectorEnd::StartedAtA {
                                            ConnectorEnd::StartedAtB
                                        } else {
                                            ConnectorEnd::StartedAtA
                                        },
                                    );
                                }
                                ConnectorKind::OnHex(_) => {
                                    panic!("Player ended up on not allowed connector: {c:?}")
                                }
                            }
                        } else {
                            panic!("No connector found with id={last:?}")
                        }
                    }
                    player.history.push(PlayerHistorySingleTurn {
                        connectors: left
                            .iter()
                            .map(|(c, end, w)| HistoryConnector {
                                id: *c,
                                end: *end,
                                weight: *w,
                            })
                            .collect(),
                    });
                }
            }
        }
        // step 3: update current player
        {
            if let Some(next) = self
                .players
                .iter()
                .cycle()
                .take(self.players.len() * 3)
                .skip_while(|p| p.id != self.current_player)
                .skip(1)
                .take(self.players.len())
                .find(|p| p.is_active && !p.is_npc && !p.hand.is_empty())
                .map(|p| p.id)
            {
                self.current_player = next;
            }
        }
        self.statistics = Statistics::compute(&self.players);
        self.check_winning_condition();
    }

    fn check_winning_condition(&mut self) {
        if self.result.is_some() {
            return;
        }
        let result = match &self.options {
            GameOptions::Delivery(d) => d.check_winning_condition(&self.players, &self.statistics),
            GameOptions::Standard(s) => s.check_winning_condition(&self.players, &self.statistics),
        };
        self.result = result;
    }
}

#[cfg(test)]
mod tests {
    use crate::game_options::{
        CollisionMode, GameOptionsStandard, OuterConnectors, WinningConditionStandard,
    };

    use super::*;

    #[test]
    fn test_serialize_game_state() {
        let options = GameOptionsStandard {
            board_radius: 1,
            outer_connectors: OuterConnectors::ReducedDeathEnds,
            random_seed: 0,
            player_count: 2,
            collision_mode: CollisionMode::PassThrough,
            winning_condition: WinningConditionStandard::HighestVelocity,
            hand_size: 3,
        };
        let game = options.start_game().unwrap();
        let json = serde_json::to_string_pretty(&game).unwrap();
        let path = format!("{}/../target/game_state.json", env!("CARGO_MANIFEST_DIR"));
        std::fs::write(&path, &json).unwrap();
        dbg!(&path);

        let restored: GameState = serde_json::from_str(&json).unwrap();
        assert_eq!(restored.players.len(), game.players.len());
    }
}
