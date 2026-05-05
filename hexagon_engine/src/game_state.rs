use crate::Board;
use crate::board_types::{
    Connector, ConnectorId, ConnectorKind, ConnectorOnHex, ConnectorPosition, Tile,
    TileRotationDirection,
};
use crate::game_options::GameOptions;
use crate::player_types::PlayerHistorySingleTurn;
use crate::random_number_generator::RandomNumberGenerator;
use crate::{Player, PlayerId};

pub struct GameState {
    pub board: Board,
    pub players: Vec<Player>,
    pub current_player: PlayerId,
    pub rng: RandomNumberGenerator,
    pub options: GameOptions,
}

impl GameState {
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
            let position = player.current_position.hexagon.clone();

            if let Some(connector) = self.board.connectors.iter().find_map(|x| match &x.kind {
                ConnectorKind::OnHex(connector_on_hex) => {
                    (connector_on_hex.hexagon == position).then_some(&x.id)
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
                            hexagon: position.clone(),
                            edge_sub: connector,
                        }),
                        weight: 1,
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
                    for (rindex, (rid, right)) in possible_paths.iter().enumerate().skip(lindex + 1)
                    {
                        if left.iter().any(|l| right.iter().any(|r| l.0 == r.0)) {
                            // crash
                            // note: only two players can crash, triple crashes are not possible
                            // the crash point can be in the middle of a connector (or anywhere along it, actually)
                            let left_velocity: u32 = left.iter().map(|x| x.1).sum();
                            let right_velocity: u32 = right.iter().map(|x| x.1).sum();

                            todo!("Crash not yet implemented");

                            break 'outer;
                        }
                    }
                    // no crash - move player along the path
                    let player = self
                        .players
                        .iter_mut()
                        .find(|p| &p.id == lid)
                        .expect("No player with id={lid:?} found");
                    if let Some((last, _)) = left.last() {
                        if let Some(c) = self.board.connectors.iter().find(|x| &x.id == last) {
                            match &c.kind {
                                ConnectorKind::DeadEnd(c) => {
                                    player.current_position = c.position.clone();
                                    player.is_active = false;
                                }
                                ConnectorKind::HexToHex(hex_to_hex) => {
                                    let arrived_at_a = if left.len() >= 2 {
                                        let second_last_id = left[left.len() - 2].0;
                                        let second_last = self
                                            .board
                                            .connectors
                                            .iter()
                                            .find(|x| x.id == second_last_id)
                                            .expect("second-to-last connector not found");
                                        match &second_last.kind {
                                            ConnectorKind::OnHex(prev) => {
                                                ConnectorPosition {
                                                    hexagon: prev.hexagon.clone(),
                                                    edge_sub: prev.edge_sub.a.clone(),
                                                } == hex_to_hex.connector_a
                                                    || ConnectorPosition {
                                                        hexagon: prev.hexagon.clone(),
                                                        edge_sub: prev.edge_sub.b.clone(),
                                                    } == hex_to_hex.connector_a
                                            }
                                            ConnectorKind::Outside(prev)
                                            | ConnectorKind::HexToHex(prev) => {
                                                prev.connector_a == hex_to_hex.connector_a
                                                    || prev.connector_b == hex_to_hex.connector_a
                                            }
                                            ConnectorKind::DeadEnd(prev) => {
                                                prev.position == hex_to_hex.connector_a
                                            }
                                        }
                                    } else {
                                        player.current_position == hex_to_hex.connector_a
                                    };
                                    player.current_position = if arrived_at_a {
                                        hex_to_hex.connector_b.clone()
                                    } else {
                                        hex_to_hex.connector_a.clone()
                                    };
                                }
                                ConnectorKind::OnHex(_) | ConnectorKind::Outside(_) => {
                                    panic!("Player ended up on not allowed connector: {c:?}")
                                }
                            }
                        } else {
                            panic!("No connector found with id={last:?}")
                        }
                    }
                    player.history.push(PlayerHistorySingleTurn {
                        connectors: left.iter().map(|(c, _)| c.clone()).collect(),
                    });
                }
            }
        }
    }
}
