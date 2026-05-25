use hexagon_types::player::PlayerId;
use serde::{Deserialize, Serialize};

use crate::game_options::{GameOptions, WinningCondition};
use crate::player_types::{HistoryConnector, PlayerHistorySingleTurn};
use crate::random_number_generator::RandomNumberGenerator;
use crate::statistics::Statistics;
use crate::{
    Board, Connector, ConnectorEnd, ConnectorId, ConnectorKind, ConnectorOnHex, ConnectorPosition,
    HexagonPosition, Player, Tile, TileRotationDirection,
};

#[derive(Clone, Serialize, Deserialize)]
pub enum GameResult {
    Win(Vec<PlayerId>),
    Draw(Vec<PlayerId>),
    Loss,
}

#[derive(Clone, Serialize, Deserialize)]
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
        let kind = &self
            .board
            .connectors
            .iter()
            .find(|c| c.id == connector_id)?
            .kind;
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
            let (connector_id, end) = player.current_position;
            let hexagon = self.board.position_of(connector_id, end).hexagon;

            let tile = player.hand.remove(tile);
            if !self.board.play_tile(hexagon, tile) {
                tracing::error!("There is already a connector at this spot.");
                return;
            }
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
                    // Players already terminated at a non-start dead-end get an empty path so
                    // they stay put and receive an empty history entry (no animation).
                    let at_dead_end = self
                        .board
                        .connectors
                        .iter()
                        .find(|c| c.id == p.current_position.0)
                        .is_some_and(|c| matches!(c.kind, ConnectorKind::DeadEnd(_)));
                    let start_id = p
                        .history
                        .first()
                        .and_then(|t| t.connectors.first())
                        .map(|hc| hc.id);
                    let path = if !at_dead_end || start_id == Some(p.current_position.0) {
                        self.board.compute_path_starting_from(&p.current_position)
                    } else {
                        vec![]
                    };
                    (p.id, path)
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
            GameOptions::Highscore(m) => {
                crate::game_options::GameOptionsStandard::check_winning_condition_for(
                    &WinningCondition::Highscore(m.winning_condition.clone()),
                    &self.players,
                    &self.statistics,
                )
            }
            GameOptions::HighscoreV2(m) => {
                let wc = &m.winning_condition;
                let all_done = self
                    .players
                    .iter()
                    .filter(|p| !p.is_npc)
                    .all(|p| !p.is_active);
                if !all_done {
                    return;
                }
                let is_won = self.players.iter().all(|p| {
                    let vel_ok = wc.min_velocity.get(&p.id).is_none_or(|&v| {
                        self.statistics
                            .max_velocity
                            .get(&p.id)
                            .copied()
                            .unwrap_or(0)
                            >= v
                    });
                    let dist_ok = wc.min_distance.get(&p.id).is_none_or(|&d| {
                        self.statistics
                            .total_path_weight
                            .get(&p.id)
                            .copied()
                            .unwrap_or(0)
                            >= d
                    });
                    let target_ok = wc
                        .target
                        .get(&p.id)
                        .is_none_or(|&t| p.current_position.0 == t);
                    vel_ok && dist_ok && target_ok
                });
                if is_won {
                    Some(GameResult::Win([PlayerId(0)].into()))
                } else {
                    Some(GameResult::Loss)
                }
            }
        };
        self.result = result;
    }
}

#[cfg(test)]
mod tests {
    use crate::game_options::{
        CollisionMode, GameOptionsStandard, OuterConnectors, WinningCondition,
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
            winning_condition: WinningCondition::HighestVelocity,
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
