use hexagon_types::player::PlayerId;
use serde::{Deserialize, Serialize};

use crate::RandomNumberGenerator;
use crate::game_options::{GameOptions, WinningCondition};
use crate::player_types::{HistoryConnector, PlayerPosition, PlayerHistorySingleTurn};
use crate::statistics::Statistics;
use crate::{
    Board, ConnectorEnd, ConnectorKind, HexagonPosition, Player, Tile, TileRotationDirection,
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
        let connector_id = player.current_position.connector;
        let end = player.current_position.end;
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
            let connector_id = player.current_position.connector;
            let end = player.current_position.end;
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
                        .find(|c| c.id == p.current_position.connector)
                        .is_some_and(|c| matches!(c.kind, ConnectorKind::DeadEnd(_)));
                    let start_id = p
                        .history
                        .first()
                        .and_then(|t| t.connectors.first())
                        .map(|hc| hc.id);
                    let path = if !at_dead_end || start_id == Some(p.current_position.connector) {
                        self.board.compute_path_starting_from(&(p.current_position.connector, p.current_position.end))
                    } else {
                        vec![]
                    };
                    (p.id, path)
                })
                .collect::<Vec<_>>();
            // check if any two paths overlap; if so, compute a hit point
            {
                use std::collections::HashMap;
                // crash_map: player id → (crash connector id, entry end, fraction from entry end)
                let mut crash_map: HashMap<PlayerId, (crate::ConnectorId, ConnectorEnd, f32)> =
                    HashMap::new();

                if self.options.collision_mode() == crate::game_options::CollisionMode::BothDie {
                    for (lindex, (lid, left)) in possible_paths.iter().enumerate() {
                        if crash_map.contains_key(lid) {
                            continue;
                        }
                        for (_, (rid, right)) in possible_paths.iter().enumerate().skip(lindex + 1)
                        {
                            if crash_map.contains_key(rid) {
                                continue;
                            }
                            if left.iter().any(|l| right.iter().any(|r| l.0 == r.0)) {
                                let left_velocity: u32 = left.iter().map(|x| x.2).sum();
                                let right_velocity: u32 = right.iter().map(|x| x.2).sum();
                                if let Some((crash_id, lend, lfrac, rend, rfrac)) =
                                    compute_crash_connector(
                                        left,
                                        right,
                                        left_velocity,
                                        right_velocity,
                                    )
                                {
                                    crash_map.insert(*lid, (crash_id, lend, lfrac));
                                    crash_map.insert(*rid, (crash_id, rend, rfrac));
                                }
                            }
                        }
                    }
                }

                for (lid, left) in &possible_paths {
                    let player = self
                        .players
                        .iter_mut()
                        .find(|p| &p.id == lid)
                        .expect("No player found");

                    if let Some((crash_id, crash_end, crash_frac)) = crash_map.get(lid) {
                        // truncate path at the crash connector (inclusive)
                        let truncated: Vec<_> = left
                            .iter()
                            .take_while(|(id, _, _)| id != crash_id)
                            .chain(left.iter().find(|(id, _, _)| id == crash_id))
                            .cloned()
                            .collect();
                        player.current_position = PlayerPosition {
                            connector: *crash_id,
                            end: *crash_end,
                            hit_fraction: Some(*crash_frac),
                        };
                        player.is_active = false;
                        player.history.push(PlayerHistorySingleTurn {
                            connectors: truncated
                                .iter()
                                .map(|(c, end, w)| HistoryConnector {
                                    id: *c,
                                    end: *end,
                                    weight: *w,
                                })
                                .collect(),
                        });
                    } else {
                        // normal movement
                        if let Some((last, end, _)) = left.last() {
                            if let Some(c) = self.board.connectors.iter().find(|x| &x.id == last) {
                                match &c.kind {
                                    ConnectorKind::DeadEnd(_) => {
                                        player.current_position =
                                            PlayerPosition::new(*last, *end);
                                        player.is_active = false;
                                    }
                                    ConnectorKind::HexToHex(_) | ConnectorKind::Outside(_) => {
                                        player.current_position = PlayerPosition::new(
                                            *last,
                                            if *end == ConnectorEnd::StartedAtA {
                                                ConnectorEnd::StartedAtB
                                            } else {
                                                ConnectorEnd::StartedAtA
                                            },
                                        );
                                    }
                                    ConnectorKind::OnHex(_) => {
                                        panic!(
                                            "Player ended up on not allowed connector: {c:?}"
                                        )
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
                crate::game_options::check_winning_condition_for_highscore_v2(
                    &m.winning_condition,
                    &self.players,
                    &self.statistics,
                )
            }
        };
        self.result = result;
    }
}

/// Finds the earliest collision point between two player paths moving toward each other.
///
/// Returns `(crash_connector_id, left_entry_end, left_fraction, right_entry_end, right_fraction)`
/// where each fraction ∈ [0, 1] is measured from the respective player's entry end.
fn compute_crash_connector(
    left: &[(crate::ConnectorId, ConnectorEnd, u32)],
    right: &[(crate::ConnectorId, ConnectorEnd, u32)],
    left_velocity: u32,
    right_velocity: u32,
) -> Option<(crate::ConnectorId, ConnectorEnd, f32, ConnectorEnd, f32)> {
    if left_velocity == 0 || right_velocity == 0 {
        return None;
    }
    let lv = left_velocity as f64;
    let rv = right_velocity as f64;

    let mut best: Option<(f64, crate::ConnectorId, ConnectorEnd, f32, ConnectorEnd, f32)> = None;

    let mut d_a: u32 = 0;
    for (lid, lend, lw) in left {
        let mut d_b: u32 = 0;
        for (rid, rend, rw) in right {
            if lid == rid {
                let w = *lw as f64;
                let da = d_a as f64;
                let db = d_b as f64;
                // time t ∈ [0,1] at which players meet: solve lv*t - da = w - (rv*t - db)
                let t = (w + da + db) / (lv + rv);
                let frac_left = ((lv * t - da) / w) as f32;
                if (0.0..=1.0).contains(&frac_left) {
                    let frac_right = 1.0 - frac_left;
                    if best.is_none() || t < best.as_ref().unwrap().0 {
                        best = Some((t, *lid, *lend, frac_left, *rend, frac_right));
                    }
                }
            }
            d_b += rw;
        }
        d_a += lw;
    }

    best.map(|(_, id, lend, fl, rend, fr)| (id, lend, fl, rend, fr))
}

#[cfg(test)]
mod tests {
    use crate::game_options::{
        CollisionMode, GameOptionsStandard, OuterConnectors, WinningCondition,
    };
    use crate::{ConnectorEdgeSub, ConnectorId, Edge, EdgeSub, Sub, Tile};

    use super::*;

    /// A radius-1 board has exactly one hexagon and no HexToHex/Outside connectors, so
    /// both players start on the same hexagon. A tile that directly pairs their two
    /// dead-end positions creates a symmetric head-on crash: equal velocities, equal
    /// pre-crash distances → hit fraction = 0.5 for both.
    #[test]
    fn test_collision_symmetric_hit_fraction() {
        let options = GameOptionsStandard {
            board_radius: 1,
            outer_connectors: OuterConnectors::ReducedDeathEnds,
            random_seed: 0,
            player_count: 2,
            collision_mode: CollisionMode::BothDie,
            winning_condition: WinningCondition::HighestVelocity,
            hand_size: 1,
        };
        let mut game = options.start_game().unwrap();

        // Retrieve each player's dead-end edge-sub position.
        let es_a = {
            let id = game.players[0].current_position.connector;
            game.board
                .connectors
                .iter()
                .find(|c| c.id == id)
                .map(|c| match &c.kind {
                    ConnectorKind::DeadEnd(d) => d.position.edge_sub.clone(),
                    _ => panic!("expected dead-end"),
                })
                .unwrap()
        };
        let es_b = {
            let id = game.players[1].current_position.connector;
            game.board
                .connectors
                .iter()
                .find(|c| c.id == id)
                .map(|c| match &c.kind {
                    ConnectorKind::DeadEnd(d) => d.position.edge_sub.clone(),
                    _ => panic!("expected dead-end"),
                })
                .unwrap()
        };

        // Build a tile whose first connector directly links both players; pair the
        // remaining 10 edge positions arbitrarily.
        let all_edges: Vec<EdgeSub> = [
            Edge::Top,
            Edge::TopLeft,
            Edge::BottomLeft,
            Edge::Bottom,
            Edge::BottomRight,
            Edge::TopRight,
        ]
        .iter()
        .flat_map(|&e| {
            [
                EdgeSub {
                    edge: e,
                    sub: Sub::Left,
                },
                EdgeSub {
                    edge: e,
                    sub: Sub::Right,
                },
            ]
        })
        .collect();

        let mut remaining: Vec<EdgeSub> = all_edges
            .into_iter()
            .filter(|es| es != &es_a && es != &es_b)
            .collect();

        let mut inner_connectors = vec![ConnectorEdgeSub {
            a: es_a,
            b: es_b,
        }];
        while remaining.len() >= 2 {
            let a = remaining.pop().unwrap();
            let b = remaining.pop().unwrap();
            inner_connectors.push(ConnectorEdgeSub { a, b });
        }
        game.players[0].hand = vec![Tile { inner_connectors }];

        game.play_tile(0);

        assert!(!game.players[0].is_active, "player 0 should be inactive");
        assert!(!game.players[1].is_active, "player 1 should be inactive");

        let frac_a = game.players[0]
            .current_position
            .hit_fraction
            .expect("player 0 missing hit_fraction");
        let frac_b = game.players[1]
            .current_position
            .hit_fraction
            .expect("player 1 missing hit_fraction");

        assert!((frac_a - 0.5).abs() < 1e-4, "expected 0.5, got {frac_a}");
        assert!((frac_b - 0.5).abs() < 1e-4, "expected 0.5, got {frac_b}");
    }

    /// When one player travels through an outer connector (weight 500) before the crash
    /// connector, their total velocity is higher but they also have 500 units of
    /// pre-crash distance. The net effect: the outer-connector player traverses *less*
    /// of the crash connector from their entry end (fraction < 0.5), while the direct
    /// player — who starts right at the crash connector and is effectively slower —
    /// travels more than halfway through it (fraction > 0.5).
    #[test]
    fn test_collision_asymmetric_outer_connector() {
        let crash_id = ConnectorId(0);
        let outside_id = ConnectorId(1);
        let exit_direct = ConnectorId(2);
        let exit_outer = ConnectorId(3);

        // Direct player: [crash(1000), dead_end(1)] — total velocity 1001
        let path_direct = vec![
            (crash_id, ConnectorEnd::StartedAtA, 1000u32),
            (exit_direct, ConnectorEnd::StartedAtA, 1),
        ];
        // Outer-connector player: [outside(500), crash(1000), dead_end(1)] — total velocity 1501
        let path_outer = vec![
            (outside_id, ConnectorEnd::StartedAtA, 500),
            (crash_id, ConnectorEnd::StartedAtB, 1000),
            (exit_outer, ConnectorEnd::StartedAtA, 1),
        ];

        let v_direct: u32 = path_direct.iter().map(|x| x.2).sum();
        let v_outer: u32 = path_outer.iter().map(|x| x.2).sum();

        let (id, _lend, frac_direct, _rend, frac_outer) =
            compute_crash_connector(&path_direct, &path_outer, v_direct, v_outer)
                .expect("paths share crash_id, so crash must be detected");

        assert_eq!(id, crash_id);
        assert!(
            frac_direct > 0.5,
            "direct player fraction {frac_direct} should exceed 0.5"
        );
        assert!(
            frac_outer < 0.5,
            "outer-connector player fraction {frac_outer} should be below 0.5"
        );
    }

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
