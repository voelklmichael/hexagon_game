use hexagon_types::WinningConditionHighscoreV1;
use hexagon_types::player::PlayerId;
use serde::{Deserialize, Serialize};

pub use hexagon_types::game_options::{CollisionMode, OuterConnectors, WinningCondition};
pub use hexagon_types::missions::MissionHighscoreV1;

use crate::{
    board_types::{Board, ConnectorEnd, Tile},
    game_state::{GameResult, GameState},
    player_types::{Player, PlayerHistorySingleTurn},
    random_number_generator::RandomNumberGenerator,
    statistics::Statistics,
};

#[derive(Clone, Serialize, Deserialize)]
pub struct GameOptionsStandard {
    pub board_radius: usize,
    pub outer_connectors: OuterConnectors,
    pub random_seed: u32,
    pub player_count: usize,
    pub collision_mode: CollisionMode,
    pub winning_condition: WinningCondition,
    pub hand_size: usize,
}

#[derive(Clone, Serialize, Deserialize)]
pub struct GameOptionsDelivery {
    pub board_radius: usize,
    pub outer_connectors: OuterConnectors,
    pub random_seed: u32,
    pub npc_count: usize,
    pub player_has_target: bool,
    pub hand_size: usize,
}

#[derive(Clone, Serialize, Deserialize, strum::EnumDiscriminants)]
#[strum_discriminants(derive(serde::Serialize, serde::Deserialize))]
pub enum GameOptions {
    Delivery(GameOptionsDelivery),
    Standard(GameOptionsStandard),
    Highscore(MissionHighscoreV1),
}

impl GameOptionsDelivery {
    pub fn start_game(self) -> Result<GameState, String> {
        let Self {
            board_radius,
            outer_connectors,
            random_seed,
            npc_count,
            player_has_target,
            hand_size,
        } = self.clone();

        let board = Board::create_board(board_radius, outer_connectors)?;
        let mut rng = RandomNumberGenerator::new(random_seed);
        let mut dead_ends = board.get_dead_ends();

        let start_id = rng
            .select_random_element(&mut dead_ends)
            .ok_or("No start found for Player".to_string())?;
        let target = {
            if player_has_target {
                Some(
                    rng.select_random_element(&mut dead_ends)
                        .ok_or("No target found for Player".to_string())?,
                )
            } else {
                None
            }
        };
        let hand = (0..hand_size)
            .map(|_| Tile::create_fully_connected(&mut rng))
            .collect();
        let mut players = [Player {
            id: PlayerId(0),
            current_position: (start_id, ConnectorEnd::StartedAtA),
            target,
            history: PlayerHistorySingleTurn::new_from_start(&start_id),
            is_npc: false,
            is_active: true,
            hand,
        }]
        .into_iter()
        .collect::<Vec<_>>();
        for i in 0..npc_count {
            let target = rng
                .select_random_element(&mut dead_ends)
                .ok_or(format!("No target found for NPC#{i}"))?;
            let start_id = rng
                .select_random_element(&mut dead_ends)
                .ok_or(format!("No start found for NPC#{i}"))?;
            players.push(Player {
                id: PlayerId(1 + i as u32),
                current_position: (start_id, ConnectorEnd::StartedAtA),
                target: Some(target),
                history: PlayerHistorySingleTurn::new_from_start(&start_id),
                is_npc: true,
                is_active: false,
                hand: Default::default(),
            });
        }

        Ok(GameState {
            board,
            current_player: players.first().unwrap().id,
            statistics: Statistics::compute(&players),
            result: None,
            players,
            rng,
            options: GameOptions::Delivery(self),
        })
    }

    fn collision_mode(&self) -> CollisionMode {
        CollisionMode::PassThrough
    }

    pub(crate) fn check_winning_condition(
        &self,
        players: &[Player],
        _stats: &Statistics,
    ) -> Option<GameResult> {
        let npc_with_target: Vec<_> = players
            .iter()
            .filter(|p| p.is_npc && p.target.is_some())
            .collect();
        let humans_with_target: Vec<_> = players
            .iter()
            .filter(|p| !p.is_npc && p.target.is_some())
            .collect();

        let all_npc_targets_reached = npc_with_target
            .iter()
            .all(|p| p.target.is_some_and(|t| t == p.current_position.0));
        let all_human_targets_reached = humans_with_target
            .iter()
            .all(|p| !p.is_active && p.target.is_some_and(|t| t == p.current_position.0));

        // Win: all targets (NPC and human) reached — checked before loss so simultaneous
        // arrival on the same turn counts as a win.
        let has_any_target = !npc_with_target.is_empty() || !humans_with_target.is_empty();
        if has_any_target && all_npc_targets_reached && all_human_targets_reached {
            return Some(GameResult::Win(
                players.iter().filter(|p| !p.is_npc).map(|p| p.id).collect(),
            ));
        }

        // Loss: a non-NPC player left the board and not all targets were reached
        if players.iter().any(|p| !p.is_npc && !p.is_active) {
            return Some(GameResult::Loss);
        }

        None
    }
}

impl GameOptionsStandard {
    pub fn start_game(self) -> Result<GameState, String> {
        let Self {
            board_radius,
            outer_connectors,
            random_seed,
            player_count,
            collision_mode,
            winning_condition,
            hand_size,
        } = self.clone();

        let board = Board::create_board(board_radius, outer_connectors)?;
        let mut rng = RandomNumberGenerator::new(random_seed);
        let mut dead_ends = board.get_dead_ends();

        let mut players = Vec::new();
        for i in 0..player_count {
            let start_id = rng
                .select_random_element(&mut dead_ends)
                .ok_or(format!("No start found for Player#{i}"))?;

            let hand = (0..hand_size)
                .map(|_| Tile::create_fully_connected(&mut rng))
                .collect();
            players.push(Player {
                id: PlayerId(i as u32),
                current_position: (start_id, ConnectorEnd::StartedAtA),
                target: None,
                history: PlayerHistorySingleTurn::new_from_start(&start_id),
                is_npc: false,
                is_active: true,
                hand,
            });
        }

        Ok(GameState {
            board,
            current_player: players.first().unwrap().id,
            statistics: Statistics::compute(&players),
            result: None,
            players,
            rng,
            options: GameOptions::Standard(self),
        })
    }

    fn collision_mode(&self) -> CollisionMode {
        self.collision_mode
    }

    pub(crate) fn check_winning_condition(
        &self,
        players: &[Player],
        stats: &Statistics,
    ) -> Option<GameResult> {
        Self::check_winning_condition_for(&self.winning_condition, players, stats)
    }

    pub(crate) fn check_winning_condition_for(
        winning_condition: &WinningCondition,
        players: &[Player],
        stats: &Statistics,
    ) -> Option<GameResult> {
        match winning_condition {
            WinningCondition::LastManStanding => {
                let active: Vec<_> = players
                    .iter()
                    .filter(|p| p.is_active && !p.is_npc)
                    .collect();
                let total_non_npc = players.iter().filter(|p| !p.is_npc).count();
                if total_non_npc == 0 {
                    return None;
                }
                match active.len() {
                    0 => Some(GameResult::Draw(
                        players.iter().filter(|p| !p.is_npc).map(|p| p.id).collect(),
                    )),
                    1 => Some(GameResult::Win(vec![active[0].id])),
                    _ => None,
                }
            }
            WinningCondition::LongestWay => {
                let all_done = players.iter().filter(|p| !p.is_npc).all(|p| !p.is_active);
                if !all_done {
                    return None;
                }
                let max = stats.total_path_weight.values().copied().max().unwrap_or(0);
                let top: Vec<PlayerId> = stats
                    .total_path_weight
                    .iter()
                    .filter(|&(_, &w)| w == max)
                    .map(|(&pid, _)| pid)
                    .collect();
                if top.len() == 1 {
                    Some(GameResult::Win(top))
                } else {
                    Some(GameResult::Draw(top))
                }
            }
            WinningCondition::HighestVelocity => {
                let all_done = players.iter().filter(|p| !p.is_npc).all(|p| !p.is_active);
                if !all_done {
                    return None;
                }
                let max = stats.max_velocity.values().copied().max().unwrap_or(0);
                let top: Vec<PlayerId> = stats
                    .max_velocity
                    .iter()
                    .filter(|&(_, &v)| v == max)
                    .map(|(&pid, _)| pid)
                    .collect();
                if top.len() == 1 {
                    Some(GameResult::Win(top))
                } else {
                    Some(GameResult::Draw(top))
                }
            }
            WinningCondition::Highscore(WinningConditionHighscoreV1 {
                min_velocity,
                min_distance,
                target,
            }) => {
                let all_done = players.iter().filter(|p| !p.is_npc).all(|p| !p.is_active);
                if !all_done {
                    return None;
                }
                assert_eq!(players.len(), 1);
                let winners: Vec<PlayerId> = players
                    .iter()
                    .filter(|p| !p.is_npc)
                    .filter(|p| {
                        let vel_ok = min_velocity.is_none_or(|v| {
                            stats.max_velocity.get(&p.id).copied().unwrap_or(0) >= v
                        });
                        let dist_ok = min_distance.is_none_or(|d| {
                            stats.total_path_weight.get(&p.id).copied().unwrap_or(0) >= d
                        });
                        let target_ok = target.is_none_or(|t| p.current_position.0 == t);
                        vel_ok && dist_ok && target_ok
                    })
                    .map(|p| p.id)
                    .collect();
                if winners.is_empty() {
                    Some(GameResult::Loss)
                } else if winners.len() == 1 {
                    Some(GameResult::Win(winners))
                } else {
                    Some(GameResult::Draw(winners))
                }
            }
        }
    }
}

fn hash_seed(seed: u32) -> u32 {
    let s = seed.wrapping_add(0x6D2B79F5);
    let t = (s ^ (s >> 15)).wrapping_mul(1 | s);
    let t = t.wrapping_add((t ^ (t >> 7)).wrapping_mul(61 | t));
    t ^ (t >> 14)
}

pub fn start_highscore_game(mission: MissionHighscoreV1) -> Result<GameState, String> {
    let start_id = mission.starting_point;
    let target = match &mission.winning_condition {
        WinningConditionHighscoreV1 { target, .. } => *target,
        _ => None,
    };
    let player = Player {
        id: PlayerId(0),
        current_position: (start_id, ConnectorEnd::StartedAtA),
        target,
        history: PlayerHistorySingleTurn::new_from_start(&start_id),
        is_npc: false,
        is_active: true,
        hand: mission.starting_hand.clone(),
    };
    Ok(GameState {
        statistics: Statistics::compute(std::slice::from_ref(&player)),
        current_player: player.id,
        board: mission.board.clone(),
        result: None,
        players: vec![player],
        rng: RandomNumberGenerator::new(mission.random_seed),
        options: GameOptions::Highscore(mission),
    })
}

impl GameOptions {
    pub fn start_game(self) -> Result<GameState, String> {
        match self {
            GameOptions::Delivery(game) => game.start_game(),
            GameOptions::Standard(game) => game.start_game(),
            GameOptions::Highscore(mission) => start_highscore_game(mission),
        }
    }

    pub fn randomize_seed(&mut self) {
        match self {
            GameOptions::Delivery(o) => o.random_seed = hash_seed(o.random_seed),
            GameOptions::Standard(o) => o.random_seed = hash_seed(o.random_seed),
            GameOptions::Highscore(o) => o.random_seed = hash_seed(o.random_seed), //TODO: this should not be possible!
        }
    }

    pub(crate) fn collision_mode(&self) -> CollisionMode {
        match self {
            GameOptions::Delivery(o) => o.collision_mode(),
            GameOptions::Standard(o) => o.collision_mode(),
            GameOptions::Highscore(_) => CollisionMode::PassThrough,
        }
    }
}
