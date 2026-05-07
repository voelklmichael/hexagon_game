use serde::{Deserialize, Serialize};

use crate::{
    board_types::{Board, ConnectorEnd, Tile},
    game_state::{GameResult, GameState},
    player_types::{Player, PlayerHistorySingleTurn, PlayerId},
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
    pub winning_condition: WinningConditionStandard,
    pub hand_size: usize,
}

#[derive(strum::EnumIter, Clone, Debug, Serialize, Deserialize)]
pub enum OuterConnectors {
    OnlyDeathEnds,
    ReducedDeathEnds,
}

#[derive(Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum CollisionMode {
    PassThrough,
    BothDie,
}

#[derive(Clone, Serialize, Deserialize)]
pub enum WinningConditionStandard {
    LastManStanding,
    LongestWay,
    HighestVelocity,
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

#[derive(Serialize, Deserialize)]
pub enum GameOptions {
    Delivery(GameOptionsDelivery),
    Standard(GameOptionsStandard),
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
                id: PlayerId(0),
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
        CollisionMode::BothDie
    }

    pub(crate) fn check_winning_condition(
        &self,
        players: &[Player],
        _stats: &Statistics,
    ) -> Option<GameResult> {
        let with_targets: Vec<_> = players
            .iter()
            .filter(|p| !p.is_npc && p.target.is_some())
            .collect();

        // A non-NPC player left the board without a target → Loss
        if players
            .iter()
            .any(|p| !p.is_npc && !p.is_active && p.target.is_none())
        {
            return Some(GameResult::Loss);
        }

        if with_targets.is_empty() {
            return None;
        }

        // Any player with a target became inactive without reaching it → Loss
        if with_targets
            .iter()
            .any(|p| !p.is_active && !p.target.is_some_and(|t| t == p.current_position.0))
        {
            return Some(GameResult::Loss);
        }

        // All players with targets reached them → Win
        if with_targets
            .iter()
            .all(|p| !p.is_active && p.target.is_some_and(|t| t == p.current_position.0))
        {
            return Some(GameResult::Win(with_targets.iter().map(|p| p.id).collect()));
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
        match self.winning_condition {
            WinningConditionStandard::LastManStanding => {
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
            WinningConditionStandard::LongestWay => {
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
                if top.len() == 1 { Some(GameResult::Win(top)) } else { Some(GameResult::Draw(top)) }
            }
            WinningConditionStandard::HighestVelocity => {
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
                if top.len() == 1 { Some(GameResult::Win(top)) } else { Some(GameResult::Draw(top)) }
            }
        }
    }
}

impl GameOptions {
    pub fn start_game(self) -> Result<GameState, String> {
        match self {
            GameOptions::Delivery(game) => game.start_game(),
            GameOptions::Standard(game) => game.start_game(),
        }
    }

    pub(crate) fn collision_mode(&self) -> CollisionMode {
        match self {
            GameOptions::Delivery(o) => o.collision_mode(),
            GameOptions::Standard(o) => o.collision_mode(),
        }
    }
}
