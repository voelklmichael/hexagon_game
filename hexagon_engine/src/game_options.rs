use crate::{
    board_types::{Board, ConnectorEnd, Tile},
    game_state::GameState,
    player_types::{Player, PlayerHistorySingleTurn, PlayerId},
    random_number_generator::RandomNumberGenerator,
};

#[derive(Clone)]
pub struct GameOptionsStandard {
    pub board_radius: usize,
    pub outer_connectors: OuterConnectors,
    pub random_seed: u32,
    pub player_count: usize,
    pub collision_mode: CollisionMode,
    pub winning_condition: WinningConditionStandard,
    pub hand_size: usize,
}

#[derive(strum::EnumIter, Clone, Debug)]
pub enum OuterConnectors {
    OnlyDeathEnds,
    ReducedDeathEnds,
}

#[derive(Clone, Copy, PartialEq)]
pub enum CollisionMode {
    PassThrough,
    BothDie,
}

#[derive(Clone)]
pub enum WinningConditionStandard {
    LastManStanding,
    LongestWay,
    HighestVelocity,
}
#[derive(Clone)]
pub struct GameOptionsDelivery {
    pub board_radius: usize,
    pub outer_connectors: OuterConnectors,
    pub random_seed: u32,
    pub npc_count: usize,
    pub player_has_target: bool,
    pub hand_size: usize,
}

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
                        .ok_or("No target found for Player".to_string())?
                        .clone(),
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
                .ok_or(format!("No target found for NPC#{i}"))?
                .clone();
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
            players,
            rng,
            options: GameOptions::Delivery(self),
        })
    }

    fn collision_mode(&self) -> CollisionMode {
        CollisionMode::BothDie
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
                id: PlayerId(0),
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
            players,
            rng,
            options: GameOptions::Standard(self),
        })
    }

    fn collision_mode(&self) -> CollisionMode {
        self.collision_mode
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
