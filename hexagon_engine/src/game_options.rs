use crate::{
    board_types::{Board, Tile},
    game_state::GameState,
    player_types::{Player, PlayerHistorySingleTurn, PlayerId},
    random_number_generator::RandomNumberGenerator,
};

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

pub enum CollisionMode {
    PassThrough,
    BothDie,
}

pub enum WinningConditionStandard {
    LastManStanding,
    LongestWay,
    HighestVelocity,
}
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
        } = self;

        let board = Board::create_board(board_radius, outer_connectors)?;
        let mut rng = RandomNumberGenerator::new(random_seed);
        let mut dead_ends = board.get_dead_ends();

        let (start, start_id) = rng
            .select_random_element(&mut dead_ends)
            .ok_or(format!("No start found for Player"))?;
        let target = {
            if player_has_target {
                Some(
                    rng.select_random_element(&mut dead_ends)
                        .ok_or(format!("No target found for Player"))?
                        .0
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
            current_position: start.clone(),
            target: target,
            history: PlayerHistorySingleTurn::new_from_start(start_id),
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
                .0
                .clone();
            let (start, start_id) = rng
                .select_random_element(&mut dead_ends)
                .ok_or(format!("No start found for NPC#{i}"))?;
            players.push(Player {
                id: PlayerId(0),
                current_position: start.clone(),
                target: Some(target),
                history: PlayerHistorySingleTurn::new_from_start(start_id),
                is_npc: true,
                is_active: false,
                hand: Default::default(),
            });
        }

        Ok(GameState {
            board,
            current_player: players.first().unwrap().id.clone(),
            players,
        })
    }
}
