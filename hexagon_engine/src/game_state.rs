use derive_aliases::derive;

use crate::{
    BoardConstructionOptionsSimple, HexagonBoard, HexagonConnector, HexagonConnectorPosition,
    HexagonTile, random_tile::random_tile_fully_connected,
};

#[derive(..SerdeClone)]
pub struct GameState {
    pub board: HexagonBoard,
    pub players: Vec<Player>,
    pub current_player_index: usize,
    pub options: GameOptionsGeneral,
    pub rng: crate::Rng,
}

#[derive(..SerdeClone)]
pub struct PlayerId(pub u32);

#[derive(..SerdeClone)]
pub struct Player {
    pub id: PlayerId,
    pub current_position: HexagonConnectorPosition,
    pub target: Option<HexagonConnectorPosition>,
    pub history: Vec<SingleTurnHistory>,
    pub hand: Vec<HexagonTile>,
    pub is_npc: bool,
    pub is_active: bool,
}
#[derive(..SerdeClone)]
pub struct SingleTurnHistory {
    pub history: Vec<HexagonConnector>,
}

#[derive(..SerdeClone)]
pub struct GameOptionsGeneral {
    pub board_options: BoardConstructionOptionsSimple,
    pub random_salt: u32,
    pub player_count: u32,
    pub hand_size: u32,
    pub npc_count: u32,
    pub players_have_target: bool,
    pub npcs_have_target: bool,
    pub player_collision_mode: PlayerCollisionMode,
}
impl GameOptionsGeneral {
    pub fn start_game(&self) -> GameState {
        let Self {
            board_options,
            random_salt,
            player_count,
            hand_size,
            npc_count,
            players_have_target,
            npcs_have_target,
            player_collision_mode: _,
        } = self.clone();
        let board = board_options.construct().unwrap();

        if player_count == 0 {
            panic!("Zero player count is not supported");
        }
        if hand_size == 0 {
            panic!("Zero hand size is not supported")
        }

        let mut dead_ends = board.get_dead_ends();
        let mut rng = crate::Rng::new(random_salt);

        let mut players = Vec::with_capacity((npc_count + player_count) as usize);
        for i in 0..player_count {
            let target = {
                if players_have_target {
                    Some(rng.select_random_element(&mut dead_ends).hexagon_a.clone())
                } else {
                    None
                }
            };
            let start = rng.select_random_element(&mut dead_ends); //TODO: check if there are enough dead ends left

            players.push(Player {
                id: PlayerId(i),
                current_position: start.hexagon_a.clone(),
                target,
                history: vec![SingleTurnHistory {
                    history: vec![HexagonConnector::DeadEnd(start.clone())],
                }],
                hand: (0..hand_size)
                    .map(|_| crate::random_tile::random_tile_fully_connected(&mut rng))
                    .collect(),
                is_npc: false,
                is_active: true,
            })
        }
        for i in 0..npc_count {
            let i = player_count + i;
            let target = {
                if npcs_have_target {
                    Some(rng.select_random_element(&mut dead_ends).hexagon_a.clone())
                } else {
                    None
                }
            };
            let start = rng.select_random_element(&mut dead_ends); //TODO: check if there are enough dead ends left

            players.push(Player {
                id: PlayerId(i),
                current_position: start.hexagon_a.clone(),
                target,
                history: vec![SingleTurnHistory {
                    history: vec![HexagonConnector::DeadEnd(start.clone())],
                }],
                hand: Default::default(),
                is_npc: true,
                is_active: true,
            })
        }

        GameState {
            board,
            players,
            options: self.clone(),
            current_player_index: 0,
            rng,
        }
    }
}

#[derive(..SerdeClone)]
pub struct DeliveryGameOptions {
    pub board_options: BoardConstructionOptionsSimple,
    pub random_salt: u32,
    pub hand_size: u32,
    pub npc_count: u32,
    pub player_has_target: bool,
    pub npcs_have_target: bool,
}
impl From<DeliveryGameOptions> for GameOptionsGeneral {
    fn from(value: DeliveryGameOptions) -> Self {
        let DeliveryGameOptions {
            board_options,
            random_salt,
            hand_size,
            npc_count,
            player_has_target,
            npcs_have_target,
        } = value;
        Self {
            board_options,
            random_salt,
            player_count: 1,
            hand_size,
            npc_count,
            players_have_target: player_has_target,
            npcs_have_target,
            player_collision_mode: PlayerCollisionMode::BothDie,
        }
    }
}

#[derive(..SerdeClone)]
pub enum PlayerCollisionMode {
    BothDie,
    PassThrough,
}

impl DeliveryGameOptions {
    pub fn start_game(&self) -> GameState {
        let options: GameOptionsGeneral = self.clone().into();
        options.start_game()
    }
}

impl GameState {
    pub fn play_tile(mut self, tile_index: usize) -> Self {
        let before = self.clone();
        let tile = self.players[self.current_player_index]
            .hand
            .remove(tile_index);
        self.players[self.current_player_index]
            .hand
            .push(random_tile_fully_connected(&mut self.rng));

        let current_position = self.players[self.current_player_index]
            .current_position
            .hexagon
            .clone();
        let Some(hexagon) = self
            .board
            .hexagons
            .iter()
            .find(|x| x.id == current_position)
        else {
            panic!(
                "No matching hexagon found: {}, {}",
                self.current_player_index, current_position.0
            );
        };
        self.board.play_tile(hexagon.position.clone(), tile);
        self.current_player_index += 1;

        if let Some((i, _)) = self
            .players
            .iter()
            .enumerate()
            .cycle()
            .skip(self.current_player_index)
            .take(self.players.len())
            .find(|(_, x)| !x.is_npc && x.is_active)
        {
            self.current_player_index = i;
        } else {
            todo!("Game is finished")
        }

        for player in &mut self.players {
            let mut movement = vec![];
            if player.current_position.hexagon == current_position {
                let mut pos = player.current_position.clone();
                let mut prev_pos: Option<HexagonConnectorPosition> = None;
                loop {
                    let found = self.board.connectors.iter().find(|c| {
                        let HexagonConnector::Direct(d) = c else { return false };
                        if d.was_removed { return false }
                        let at_a = d.connector_a == pos;
                        let at_b = d.connector_b == pos;
                        if !at_a && !at_b { return false }
                        let other = if at_a { &d.connector_b } else { &d.connector_a };
                        prev_pos.as_ref().map_or(true, |p| p != other)
                    });
                    if let Some(HexagonConnector::Direct(d)) = found {
                        let next = if d.connector_a == pos {
                            d.connector_b.clone()
                        } else {
                            d.connector_a.clone()
                        };
                        movement.push(HexagonConnector::Direct(d.clone()));
                        prev_pos = Some(pos);
                        pos = next;
                    } else {
                        break;
                    }
                }
                player.current_position = pos;
            }
            player.history.push(SingleTurnHistory { history: movement });
        }

        self
    }
}
