use crate::Board;
use crate::{Player, PlayerId};

pub struct GameState {
    pub board: Board,
    pub players: Vec<Player>,
    pub current_player: PlayerId,
}
