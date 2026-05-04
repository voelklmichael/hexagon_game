use crate::Board;
use crate::board_types::TileRotationDirection;
use crate::{Player, PlayerId};

pub struct GameState {
    pub board: Board,
    pub players: Vec<Player>,
    pub current_player: PlayerId,
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
}
