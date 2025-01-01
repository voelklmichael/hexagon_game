#![feature(let_chains)]
mod app;
mod bimap;
mod game_state;
mod permutations;
pub use app::Hexagon;

//TODO: remove
pub fn testing() {
    game_state::Board::new(3);
}

pub fn testing2() {
    game_state::GameState::testing()
}
