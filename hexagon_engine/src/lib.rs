#![allow(unused)]

mod board_types;
mod game;
mod game_options;
mod game_state;
mod player_types;
mod random_number_generator;
mod render;
mod statistics;

use board_types::*;
use game_state::*;
use player_types::*;
use statistics::*;

pub use board_types::{
    ConnectorDeadEnd, ConnectorEdgeSub, ConnectorKind, ConnectorOnHex, ConnectorOutside,
    ConnectorPosition, Edge, EdgeSub, HexagonPosition, Sub, TileRotationDirection,
};
pub use game_options::{
    CollisionMode, GameOptions, GameOptionsDelivery, GameOptionsDiscriminants, GameOptionsStandard,
    OuterConnectors, WinningCondition, start_highscore_game_v1, start_highscore_game_v2,
};
pub use game_state::{GameResult, GameState};
pub use random_number_generator::RandomNumberGenerator;
pub use render::{Color, CurrentPlayerPosition, PlayerData, RenderTask, UsedConnector};
pub use statistics::Statistics;
