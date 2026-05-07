use std::collections::HashMap;

use hexagon_engine::{
    CollisionMode, Color, GameOptionsDelivery, GameOptionsDiscriminants, GameOptionsStandard,
    GameState, HexagonPosition, OuterConnectors, PlayerId, WinningConditionStandard,
};

#[derive(serde::Serialize, serde::Deserialize)]
#[serde(default, rename_all = "camelCase")]
pub struct OptionsState {
    pub selected: GameOptionsDiscriminants,
    pub standard: GameOptionsStandard,
    pub delivery: GameOptionsDelivery,
}

impl Default for OptionsState {
    fn default() -> Self {
        Self {
            selected: GameOptionsDiscriminants::Delivery,
            standard: GameOptionsStandard {
                board_radius: 2,
                outer_connectors: OuterConnectors::ReducedDeathEnds,
                random_seed: 0,
                player_count: 2,
                collision_mode: CollisionMode::PassThrough,
                winning_condition: WinningConditionStandard::HighestVelocity,
                hand_size: 3,
            },
            delivery: GameOptionsDelivery {
                board_radius: 2,
                outer_connectors: OuterConnectors::ReducedDeathEnds,
                random_seed: 0,
                npc_count: 2,
                player_has_target: true,
                hand_size: 3,
            },
        }
    }
}

#[derive(serde::Serialize, serde::Deserialize)]
#[serde(default, rename_all = "camelCase")]
pub struct MusicState {
    pub volume: f32,
    pub muted: bool,
}

impl Default for MusicState {
    fn default() -> Self {
        Self {
            volume: 1.0,
            muted: false,
        }
    }
}

#[derive(serde::Serialize, serde::Deserialize)]
#[serde(default, rename_all = "camelCase")]
pub struct BoardInteraction {
    #[serde(skip)]
    pub selected_tile: Option<usize>,
    #[serde(skip)]
    pub selected_hexagon: Option<HexagonPosition>,
    #[serde(skip)]
    pub animation_t: f32,
}

impl Default for BoardInteraction {
    fn default() -> Self {
        Self {
            selected_tile: None,
            selected_hexagon: None,
            animation_t: 0.0,
        }
    }
}

#[derive(serde::Serialize, serde::Deserialize)]
#[serde(default, rename_all = "camelCase")]
pub struct GameHistory {
    #[serde(skip)]
    pub undo_stack: Vec<GameState>,
    #[serde(skip)]
    pub redo_stack: Vec<GameState>,
}

impl Default for GameHistory {
    fn default() -> Self {
        Self {
            undo_stack: Vec::new(),
            redo_stack: Vec::new(),
        }
    }
}

#[derive(serde::Serialize, serde::Deserialize)]
#[serde(default, rename_all = "camelCase")]
pub struct HexApp {
    pub game: Option<GameState>,
    pub history: GameHistory,
    pub player_colors: HashMap<PlayerId, Color>,
    pub interaction: BoardInteraction,
    pub options: OptionsState,
    pub music: MusicState,
}

impl Default for HexApp {
    fn default() -> Self {
        Self {
            game: None,
            history: GameHistory::default(),
            player_colors: HashMap::new(),
            interaction: BoardInteraction::default(),
            options: OptionsState::default(),
            music: MusicState::default(),
        }
    }
}

impl HexApp {
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        if let Some(storage) = cc.storage {
            eframe::get_value(storage, eframe::APP_KEY).unwrap_or_default()
        } else {
            Default::default()
        }
    }
}

impl eframe::App for HexApp {
    fn save(&mut self, storage: &mut dyn eframe::Storage) {
        eframe::set_value(storage, eframe::APP_KEY, self);
    }

    fn ui(&mut self, ui: &mut egui::Ui, _frame: &mut eframe::Frame) {
        egui::CentralPanel::default().show_inside(ui, |ui| {
            ui.heading("Hexagon Game");
        });
    }
}
