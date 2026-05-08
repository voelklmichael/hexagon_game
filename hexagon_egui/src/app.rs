use indexmap::IndexMap;

use hexagon_engine::{
    CollisionMode, Color, GameOptionsDelivery, GameOptionsDiscriminants, GameOptionsStandard,
    GameState, OuterConnectors, PlayerId, WinningConditionStandard,
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
    pub paused: bool,
    pub current_track: usize,
}

impl Default for MusicState {
    fn default() -> Self {
        Self {
            volume: 1.0,
            paused: false,
            current_track: 0,
        }
    }
}

#[derive(serde::Serialize, serde::Deserialize)]
#[serde(default, rename_all = "camelCase")]
pub struct BoardInteraction {
    #[serde(skip)]
    pub selected_tile: Option<usize>,
    #[serde(skip)]
    pub animation_t: f32,
}

impl Default for BoardInteraction {
    fn default() -> Self {
        Self {
            selected_tile: None,
            animation_t: 0.0,
        }
    }
}

#[derive(serde::Serialize, serde::Deserialize)]
#[serde(default, rename_all = "camelCase")]
pub struct RenderingData {
    pub player_colors: IndexMap<PlayerId, Color>,
    pub dead_end_color: Color,
    pub closed_loop_color: Color,
    pub unused_color: Color,
    pub hex_fill: Color,
    pub hex_stroke: Color,
    pub highlighted_hex_fill: Color,
    pub highlighted_hex_stroke: Color,
}

impl Default for RenderingData {
    fn default() -> Self {
        Self {
            player_colors: [
                (PlayerId(0), Color::Green),
                (PlayerId(1), Color::Red),
                (PlayerId(2), Color::Blue),
                (PlayerId(3), Color::Purple),
                (PlayerId(4), Color::Cyan),
                (PlayerId(5), Color::Pink),
            ]
            .into_iter()
            .collect(),
            dead_end_color: Color::Gray,
            closed_loop_color: Color::Teal,
            unused_color: Color::Golden,
            hex_fill: Color::Beige,
            hex_stroke: Color::DarkGray,
            highlighted_hex_fill: Color::Moccasin,
            highlighted_hex_stroke: Color::DarkOrange,
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
    pub rendering_data: RenderingData,
    pub interaction: BoardInteraction,
    pub options: OptionsState,
    pub music: MusicState,
    #[serde(skip)]
    #[cfg(not(target_arch = "wasm32"))]
    pub music_player: Option<crate::music::MusicPlayer>,
}

impl Default for HexApp {
    fn default() -> Self {
        Self {
            game: None,
            history: GameHistory::default(),
            rendering_data: RenderingData::default(),
            interaction: BoardInteraction::default(),
            options: OptionsState::default(),
            music: MusicState::default(),
            #[cfg(not(target_arch = "wasm32"))]
            music_player: None,
        }
    }
}

impl HexApp {
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        let mut app: Self = if let Some(storage) = cc.storage {
            eframe::get_value(storage, eframe::APP_KEY).unwrap_or_default()
        } else {
            Default::default()
        };

        #[cfg(not(target_arch = "wasm32"))]
        {
            app.music.current_track = app.music.current_track.min(crate::music::TRACKS.len() - 1);
            app.music_player = crate::music::MusicPlayer::new(app.music.volume);
            if let Some(player) = &app.music_player {
                player.play_track(&crate::music::track_path(app.music.current_track));
                if app.music.paused {
                    player.set_paused(true);
                }
            }
        }

        app
    }
}

impl eframe::App for HexApp {
    fn save(&mut self, storage: &mut dyn eframe::Storage) {
        eframe::set_value(storage, eframe::APP_KEY, self);
    }

    fn ui(&mut self, ui: &mut egui::Ui, _frame: &mut eframe::Frame) {
        #[cfg(not(target_arch = "wasm32"))]
        if let Some(player) = &self.music_player {
            if !self.music.paused && player.check_and_reset_finished() {
                self.music.current_track =
                    (self.music.current_track + 1) % crate::music::TRACKS.len();
                player.play_track(&crate::music::track_path(self.music.current_track));
            }
        }

        if self.interaction.animation_t < 1.0 {
            let dt = ui.ctx().input(|i| i.stable_dt);
            self.interaction.animation_t = (self.interaction.animation_t + dt * 2.0).min(1.0);
            ui.ctx().request_repaint();
        }

        egui::CentralPanel::default().show_inside(ui, |ui| {
            if crate::panels::options::show(ui, &mut self.options) {
                let result = match self.options.selected {
                    hexagon_engine::GameOptionsDiscriminants::Standard => {
                        self.options.standard.clone().start_game()
                    }
                    hexagon_engine::GameOptionsDiscriminants::Delivery => {
                        self.options.delivery.clone().start_game()
                    }
                };
                match result {
                    Ok(game) => self.game = Some(game),
                    Err(e) => eprintln!("Failed to start game: {e}"),
                }
            }
            ui.separator();
            if let Some(game) = &mut self.game {
                crate::panels::game_board::show(
                    ui,
                    game,
                    &self.rendering_data,
                    &mut self.interaction,
                );
                ui.separator();
                crate::panels::hand::show(
                    ui,
                    game,
                    &self.rendering_data,
                    &mut self.interaction,
                    &mut self.history,
                );
                ui.separator();
            }
            crate::panels::controls::show(ui, &mut self.game, &mut self.history);
            ui.separator();
            crate::panels::game_state_json::show(ui, self.game.as_ref());
            ui.separator();
            crate::panels::rendering::show(ui, &mut self.rendering_data);
            ui.separator();
            crate::panels::statistics::show(
                ui,
                &self.rendering_data,
                self.game.as_ref().map(|g| &g.statistics),
            );
            ui.separator();
            #[cfg(not(target_arch = "wasm32"))]
            crate::panels::music::show(ui, &mut self.music, self.music_player.as_ref());
        });
    }
}
