use indexmap::IndexMap;

use hexagon_engine::{
    CollisionMode, Color, GameOptionsDelivery, GameOptionsDiscriminants, GameOptionsStandard,
    GameState, OuterConnectors, PlayerId, WinningConditionStandard,
};

#[derive(serde::Serialize, serde::Deserialize, PartialEq, Clone, Copy, Default)]
#[serde(rename_all = "camelCase")]
pub enum LeftTab {
    #[default]
    Options,
    Hand,
    Controls,
    Music,
    Rendering,
}

impl LeftTab {
    fn label(self) -> &'static str {
        match self {
            LeftTab::Options => "Options",
            LeftTab::Hand => "Player Hand",
            LeftTab::Controls => "Controls",
            LeftTab::Music => "Music",
            LeftTab::Rendering => "Rendering",
        }
    }
}

#[derive(serde::Serialize, serde::Deserialize, PartialEq, Clone, Copy, Default)]
#[serde(rename_all = "camelCase")]
pub enum RightTab {
    #[default]
    Help,
    Statistics,
    GameStateJson,
}

impl RightTab {
    fn label(self) -> &'static str {
        match self {
            RightTab::Help => "Help / Tutorial",
            RightTab::Statistics => "Statistics",
            RightTab::GameStateJson => "Game State JSON",
        }
    }
}

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
#[derive(Default)]
pub struct GameHistory {
    #[serde(skip)]
    pub undo_stack: Vec<GameState>,
    #[serde(skip)]
    pub redo_stack: Vec<GameState>,
}


#[derive(serde::Serialize, serde::Deserialize)]
#[serde(default, rename_all = "camelCase")]
#[derive(Default)]
pub struct HexApp {
    pub game: Option<GameState>,
    pub history: GameHistory,
    pub rendering_data: RenderingData,
    pub interaction: BoardInteraction,
    pub options: OptionsState,
    pub music: MusicState,
    pub left_tab: LeftTab,
    pub right_panel_open: bool,
    pub right_tab: RightTab,
    #[serde(skip)]
    #[cfg(not(target_arch = "wasm32"))]
    pub music_player: Option<crate::music::MusicPlayer>,
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
        if let Some(player) = &self.music_player
            && !self.music.paused && player.check_and_reset_finished() {
                self.music.current_track =
                    (self.music.current_track + 1) % crate::music::TRACKS.len();
                player.play_track(&crate::music::track_path(self.music.current_track));
            }

        if self.interaction.animation_t < 1.0 {
            let dt = ui.ctx().input(|i| i.stable_dt);
            self.interaction.animation_t = (self.interaction.animation_t + dt * 2.0).min(1.0);
            ui.ctx().request_repaint();
        }

        let total_width = ui.available_width();
        let max_side = total_width * 0.20;

        // Left panel — always visible, burger menu tabs
        egui::Panel::left("left_panel")
            .resizable(true)
            .max_size(max_side)
            .show_inside(ui, |ui| {
                // Burger menu header — copy tab to local so closure doesn't hold &mut self
                let mut left_tab = self.left_tab;
                ui.horizontal(|ui| {
                    egui::ComboBox::from_id_salt("left_tab_select")
                        .selected_text(format!("☰  {}", left_tab.label()))
                        .show_ui(ui, |ui| {
                            for tab in [
                                LeftTab::Options,
                                LeftTab::Hand,
                                LeftTab::Controls,
                                LeftTab::Music,
                                LeftTab::Rendering,
                            ] {
                                ui.selectable_value(&mut left_tab, tab, tab.label());
                            }
                        });
                });
                self.left_tab = left_tab;
                ui.separator();

                egui::ScrollArea::vertical().show(ui, |ui| {
                    match self.left_tab {
                        LeftTab::Options => {
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
                            crate::panels::predefined_games::show(ui, &mut self.game);
                        }
                        LeftTab::Hand => {
                            crate::panels::hand::show(
                                ui,
                                &mut self.game,
                                &self.rendering_data,
                                &mut self.interaction,
                                &mut self.history,
                            );
                        }
                        LeftTab::Controls => {
                            crate::panels::controls::show(ui, &mut self.game, &mut self.history);
                        }
                        LeftTab::Music => {
                            #[cfg(not(target_arch = "wasm32"))]
                            crate::panels::music::show(
                                ui,
                                &mut self.music,
                                self.music_player.as_ref(),
                            );
                            #[cfg(target_arch = "wasm32")]
                            ui.label("Music is not available on web.");
                        }
                        LeftTab::Rendering => {
                            crate::panels::rendering::show(ui, &mut self.rendering_data);
                        }
                    }
                });
            });

        // Right panel — collapsible, burger menu tabs
        let right_open = self.right_panel_open;
        if right_open {
            egui::Panel::right("right_panel")
                .resizable(true)
                .max_size(max_side)
                .show_inside(ui, |ui| {
                    let mut right_tab = self.right_tab;
                    ui.horizontal(|ui| {
                        if ui.button("✕").clicked() {
                            self.right_panel_open = false;
                        }
                        egui::ComboBox::from_id_salt("right_tab_select")
                            .selected_text(format!("☰  {}", right_tab.label()))
                            .show_ui(ui, |ui| {
                                for tab in [
                                    RightTab::Help,
                                    RightTab::Statistics,
                                    RightTab::GameStateJson,
                                ] {
                                    ui.selectable_value(&mut right_tab, tab, tab.label());
                                }
                            });
                    });
                    self.right_tab = right_tab;
                    ui.separator();

                    egui::ScrollArea::vertical().show(ui, |ui| {
                        match self.right_tab {
                            RightTab::Help => {
                                crate::panels::help::show(ui);
                            }
                            RightTab::Statistics => {
                                crate::panels::statistics::show(
                                    ui,
                                    &self.rendering_data,
                                    self.game.as_ref().map(|g| &g.statistics),
                                );
                            }
                            RightTab::GameStateJson => {
                                crate::panels::game_state_json::show(ui, self.game.as_ref());
                            }
                        }
                    });
                });
        }

        // Centre — game board, fills all remaining space
        egui::CentralPanel::default().show_inside(ui, |ui| {
            if !right_open {
                ui.with_layout(egui::Layout::right_to_left(egui::Align::TOP), |ui| {
                    if ui.small_button("☰").on_hover_text("Open side panel").clicked() {
                        self.right_panel_open = true;
                    }
                });
            }
            if let Some(game) = &mut self.game {
                crate::panels::game_board::show(
                    ui,
                    game,
                    &self.rendering_data,
                    &mut self.interaction,
                );
            }
        });
    }
}
