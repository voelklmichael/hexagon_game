mod backend_reqwest;
mod backend_responses;
pub use backend_reqwest::BackendReqwest;
use hexagon_types::DBHighscorePeak;
use indexmap::IndexMap;
use std::collections::HashSet;
use uuid::Uuid;

use hexagon_engine::{
    CollisionMode, Color, GameOptionsDelivery, GameOptionsDiscriminants, GameOptionsStandard,
    GameResult, GameState, OuterConnectors, PlayerId, RandomNumberGenerator,
    WinningConditionStandard,
};

#[derive(serde::Serialize, serde::Deserialize, PartialEq, Clone, Copy, Default)]
#[serde(rename_all = "camelCase")]
pub enum LeftTab {
    #[default]
    Missions,
    Options,
    Hand,
    Controls,
    Music,
    Rendering,
}

impl LeftTab {
    fn label(self) -> &'static str {
        match self {
            LeftTab::Missions => "Missions",
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
    UserLogin,
}

impl RightTab {
    fn label(self) -> &'static str {
        match self {
            RightTab::Help => "Help / Tutorial",
            RightTab::Statistics => "Statistics",
            RightTab::GameStateJson => "Game State JSON",
            RightTab::UserLogin => "User Login",
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
    #[serde(skip)]
    pub confetti: crate::confetti::ConfettiState,
}

impl Default for BoardInteraction {
    fn default() -> Self {
        Self {
            selected_tile: None,
            animation_t: 0.0,
            confetti: crate::confetti::ConfettiState::default(),
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
    pub background: Color,
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
            #[cfg(not(target_arch = "wasm32"))]
            background: Color::Black,
            #[cfg(target_arch = "wasm32")]
            background: Color::White,
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

#[derive(Default, PartialEq, Clone, Copy)]
enum ReplayPhase {
    #[default]
    InitialWait,
    Animating,
    LoopWait,
}

pub struct ReplayPlayer {
    /// Snapshot sequence: [before move 1, before move 2, …, final state]
    states: Vec<GameState>,
    step: usize,
    timer: f32,
    phase: ReplayPhase,
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
    pub replay: Option<ReplayPlayer>,
    #[serde(skip)]
    pub music_player: Option<crate::music::MusicPlayer>,
    pub rng: RandomNumberGenerator,
    pub missions_won: HashSet<Uuid>,
    pub current_mission: Option<usize>,
    pub user_login: crate::panels::user_login::UserLogin,
    #[serde(skip)]
    pub backend_reqwest: BackendReqwest,
    #[serde(skip)]
    pub mission_result_reported: bool,
    #[serde(skip)]
    pub mission_user_best: Option<DBHighscorePeak>,
    #[serde(skip)]
    pub mission_overall_best: Option<DBHighscorePeak>,
}

impl HexApp {
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        let mut app: Self = if let Some(storage) = cc.storage {
            eframe::get_value(storage, eframe::APP_KEY).unwrap_or_default()
        } else {
            Default::default()
        };

        app.music.current_track = app.music.current_track.min(crate::music::TRACKS.len() - 1);
        app.music_player = crate::music::MusicPlayer::new(app.music.volume);
        if let Some(player) = &app.music_player {
            player.play_track(app.music.current_track);
            if app.music.paused {
                player.set_paused(true);
            }
        }

        app
    }
}

impl eframe::App for HexApp {
    fn save(&mut self, storage: &mut dyn eframe::Storage) {
        eframe::set_value(storage, eframe::APP_KEY, self);
    }

    fn logic(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        ctx.plugin_or_default::<egui_async::EguiAsyncPlugin>();
    }

    fn ui(&mut self, ui: &mut egui::Ui, _frame: &mut eframe::Frame) {
        self.backend_reqwest.poll_me_task();

        if let Some(player) = &self.music_player
            && !self.music.paused
            && player.check_and_reset_finished()
        {
            self.music.current_track = (self.music.current_track + 1) % crate::music::TRACKS.len();
            player.play_track(self.music.current_track);
        }

        if self.interaction.animation_t < 1.0 {
            let dt = ui.ctx().input(|i| i.stable_dt);
            self.interaction.animation_t = (self.interaction.animation_t + dt * 2.0).min(1.0);
            ui.ctx().request_repaint();
        }

        // Replay management: start 1 s after game ends (timer begins immediately,
        // in parallel with the finishing animation), loop with 3 s pause between iterations.
        let game_done = self.game.as_ref().is_some_and(|g| g.result.is_some());
        if game_done {
            if self.replay.is_none() {
                let states: Vec<GameState> = self
                    .history
                    .undo_stack
                    .iter()
                    .cloned()
                    .chain(self.game.iter().cloned())
                    .collect();
                if !states.is_empty() {
                    self.replay = Some(ReplayPlayer {
                        states,
                        step: 0,
                        timer: 1.0,
                        phase: ReplayPhase::InitialWait,
                    });
                }
            }
        } else if !game_done {
            self.replay = None;
        }

        if let Some(replay) = &mut self.replay {
            let dt = ui.ctx().input(|i| i.stable_dt);
            match replay.phase {
                ReplayPhase::InitialWait => {
                    replay.timer -= dt;
                    if replay.timer <= 0.0 {
                        replay.step = 0;
                        replay.phase = ReplayPhase::Animating;
                        self.interaction.animation_t = 0.0;
                    }
                }
                ReplayPhase::Animating => {
                    // The existing animation tick (earlier in ui()) advances animation_t to 1.0.
                    // Once it arrives, move to the next step or end the loop.
                    if self.interaction.animation_t >= 1.0 {
                        if replay.step + 1 < replay.states.len() {
                            replay.step += 1;
                            self.interaction.animation_t = 0.0;
                        } else {
                            replay.phase = ReplayPhase::LoopWait;
                            replay.timer = 3.0;
                        }
                    }
                }
                ReplayPhase::LoopWait => {
                    replay.timer -= dt;
                    if replay.timer <= 0.0 {
                        replay.step = 0;
                        replay.phase = ReplayPhase::Animating;
                        self.interaction.animation_t = 0.0;
                    }
                }
            }
            ui.ctx().request_repaint();
        }

        self.process_backend_responses();

        // Report mission completion to backend once per win, if the user is logged in.
        if game_done
            && !self.mission_result_reported
            && self.current_mission.is_some()
            && self.user_login.logged_in_as.is_some()
        {
            if let Some(game) = &self.game {
                if matches!(&game.result, Some(GameResult::Win(_))) {
                    if let (Some((user_id, _)), Some(mission_idx)) =
                        (&self.user_login.logged_in_as, self.current_mission)
                    {
                        if let Some(mission_uuid) = crate::panels::missions::mission_id(mission_idx)
                        {
                            self.backend_reqwest.report_mission_done(
                                *user_id,
                                mission_uuid,
                                &game.statistics,
                            );
                            self.mission_result_reported = true;
                        }
                    }
                }
            }
        }
        if !game_done {
            self.mission_result_reported = false;
        }

        // Snapshot the replay step index now (before panel closures borrow self).
        // During InitialWait show the live game (final state); only switch to the
        // replay snapshot once the actual playback starts.
        let replay_step: Option<usize> = self.replay.as_ref().and_then(|r| match r.phase {
            ReplayPhase::InitialWait => None,
            ReplayPhase::Animating | ReplayPhase::LoopWait => Some(r.step),
        });

        let total_width = ui.available_width();
        let max_side = total_width * 0.20;

        // Left panel — always visible, burger menu tabs
        egui::Panel::left("left_panel")
            .resizable(true)
            .show_inside(ui, |ui| {
                // Burger menu header — copy tab to local so closure doesn't hold &mut self
                let mut left_tab = self.left_tab;
                ui.horizontal(|ui| {
                    egui::ComboBox::from_id_salt("left_tab_select")
                        .selected_text(format!("☰  {}", left_tab.label()))
                        .show_ui(ui, |ui| {
                            for tab in [
                                LeftTab::Missions,
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

                egui::ScrollArea::vertical().show(ui, |ui| match self.left_tab {
                    LeftTab::Missions => {
                        if let Some(idx) =
                            crate::panels::missions::show(ui, &mut self.game, &self.missions_won)
                        {
                            self.history.undo_stack.clear();
                            self.history.redo_stack.clear();
                            self.current_mission = Some(idx);
                            self.left_tab = LeftTab::Hand;
                            self.mission_user_best = None;
                            self.mission_overall_best = None;
                            if let Some(mission_uuid) = crate::panels::missions::mission_id(idx) {
                                self.backend_reqwest
                                    .fetch_mission_overall_best(mission_uuid);
                                if let Some((user_id, _)) = &self.user_login.logged_in_as {
                                    self.backend_reqwest
                                        .fetch_mission_user_best(*user_id, mission_uuid);
                                }
                            }
                        }
                    }
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
                                Ok(game) => {
                                    self.game = Some(game);
                                    self.history.undo_stack.clear();
                                    self.history.redo_stack.clear();
                                    self.current_mission = None;
                                    self.left_tab = LeftTab::Hand;
                                }
                                Err(e) => eprintln!("Failed to start game: {e}"),
                            }
                        }
                        ui.separator();
                        if crate::panels::predefined_games::show(ui, &mut self.game, &mut self.rng)
                        {
                            self.history.undo_stack.clear();
                            self.history.redo_stack.clear();
                            self.current_mission = None;
                            self.left_tab = LeftTab::Hand;
                        }
                    }
                    LeftTab::Hand => {
                        crate::panels::hand::show(
                            ui,
                            &mut self.game,
                            &self.rendering_data,
                            &mut self.interaction,
                            &mut self.history,
                            &mut self.current_mission,
                            &mut self.missions_won,
                            self.user_login.logged_in_as.as_ref().map(|(id, _)| *id),
                            &mut self.backend_reqwest,
                        );
                    }
                    LeftTab::Controls => {
                        if crate::panels::controls::show(ui, &mut self.game, &mut self.history) {
                            self.left_tab = LeftTab::Hand;
                        }
                    }
                    LeftTab::Music => {
                        crate::panels::music::show(ui, &mut self.music, self.music_player.as_ref());
                    }
                    LeftTab::Rendering => {
                        crate::panels::rendering::show(ui, &mut self.rendering_data);
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
                        if ui.button("➡").clicked() {
                            self.right_panel_open = false;
                        }
                        egui::ComboBox::from_id_salt("right_tab_select")
                            .selected_text(format!("☰  {}", right_tab.label()))
                            .show_ui(ui, |ui| {
                                for tab in [
                                    RightTab::Help,
                                    RightTab::Statistics,
                                    RightTab::GameStateJson,
                                    RightTab::UserLogin,
                                ] {
                                    ui.selectable_value(&mut right_tab, tab, tab.label());
                                }
                            });
                    });
                    self.right_tab = right_tab;
                    ui.separator();

                    egui::ScrollArea::vertical().show(ui, |ui| match self.right_tab {
                        RightTab::Help => {
                            crate::panels::help::show(ui);
                        }
                        RightTab::Statistics => {
                            crate::panels::statistics::show(
                                ui,
                                &self.rendering_data,
                                self.game.as_ref().map(|g| &g.statistics),
                                self.mission_user_best.as_ref(),
                                self.mission_overall_best.as_ref(),
                            );
                        }
                        RightTab::GameStateJson => {
                            crate::panels::game_state_json::show(ui, self.game.as_ref());
                        }
                        RightTab::UserLogin => {
                            crate::panels::user_login::show(
                                ui,
                                &mut self.user_login,
                                &mut self.backend_reqwest,
                            );
                        }
                    });
                });
        }

        // Centre — game board, fills all remaining space
        egui::CentralPanel::default().show_inside(ui, |ui| {
            if !right_open {
                ui.with_layout(egui::Layout::right_to_left(egui::Align::TOP), |ui| {
                    if ui
                        .small_button("☰")
                        .on_hover_text("Open side panel")
                        .clicked()
                    {
                        self.right_panel_open = true;
                    }
                });
            }
            if let Some(step) = replay_step {
                if let Some(game) = self.replay.as_ref().and_then(|r| r.states.get(step)) {
                    crate::panels::game_board::show(
                        ui,
                        game,
                        &self.rendering_data,
                        &mut self.interaction,
                    );
                }
            } else if let Some(game) = &self.game {
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
