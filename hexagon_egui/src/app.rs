mod backend_reqwest;
mod backend_responses;
pub use backend_reqwest::BackendReqwest;
use hexagon_types::{DBHighscorePeak, MissionEntry, player::PlayerId};
use indexmap::IndexMap;
use std::collections::{HashMap, HashSet};
use strum::VariantArray;
use uuid::Uuid;

use hexagon_engine::{
    CollisionMode, Color, GameOptionsDelivery, GameOptionsDiscriminants, GameOptionsStandard,
    GameResult, GameState, OuterConnectors, RandomNumberGenerator, WinningCondition,
};

#[derive(serde::Serialize, serde::Deserialize, PartialEq, Clone, Copy, Default, VariantArray)]
#[serde(rename_all = "camelCase")]
pub enum LeftTab {
    Missions,
    Multiplayer,
    Hand,
    #[default]
    Introduction,
    Options,
    Statistics,
    Controls,
    Rendering,
    GameStateJson,
}

impl LeftTab {
    fn label(self) -> &'static str {
        match self {
            LeftTab::Missions => "Missions",
            LeftTab::Multiplayer => "Multiplayer",
            LeftTab::Options => "Options",
            LeftTab::Hand => "Player Hand",
            LeftTab::Controls => "Controls",
            LeftTab::Introduction => "Introduction",
            LeftTab::Rendering => "Rendering",
            LeftTab::Statistics => "Statistics",
            LeftTab::GameStateJson => "Game State JSON",
        }
    }

    fn visible(self) -> bool {
        match self {
            LeftTab::Rendering | LeftTab::GameStateJson => cfg!(debug_assertions),
            _ => true,
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
                winning_condition: WinningCondition::HighestVelocity,
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
    /// On WASM, browsers block autoplay before a user gesture. This flag
    /// is false until the user clicks something, at which point we start
    /// the player. On desktop it is set to true immediately on startup.
    #[serde(skip)]
    pub started: bool,
}

impl Default for MusicState {
    fn default() -> Self {
        Self {
            volume: 1.0,
            paused: false,
            current_track: 0,
            started: false,
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
    #[serde(skip)]
    pub replay: Option<ReplayPlayer>,
    #[serde(skip)]
    pub music_player: Option<crate::music::MusicPlayer>,
    pub rng: RandomNumberGenerator,
    pub missions: Vec<MissionEntry>,
    pub missions_won: HashSet<Uuid>,
    pub current_mission: Option<usize>,
    pub user_login: crate::panels::user_login::UserLogin,
    #[serde(skip)]
    pub backend_reqwest: BackendReqwest,
    #[serde(skip)]
    pub mission_result_reported: bool,
    /// Cached per-user highscores keyed by mission id, persisted for offline display.
    pub cached_user_bests: HashMap<Uuid, DBHighscorePeak>,
    /// Cached global highscores keyed by mission id, persisted for offline display.
    pub cached_overall_bests: HashMap<Uuid, DBHighscorePeak>,
    #[serde(skip, default = "default_true")]
    pub show_introduction_screen: bool,
    #[serde(skip)]
    pub slideshow: crate::panels::introduction_central_panel::SlideshowState,
}

fn default_true() -> bool {
    true
}

impl HexApp {
    fn start_intro_mission(&mut self) {
        let idx = 0;
        let prev_game = self.game.take();
        crate::panels::missions::start_mission(&self.missions, idx, &mut self.game);
        if self.game.is_none() {
            self.game = prev_game;
        } else {
            self.history.undo_stack.clear();
            self.history.redo_stack.clear();
            self.current_mission = Some(idx);
            self.left_tab = LeftTab::Hand;
            self.show_introduction_screen = false;
            if let Some(mission_uuid) = crate::panels::missions::mission_id(&self.missions, idx) {
                self.backend_reqwest
                    .fetch_mission_overall_best(mission_uuid);
                if let Some((user_id, _)) = &self.user_login.logged_in_as {
                    self.backend_reqwest
                        .fetch_mission_user_best(*user_id, mission_uuid);
                }
            }
        }
    }

    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        let mut app: Self = if let Some(storage) = cc.storage {
            eframe::get_value(storage, eframe::APP_KEY).unwrap_or_default()
        } else {
            Default::default()
        };

        app.music.current_track = app.music.current_track.min(crate::music::TRACKS.len() - 1);
        app.music_player = crate::music::MusicPlayer::new(app.music.volume);
        // On desktop, autoplay works; on WASM the browser blocks it until a user
        // gesture, so we defer the first play_track call to the hand buttons.
        #[cfg(not(target_arch = "wasm32"))]
        if let Some(player) = &app.music_player {
            player.play_track(app.music.current_track);
            if app.music.paused {
                player.set_paused(true);
            }
            app.music.started = true;
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

        let current_time = ui.ctx().input(|i| i.time);
        self.process_backend_responses(current_time);

        let logged_in_user_id = self.user_login.logged_in_as.as_ref().map(|(id, _)| *id);
        self.backend_reqwest
            .poll_mission_writes(logged_in_user_id, current_time);
        self.backend_reqwest
            .poll_fetch_retries(logged_in_user_id, current_time);

        // Queue mission completion write once per win. The write is submitted (and
        // retried on failure) by `poll_mission_writes`; if the player is not yet
        // logged in the write is held until they log in.
        if game_done
            && !self.mission_result_reported
            && let Some(game) = &self.game
            && matches!(&game.result, Some(GameResult::Win(_)))
            && let Some(mission_idx) = self.current_mission
            && let Some(mission_uuid) =
                crate::panels::missions::mission_id(&self.missions, mission_idx)
        {
            if let Ok(value) = serde_json::to_value(game) {
                self.backend_reqwest
                    .queue_mission_result(mission_uuid, &game.statistics, value);
            }
            self.mission_result_reported = true;
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

        // Left panel — hidden during introduction
        if !self.show_introduction_screen {
            egui::Panel::left("left_panel")
                .resizable(true)
                .show_inside(ui, |ui| {
                    // Burger menu header — copy tab to local so closure doesn't hold &mut self
                    if !self.left_tab.visible() {
                        self.left_tab = LeftTab::default();
                    }
                    let mut left_tab = self.left_tab;
                    ui.horizontal(|ui| {
                        egui::ComboBox::from_id_salt("left_tab_select")
                            .selected_text(format!("☰  {}", left_tab.label()))
                            .show_ui(ui, |ui| {
                                for tab in LeftTab::VARIANTS {
                                    if tab.visible() {
                                        ui.selectable_value(&mut left_tab, *tab, tab.label());
                                    }
                                }
                            });
                    });
                    self.left_tab = left_tab;
                    ui.separator();

                    egui::ScrollArea::vertical().show(ui, |ui| match self.left_tab {
                        LeftTab::Missions => {
                            if let Some(idx) = crate::panels::missions::show(
                                ui,
                                &self.missions,
                                &mut self.game,
                                &self.missions_won,
                            ) {
                                self.history.undo_stack.clear();
                                self.history.redo_stack.clear();
                                self.current_mission = Some(idx);
                                self.left_tab = LeftTab::Hand;
                                self.show_introduction_screen = false;
                                if let Some(mission_uuid) =
                                    crate::panels::missions::mission_id(&self.missions, idx)
                                {
                                    self.backend_reqwest
                                        .fetch_mission_overall_best(mission_uuid);
                                    if let Some((user_id, _)) = &self.user_login.logged_in_as {
                                        self.backend_reqwest
                                            .fetch_mission_user_best(*user_id, mission_uuid);
                                    }
                                }
                            }
                        }
                        LeftTab::Multiplayer => {
                            if crate::panels::options::multiplayer_game(
                                ui,
                                &mut self.options,
                                &mut self.rng,
                            ) {
                                match self.options.standard.clone().start_game() {
                                    Ok(game) => {
                                        self.game = Some(game);
                                        self.history.undo_stack.clear();
                                        self.history.redo_stack.clear();
                                        self.current_mission = None;
                                        self.left_tab = LeftTab::Hand;
                                        self.show_introduction_screen = false;
                                    }
                                    Err(e) => eprintln!("Failed to start game: {e}"),
                                }
                            }
                        }
                        LeftTab::Options => {
                            if crate::panels::options::show(ui, &mut self.options, &mut self.rng) {
                                let result = match self.options.selected {
                                hexagon_engine::GameOptionsDiscriminants::Standard => {
                                    self.options.standard.clone().start_game()
                                }
                                hexagon_engine::GameOptionsDiscriminants::Delivery => {
                                    self.options.delivery.clone().start_game()
                                }
                                hexagon_engine::GameOptionsDiscriminants::Highscore
                                | hexagon_engine::GameOptionsDiscriminants::HighscoreV2 => Err(
                                    "Highscore missions must be started from the Missions panel"
                                        .to_string(),
                                ),
                            };
                                match result {
                                    Ok(game) => {
                                        self.game = Some(game);
                                        self.history.undo_stack.clear();
                                        self.history.redo_stack.clear();
                                        self.current_mission = None;
                                        self.left_tab = LeftTab::Hand;
                                        self.show_introduction_screen = false;
                                    }
                                    Err(e) => eprintln!("Failed to start game: {e}"),
                                }
                            }
                            ui.separator();
                            if crate::panels::predefined_games::show(
                                ui,
                                &mut self.game,
                                &mut self.rng,
                            ) {
                                self.history.undo_stack.clear();
                                self.history.redo_stack.clear();
                                self.current_mission = None;
                                self.left_tab = LeftTab::Hand;
                                self.show_introduction_screen = false;
                            }
                        }
                        LeftTab::Hand => crate::panels::hand::show(
                            ui,
                            &mut self.game,
                            &self.rendering_data,
                            &mut self.interaction,
                            &mut self.history,
                            &mut self.current_mission,
                            &self.missions,
                            &mut self.missions_won,
                            &mut self.music,
                            self.music_player.as_ref(),
                        ),
                        LeftTab::Controls => {
                            if crate::panels::controls::show(ui, &mut self.game, &mut self.history)
                            {
                                self.left_tab = LeftTab::Hand;
                            }
                            ui.separator();
                            crate::panels::music::show(
                                ui,
                                &mut self.music,
                                self.music_player.as_ref(),
                            );
                        }
                        LeftTab::Rendering => {
                            #[cfg(debug_assertions)]
                            crate::panels::rendering::show(ui, &mut self.rendering_data);
                        }
                        LeftTab::Statistics => {
                            let active_id = self.current_mission.and_then(|idx| {
                                crate::panels::missions::mission_id(&self.missions, idx)
                            });
                            let user_best =
                                active_id.and_then(|id| self.cached_user_bests.get(&id));
                            let overall_best =
                                active_id.and_then(|id| self.cached_overall_bests.get(&id));
                            crate::panels::statistics::show(
                                ui,
                                &self.rendering_data,
                                self.game.as_ref().map(|g| &g.statistics),
                                self.game.as_ref().map(|g| &g.options),
                                user_best,
                                overall_best,
                            );
                        }
                        LeftTab::GameStateJson => {
                            #[cfg(debug_assertions)]
                            crate::panels::game_state_json::show(ui, self.game.as_ref());
                        }
                        LeftTab::Introduction => {
                            if crate::panels::introduction::show(
                                ui,
                                &mut self.show_introduction_screen,
                                !self.missions.is_empty(),
                            ) {
                                self.start_intro_mission();
                            }
                        }
                    });
                });
        } // end left panel visibility guard

        // Right panel — collapsible, shows User Login
        let right_open = self.right_panel_open;
        if right_open {
            egui::Panel::right("right_panel")
                .resizable(true)
                .show_inside(ui, |ui| {
                    ui.horizontal(|ui| {
                        if ui.button("➡").clicked() {
                            self.right_panel_open = false;
                        }
                        ui.label("User Login");
                    });
                    ui.separator();
                    egui::ScrollArea::vertical().show(ui, |ui| {
                        crate::panels::user_login::show(
                            ui,
                            &mut self.user_login,
                            &mut self.backend_reqwest,
                        );
                    });
                });
        }

        // Centre — game board, fills all remaining space
        egui::CentralPanel::default().show_inside(ui, |ui| {
            if !right_open {
                let avail = ui.available_rect_before_wrap();
                let ctx = ui.ctx().clone();
                egui::Area::new(egui::Id::new("right_panel_btn"))
                    .fixed_pos(avail.right_top() + egui::vec2(-72.0, 4.0))
                    .order(egui::Order::Foreground)
                    .show(&ctx, |ui| {
                        ui.horizontal(|ui| {
                            let dark = ui.visuals().dark_mode;
                            if ui
                                .button(if dark { "☀" } else { "🌙" })
                                .on_hover_text(if dark {
                                    "Switch to light mode"
                                } else {
                                    "Switch to dark mode"
                                })
                                .clicked()
                            {
                                ui.ctx().set_visuals(if dark {
                                    egui::Visuals::light()
                                } else {
                                    egui::Visuals::dark()
                                });
                            }
                            let logged_in = self.user_login.logged_in_as.is_some();
                            let bust_label = if logged_in { "👤✔" } else { "👤✘" };
                            if ui
                                .button(bust_label)
                                .on_hover_text("Open side panel")
                                .clicked()
                            {
                                self.right_panel_open = true;
                            }
                        });
                    });
            }
            if self.show_introduction_screen {
                if self.slideshow.show(ui, !self.missions.is_empty()) {
                    self.start_intro_mission();
                }
            } else if let Some(step) = replay_step {
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
