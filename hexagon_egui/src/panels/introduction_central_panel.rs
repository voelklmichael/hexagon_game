use std::time::Duration;

use hexagon_engine::{
    CollisionMode, ConnectorEdgeSub, Edge, EdgeSub, GameOptions, GameOptionsStandard, GameState,
    OuterConnectors, Sub, WinningCondition,
};
use hexagon_types::Tile;
use strum::VariantArray;

use crate::panels::hand::draw_tile_preview;

const AUTO_CYCLE_SECS: f64 = 5.0;
const DOT_RADIUS: f32 = 5.0;
const DOT_SPACING: f32 = 22.0;

pub struct AnimSlideTiming {
    /// Pause showing the board before bringing in the next tile
    pub board_show_secs: f64,
    /// How long the tile preview is shown on the left before playing
    pub tile_show_secs: f64,
    /// How long the player movement animation runs after the tile is placed
    pub play_secs: f64,
    /// Hold time after the player has reached the new position
    pub player_anim_secs: f64,
}

const ANIM_TIMING: AnimSlideTiming = AnimSlideTiming {
    board_show_secs: 0.6,
    tile_show_secs: 1.0,
    play_secs: 1.4,
    player_anim_secs: 0.8,
};

const ANIM_TILE_COUNT: usize = 4;

const PHASE_TEXTS: [&str; 5] = [
    "Ready for the next tile",
    "Select a tile from your hand to play",
    "Tile placed — player follows the new path!",
    "Player reached a new position",
    "Player reaches target - game won!",
];

#[derive(Clone, Copy, strum::VariantArray, PartialEq)]
enum Slide {
    Welcome,
    AnimatingGame,
    PlacingTilesRotating,
    PlacingTilesPlaying,
    Acknowledgments,
}

impl Slide {
    fn title(self) -> &'static str {
        match self {
            Self::Welcome => "Hexagon - The Game",
            Self::PlacingTilesRotating => "Placing Tiles",
            Self::PlacingTilesPlaying => "Placing Tiles",
            Self::AnimatingGame => "Playing a Game",
            Self::Acknowledgments => "Acknowledgments",
        }
    }

    fn show(
        self,
        ui: &mut egui::Ui,
        demo_game: Option<&GameState>,
        animating_game_states: &mut Option<(Vec<GameState>, Vec<usize>)>,
        slide_elapsed: f64,
    ) {
        let title_color = ui.visuals().strong_text_color();
        ui.add_space(14.0);
        ui.label(
            egui::RichText::new(self.title())
                .size(19.0)
                .color(title_color)
                .strong(),
        );
        ui.add_space(8.0);

        let hex_fill = ui.visuals().extreme_bg_color;
        let hex_stroke = ui.visuals().text_color();
        let available_rect = ui.available_rect_before_wrap();
        match self {
            Self::Welcome => {
                ui.label(
                    egui::RichText::new(
                        "Place tiles to connect the paths — guide your player to victory.",
                    )
                    .size(14.5)
                    .italics(),
                );
                ui.add_space(6.0);
                let tiles = demo_tiles();
                let body_h = available_rect.height().max(1.0);
                let r_from_h = (body_h / 2.4) as f64;
                let spacing = ui.spacing().item_spacing.x;
                let body_w = (available_rect.width() - 48.0).max(1.0);
                let r_from_w = ((body_w - 2.0 * spacing) / (tiles.len() as f32 * 2.4)) as f64;
                let tile_r = r_from_h.min(r_from_w).max(10.0);
                let tile_side = tile_r as f32 * 2.4;
                let total_w = tiles.len() as f32 * tile_side + (tiles.len() - 1) as f32 * spacing;
                let left_pad = (available_rect.width() - total_w).max(0.0) / 2.0;
                ui.horizontal(|ui| {
                    ui.add_space(left_pad);
                    for (connectors, color) in &tiles {
                        draw_tile_preview(
                            ui, tile_r, connectors, false, hex_fill, hex_stroke, *color,
                        );
                    }
                });
            }
            Self::PlacingTilesRotating => {
                let (tile, color) = demo_tiles()[2].clone();
                let mut rotated = Tile {
                    inner_connectors: tile.clone(),
                };
                rotated.rotate(hexagon_types::TileRotationDirection::CounterClockwise);

                let body_h = available_rect.height().max(10.0);
                const CENTER_W: f32 = 150.0;
                let side_w = (available_rect.width() - CENTER_W).max(0.0) / 2.0;
                let tile_r = ((side_w - 8.0) / 2.4).min(body_h / 2.4).max(8.0) as f64;
                let top_pad = (body_h - tile_r as f32 * 2.4).max(0.0) / 2.0;

                egui_extras::StripBuilder::new(ui)
                    .size(egui_extras::Size::exact(side_w))
                    .size(egui_extras::Size::exact(CENTER_W))
                    .size(egui_extras::Size::exact(side_w))
                    .horizontal(|mut strip| {
                        strip.cell(|ui| {
                            ui.vertical_centered(|ui| {
                                ui.add_space(top_pad);
                                draw_tile_preview(
                                    ui, tile_r, &tile, false, hex_fill, hex_stroke, color,
                                );
                            });
                        });
                        strip.cell(|ui| {
                            let lines = ["You can rotate the tile", "by clicking ↺"];
                            let line_h = 14.5 + 6.0;
                            let text_h = lines.len() as f32 * line_h;
                            ui.add_space((body_h - text_h).max(0.0) / 2.0);
                            ui.vertical_centered_justified(|ui| {
                                for line in lines {
                                    ui.label(egui::RichText::new(line).size(14.5));
                                    ui.add_space(6.0);
                                }
                            });
                        });
                        strip.cell(|ui| {
                            ui.vertical_centered(|ui| {
                                ui.add_space(top_pad);
                                draw_tile_preview(
                                    ui,
                                    tile_r,
                                    &rotated.inner_connectors,
                                    false,
                                    hex_fill,
                                    hex_stroke,
                                    color,
                                );
                            });
                        });
                    });
            }
            Self::Acknowledgments => {
                let text_color = ui.visuals().text_color();
                let normal = egui::TextFormat {
                    font_id: egui::FontId::proportional(14.5),
                    color: text_color,
                    ..Default::default()
                };
                let italic = egui::TextFormat {
                    font_id: egui::FontId::proportional(14.5),
                    italics: true,
                    color: text_color,
                    ..Default::default()
                };
                let mut job = egui::text::LayoutJob::default();
                job.append("This game is inspired by ", 0.0, normal.clone());
                job.append("Tsuro — The Game of the Path", 0.0, italic);
                job.append(
                    ",\nadapted for hexagonal tiles and single player.",
                    0.0,
                    normal,
                );
                ui.label(job);
                ui.add_space(10.0);
                ui.label(
                    egui::RichText::new("Music")
                        .size(14.5)
                        .strong()
                        .color(ui.visuals().strong_text_color()),
                );
                ui.add_space(4.0);
                ui.label(
                    egui::RichText::new(
                        "All tracks from Pixabay (pixabay.com/music) — artists:\n\
                         \u{2022} Alex Zavesa\n\
                         \u{2022} LightBeatsMusic\n\
                         \u{2022} MagpieMusic\n\
                         \u{2022} Starostin\n\
                         \u{2022} MiroMaxMusic\n\
                         \u{2022} KornevMusic\n\
                         \u{2022} FreeMusicLab",
                    )
                    .size(14.5),
                );
            }
            Self::AnimatingGame => {
                let (states, played) = animating_game_states
                    .as_ref()
                    .map_or((&[][..], &[][..]), |(s, p)| (s.as_slice(), p.as_slice()));
                show_animating_game_slide(ui, &ANIM_TIMING, states, played, slide_elapsed);
            }
            Self::PlacingTilesPlaying => {
                ui.label(egui::RichText::new(
                    "Select a tile from your hand, rotate it with ↺ / ↻, then press ➡ to play it.\n\
                     All players advance along the path after each tile.",
                ).size(14.5));
                let Some(before) = demo_game else {
                    return;
                };
                let mut after = before.clone();
                after.play_tile(0);

                let rendering_data = crate::app::RenderingData::default();
                let board_h = available_rect.height().max(10.0);
                let middle_w = (available_rect.width() * 0.26).max(50.0);
                let board_w = ((available_rect.width() - middle_w) / 2.0).max(10.0);

                // Tile to be played: current player's first hand tile
                let tile_connectors: &[ConnectorEdgeSub] = before
                    .players
                    .iter()
                    .find(|p| p.id == before.current_player)
                    .and_then(|p| p.hand.first())
                    .map(|t| t.inner_connectors.as_slice())
                    .unwrap_or(&[]);
                let tile_color = crate::panels::color_to_egui(
                    rendering_data
                        .player_colors
                        .get(&before.current_player)
                        .copied()
                        .unwrap_or(hexagon_engine::Color::Green),
                );
                let label_h = 32.0;
                let tile_r = ((middle_w - 8.0) / 2.4 / 1.5)
                    .min((board_h - label_h) / 2.4)
                    .max(8.0) as f64;

                ui.horizontal(|ui| {
                    ui.allocate_ui(egui::vec2(board_w, board_h), |ui| {
                        let mut bi = crate::app::BoardInteraction {
                            animation_t: 1.0,
                            ..Default::default()
                        };
                        crate::panels::game_board::show(ui, before, &rendering_data, &mut bi);
                    });
                    ui.allocate_ui(egui::vec2(middle_w, board_h), |ui| {
                        ui.vertical_centered(|ui| {
                            let top_pad = (board_h - tile_r as f32 * 2.4 - label_h).max(0.0) / 2.0;
                            ui.add_space(top_pad);
                            draw_tile_preview(
                                ui,
                                tile_r,
                                tile_connectors,
                                true,
                                hex_fill,
                                hex_stroke,
                                tile_color,
                            );
                            ui.add_space(4.0);
                            ui.label(
                                egui::RichText::new("play by pressing ➡")
                                    .size(14.5)
                                    .color(ui.visuals().weak_text_color()),
                            );
                        });
                    });
                    ui.allocate_ui(egui::vec2(board_w, board_h), |ui| {
                        let mut bi = crate::app::BoardInteraction {
                            animation_t: 1.0,
                            ..Default::default()
                        };
                        crate::panels::game_board::show(ui, &after, &rendering_data, &mut bi);
                    });
                });
            }
        }
    }
}

fn demo_tiles() -> [(Vec<ConnectorEdgeSub>, egui::Color32); 3] {
    let es = |edge, sub| EdgeSub { edge, sub };
    let c = |a, b| ConnectorEdgeSub { a, b };

    let ring = vec![
        c(es(Edge::Top, Sub::Left), es(Edge::TopRight, Sub::Right)),
        c(es(Edge::Top, Sub::Right), es(Edge::TopLeft, Sub::Left)),
        c(
            es(Edge::TopLeft, Sub::Right),
            es(Edge::BottomLeft, Sub::Left),
        ),
        c(
            es(Edge::BottomLeft, Sub::Right),
            es(Edge::Bottom, Sub::Left),
        ),
        c(
            es(Edge::Bottom, Sub::Right),
            es(Edge::BottomRight, Sub::Left),
        ),
        c(
            es(Edge::BottomRight, Sub::Right),
            es(Edge::TopRight, Sub::Left),
        ),
    ];
    let crossing = vec![
        c(es(Edge::Top, Sub::Left), es(Edge::Bottom, Sub::Right)),
        c(es(Edge::Top, Sub::Right), es(Edge::Bottom, Sub::Left)),
        c(
            es(Edge::TopLeft, Sub::Left),
            es(Edge::BottomRight, Sub::Right),
        ),
        c(
            es(Edge::TopLeft, Sub::Right),
            es(Edge::BottomRight, Sub::Left),
        ),
        c(
            es(Edge::BottomLeft, Sub::Left),
            es(Edge::TopRight, Sub::Right),
        ),
        c(
            es(Edge::BottomLeft, Sub::Right),
            es(Edge::TopRight, Sub::Left),
        ),
    ];
    let spiral = vec![
        c(es(Edge::Top, Sub::Left), es(Edge::BottomLeft, Sub::Right)),
        c(es(Edge::Top, Sub::Right), es(Edge::BottomRight, Sub::Left)),
        c(es(Edge::TopLeft, Sub::Left), es(Edge::Bottom, Sub::Right)),
        c(
            es(Edge::TopLeft, Sub::Right),
            es(Edge::BottomLeft, Sub::Left),
        ),
        c(es(Edge::Bottom, Sub::Left), es(Edge::TopRight, Sub::Right)),
        c(
            es(Edge::BottomRight, Sub::Right),
            es(Edge::TopRight, Sub::Left),
        ),
    ];
    [
        (ring, egui::Color32::from_rgb(70, 130, 200)),
        (crossing, egui::Color32::from_rgb(190, 80, 70)),
        (spiral, egui::Color32::from_rgb(60, 160, 90)),
    ]
}
fn init_demo_game() -> GameState {
    let mut game = GameOptions::Standard(GameOptionsStandard {
        board_radius: 2,
        outer_connectors: OuterConnectors::ReducedDeathEnds,
        random_seed: 35646,
        player_count: 1,
        collision_mode: CollisionMode::PassThrough,
        winning_condition: WinningCondition::LongestWay,
        hand_size: 3,
    })
    .start_game()
    .expect("demo game");
    for _ in 0..1 {
        if game.result.is_none() {
            game.play_tile(0);
        }
    }
    game
}

fn init_animating_game_states() -> (Vec<GameState>, Vec<usize>) {
    const ANIM_TILE_PICKS: [usize; ANIM_TILE_COUNT] = [1, 2, 1, 0];
    let json = include_str!("full_game_start.json");
    let mut game: GameState = serde_json::from_str(json).expect("full_game_start.json");

    let mut states = vec![game.clone()];
    let mut played_indices: Vec<usize> = Vec::new();
    for &pick in &ANIM_TILE_PICKS {
        let hand_len = game
            .players
            .iter()
            .find(|p| p.id == game.current_player)
            .map_or(1, |p| p.hand.len().max(1));
        let idx = pick % hand_len;
        played_indices.push(idx);
        game.play_tile(idx);
        states.push(game.clone());
    }
    (states, played_indices)
}

fn show_animating_game_slide(
    ui: &mut egui::Ui,
    timing: &AnimSlideTiming,
    states: &[GameState],
    played_indices: &[usize],
    elapsed: f64,
) {
    let num_tiles = states.len().saturating_sub(1);
    if num_tiles == 0 {
        return;
    }

    let per_tile =
        timing.board_show_secs + timing.tile_show_secs + timing.play_secs + timing.player_anim_secs;
    let total = per_tile * num_tiles as f64 + timing.board_show_secs;
    let t = elapsed % total;

    // Determine phase: (board state index, tile to show from which state index, animation_t, text)
    let (board_idx, show_tile_from, anim_t, status_text): (usize, Option<usize>, f32, &str) =
        if t >= total - timing.board_show_secs {
            (num_tiles, None, 1.0, PHASE_TEXTS[4])
        } else {
            let tile_idx = ((t / per_tile) as usize).min(num_tiles - 1);
            let t_in = t - tile_idx as f64 * per_tile;

            if t_in < timing.board_show_secs {
                (tile_idx, None, 1.0, PHASE_TEXTS[0])
            } else if t_in < timing.board_show_secs + timing.tile_show_secs {
                (tile_idx, Some(tile_idx), 1.0, PHASE_TEXTS[1])
            } else if t_in < timing.board_show_secs + timing.tile_show_secs + timing.play_secs {
                let progress = ((t_in - timing.board_show_secs - timing.tile_show_secs)
                    / timing.play_secs) as f32;
                (tile_idx + 1, Some(tile_idx), progress, PHASE_TEXTS[2])
            } else {
                let text = if tile_idx == num_tiles - 1 {
                    PHASE_TEXTS[4]
                } else {
                    PHASE_TEXTS[3]
                };
                (tile_idx + 1, None, 1.0, text)
            }
        };

    // Collect tile connectors now (owned) to avoid borrow conflicts with board_state below
    let tile_preview: Option<Vec<ConnectorEdgeSub>> = show_tile_from.and_then(|idx| {
        let s = &states[idx];
        let hand_idx = played_indices.get(idx).copied().unwrap_or(0);
        s.players
            .iter()
            .find(|p| p.id == s.current_player)
            .and_then(|p| p.hand.get(hand_idx))
            .map(|t| t.inner_connectors.clone())
    });

    let board_state = &states[board_idx];
    let rendering_data = crate::app::RenderingData::default();
    let hex_fill = ui.visuals().extreme_bg_color;
    let hex_stroke = ui.visuals().text_color();
    let available_rect = ui.available_rect_before_wrap();
    let board_h = available_rect.height().max(10.0);

    let tile_color = crate::panels::color_to_egui(
        rendering_data
            .player_colors
            .get(&board_state.current_player)
            .copied()
            .unwrap_or(hexagon_engine::Color::Green),
    );

    const CENTER_W: f32 = 150.0;
    let side_w = (available_rect.width() - CENTER_W).max(0.0) / 2.0;

    let tile_r = ((side_w - 8.0) / 2.4).min(board_h / 2.4).max(8.0) as f64;
    let top_pad = (board_h - tile_r as f32 * 2.4).max(0.0) / 2.0;

    egui_extras::StripBuilder::new(ui)
        .size(egui_extras::Size::exact(side_w))
        .size(egui_extras::Size::exact(CENTER_W))
        .size(egui_extras::Size::exact(side_w))
        .horizontal(|mut strip| {
            strip.cell(|ui| {
                ui.vertical_centered(|ui| {
                    ui.add_space(top_pad);
                    if let Some(connectors) = &tile_preview {
                        draw_tile_preview(
                            ui, tile_r, connectors, true, hex_fill, hex_stroke, tile_color,
                        );
                    }
                });
            });
            strip.cell(|ui| {
                let active = ui.visuals().strong_text_color();
                let dim = ui.visuals().weak_text_color();
                let text_h: f32 = PHASE_TEXTS.len() as f32 * (12.5 + 6.0);
                let top_pad = (board_h - text_h).max(0.0) / 2.0;
                ui.add_space(top_pad);
                ui.vertical_centered(|ui| {
                    for &label in &PHASE_TEXTS {
                        let is_active = label == status_text;
                        let text = egui::RichText::new(label).size(12.5).color(if is_active {
                            active
                        } else {
                            dim
                        });
                        let text = if is_active { text.strong() } else { text };
                        ui.label(text);
                        ui.add_space(6.0);
                    }
                });
            });
            strip.cell(|ui| {
                let mut bi = crate::app::BoardInteraction {
                    animation_t: anim_t,
                    ..Default::default()
                };
                crate::panels::game_board::show(ui, board_state, &rendering_data, &mut bi);
            });
        });
}

pub struct SlideshowState {
    current: usize,
    last_changed: Option<f64>,
    auto_cycle: bool,
    demo_game: Option<GameState>,
    animating_game_states: Option<(Vec<GameState>, Vec<usize>)>,
}

impl Default for SlideshowState {
    fn default() -> Self {
        Self {
            current: 0,
            last_changed: None,
            auto_cycle: true,
            demo_game: None,
            animating_game_states: None,
        }
    }
}

impl SlideshowState {
    pub fn show(&mut self, ui: &mut egui::Ui, missions_available: bool) -> bool {
        if self.demo_game.is_none() {
            self.demo_game = Some(init_demo_game());
        }
        if self.animating_game_states.is_none() {
            self.animating_game_states = Some(init_animating_game_states());
        }
        let now = ui.ctx().input(|i| i.time);
        if self.last_changed.is_none() {
            self.last_changed = Some(now);
        }

        let is_anim_slide = [Slide::AnimatingGame].contains(&Slide::VARIANTS[self.current]);

        ui.ctx().request_repaint_after(if is_anim_slide {
            Duration::from_millis(33)
        } else {
            Duration::from_millis(500)
        });

        if self.auto_cycle {
            let elapsed = now - self.last_changed.unwrap_or(now);
            let cycle_secs = if is_anim_slide {
                let n = self
                    .animating_game_states
                    .as_ref()
                    .map_or(ANIM_TILE_COUNT, |s| s.0.len().saturating_sub(1));
                let per = ANIM_TIMING.board_show_secs
                    + ANIM_TIMING.tile_show_secs
                    + ANIM_TIMING.play_secs
                    + ANIM_TIMING.player_anim_secs;
                n as f64 * per + ANIM_TIMING.board_show_secs
            } else {
                AUTO_CYCLE_SECS
            };
            if elapsed >= cycle_secs {
                self.current = (self.current + 1) % Slide::VARIANTS.len();
                self.last_changed = Some(now);
            }
        }

        let mut start_clicked = false;

        ui.vertical_centered(|ui| {
            ui.add_space(32.0);
            ui.label(egui::RichText::new("Introduction").size(48.0));
            ui.add_space(20.0);

            ui.with_layout(
                egui::Layout::bottom_up(egui::Align::Center).with_cross_justify(true),
                |ui| {
                    if ui
                        .add_enabled(
                            missions_available,
                            egui::Button::new(egui::RichText::new("Start game").size(20.0))
                                .min_size(egui::vec2(160.0, 40.0)),
                        )
                        .clicked()
                    {
                        start_clicked = true;
                    }
                    ui.add_space(20.0);

                    self.draw_dots(now, ui);
                    self.show_slide(now, ui);

                    ui.with_layout(
                        egui::Layout::top_down(egui::Align::Center).with_cross_justify(true),
                        |ui| {
                            Slide::VARIANTS[self.current].show(
                                ui,
                                self.demo_game.as_ref(),
                                &mut self.animating_game_states,
                                now - self.last_changed.unwrap_or(now),
                            )
                        },
                    );
                },
            );
        });

        start_clicked
    }

    fn draw_dots(&mut self, now: f64, ui: &mut egui::Ui) {
        let text_color = ui.visuals().text_color();
        let dot_area_width = Slide::VARIANTS.len() as f32 * DOT_SPACING;
        ui.horizontal(|ui| {
            ui.add_space((ui.available_width() - dot_area_width) / 2.0);
            for (i, _) in Slide::VARIANTS.iter().enumerate() {
                let (dot_rect, dot_response) = ui.allocate_exact_size(
                    egui::vec2(DOT_SPACING, DOT_SPACING),
                    egui::Sense::click(),
                );
                let color = if i == self.current {
                    text_color
                } else {
                    ui.visuals().weak_text_color()
                };
                ui.painter()
                    .circle_filled(dot_rect.center(), DOT_RADIUS, color);
                if dot_response.clicked() {
                    self.current = i;
                    self.auto_cycle = false;
                    self.last_changed = Some(now);
                }
            }
        });
        ui.add_space(8.0);
    }

    fn show_slide(&mut self, now: f64, ui: &mut egui::Ui) {
        if self.auto_cycle {
            let bar_h = DOT_RADIUS * 0.6;
            let available_width = ui.available_width();
            let slide_rect = ui.allocate_space(egui::vec2(available_width, bar_h)).1;

            let elapsed = now - self.last_changed.unwrap_or(now);
            let progress = (elapsed / AUTO_CYCLE_SECS).clamp(0.0, 1.0) as f32;
            let (filled, empty) = slide_rect.split_left_right_at_fraction(progress);
            ui.painter()
                .rect_filled(empty, 10.0, ui.visuals().faint_bg_color);
            ui.painter()
                .rect_filled(filled, 0.0, ui.visuals().selection.bg_fill);
        }
    }
}
