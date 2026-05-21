use std::time::Duration;

use hexagon_engine::{ConnectorEdgeSub, Edge, EdgeSub, Sub};
use strum::VariantArray;

const AUTO_CYCLE_SECS: f64 = 5.0;
const DOT_RADIUS: f32 = 5.0;
const DOT_SPACING: f32 = 22.0;

#[derive(Clone, Copy, strum::VariantArray)]
enum Slide {
    Welcome,
    PlacingTilesRotating,
    PlacingTilesPlaying,
    ConnectorTypes,
    WinningConditions,
    Controls,
}

impl Slide {
    fn title(self) -> &'static str {
        match self {
            Self::Welcome => "Hexagon - The Game",
            Self::PlacingTilesRotating => "Placing Tiles",
            Self::PlacingTilesPlaying => "Placing Tiles",
            Self::ConnectorTypes => "Connector Types",
            Self::WinningConditions => "Winning Conditions",
            Self::Controls => "Controls",
        }
    }

    fn show(self, ui: &mut egui::Ui) {
        let title_color = ui.visuals().strong_text_color();
        ui.add_space(14.0);
        ui.label(
            egui::RichText::new(self.title())
                .size(19.0)
                .color(title_color)
                .strong(),
        );
        ui.add_space(8.0);

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
                let hex_fill = ui.visuals().extreme_bg_color;
                let available_rect = ui.available_rect_before_wrap();
                let hex_stroke = ui.visuals().text_color();
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
                        crate::panels::hand::draw_tile_preview(
                            ui, tile_r, connectors, false, hex_fill, hex_stroke, *color,
                        );
                    }
                });
            }
            Self::PlacingTilesRotating => {
                ui.label(egui::RichText::new(
                    "Select a tile from your hand, rotate it with ↺ / ↻, then press ➡ to play it.\n\
                     All players advance along the path after each tile.",
                ).size(14.5));
            }
            Self::PlacingTilesPlaying => {
                ui.label(egui::RichText::new(
                    "Select a tile from your hand, rotate it with ↺ / ↻, then press ➡ to play it.\n\
                     All players advance along the path after each tile.",
                ).size(14.5));
            }
            Self::ConnectorTypes => {
                ui.label(
                    egui::RichText::new(
                        "OnHex — a curve inside one hexagon.\n\
                     HexToHex — a passage between two adjacent hexagons.\n\
                     Outside — connects two non-adjacent outer edges.\n\
                     DeadEnd — no continuation; players stop here.",
                    )
                    .size(14.5),
                );
            }
            Self::WinningConditions => {
                ui.label(
                    egui::RichText::new(
                        "Last Man Standing — the last active player wins.\n\
                     Longest Way — greatest total path weight wins.\n\
                     Highest Velocity — greatest single-turn speed wins.",
                    )
                    .size(14.5),
                );
            }
            Self::Controls => {
                ui.label(
                    egui::RichText::new(
                        "Click a tile to select it  ·  ↺ / ↻ to rotate  ·  ➡ to play it\n\
                     Undo / Redo to step through placements\n\
                     Restart replays with the same seed  ·  New Game starts fresh",
                    )
                    .size(14.5),
                );
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
pub struct SlideshowState {
    current: usize,
    last_changed: Option<f64>,
    auto_cycle: bool,
}

impl Default for SlideshowState {
    fn default() -> Self {
        Self {
            current: 0,
            last_changed: None,
            auto_cycle: true,
        }
    }
}

impl SlideshowState {
    pub fn show(&mut self, ui: &mut egui::Ui, missions_available: bool) -> bool {
        let now = ui.ctx().input(|i| i.time);
        if self.last_changed.is_none() {
            self.last_changed = Some(now);
        }

        if self.auto_cycle {
            let elapsed = now - self.last_changed.unwrap_or(now);
            if elapsed >= AUTO_CYCLE_SECS {
                self.current = (self.current + 1) % Slide::VARIANTS.len();
                self.last_changed = Some(now);
            }
            ui.ctx().request_repaint_after(Duration::from_millis(500));
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
                        |ui| Slide::VARIANTS[self.current].show(ui),
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
