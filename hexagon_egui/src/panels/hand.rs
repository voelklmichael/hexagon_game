use hexagon_engine::{
    ConnectorEdgeSub, Edge, EdgeSub, GameResult, GameState, Sub,
    TileRotationDirection,
};

use crate::app::{BoardInteraction, GameHistory, RenderingData};
use crate::panels::color_to_egui;

const PREVIEW_R: f64 = 28.0;

fn preview_h() -> f64 {
    PREVIEW_R * 3.0_f64.sqrt() / 2.0
}

fn edge_sub_point(edge_sub: &EdgeSub) -> (f64, f64) {
    let h = preview_h();
    let v = [
        (PREVIEW_R, 0.0),
        (PREVIEW_R / 2.0, h),
        (-PREVIEW_R / 2.0, h),
        (-PREVIEW_R, 0.0),
        (-PREVIEW_R / 2.0, -h),
        (PREVIEW_R / 2.0, -h),
    ];
    let (a, b) = match edge_sub.edge {
        Edge::Top => (v[5], v[4]),
        Edge::TopLeft => (v[4], v[3]),
        Edge::BottomLeft => (v[3], v[2]),
        Edge::Bottom => (v[2], v[1]),
        Edge::BottomRight => (v[1], v[0]),
        Edge::TopRight => (v[0], v[5]),
    };
    let t = match edge_sub.sub {
        Sub::Left => 0.25,
        Sub::Right => 0.75,
    };
    (a.0 + t * (b.0 - a.0), a.1 + t * (b.1 - a.1))
}

fn edge_inward_normal(edge: &Edge) -> (f64, f64) {
    let s = 3.0_f64.sqrt() / 2.0;
    match edge {
        Edge::Top => (0.0, 1.0),
        Edge::TopLeft => (s, 0.5),
        Edge::BottomLeft => (s, -0.5),
        Edge::Bottom => (0.0, -1.0),
        Edge::BottomRight => (-s, -0.5),
        Edge::TopRight => (-s, 0.5),
    }
}

fn draw_tile_preview(
    ui: &mut egui::Ui,
    connectors: &[ConnectorEdgeSub],
    selected: bool,
    hex_fill: egui::Color32,
    hex_stroke: egui::Color32,
    connector_color: egui::Color32,
) -> egui::Response {
    let h = preview_h();
    let size = egui::vec2(PREVIEW_R as f32 * 2.4, PREVIEW_R as f32 * 2.4);
    let (rect, response) = ui.allocate_exact_size(size, egui::Sense::click());
    let painter = ui.painter_at(rect);
    let cx = rect.center().x;
    let cy = rect.center().y;

    let to = |wx: f64, wy: f64| egui::pos2(cx + wx as f32, cy + wy as f32);

    let hex_verts: Vec<egui::Pos2> = [
        (PREVIEW_R, 0.0),
        (PREVIEW_R / 2.0, h),
        (-PREVIEW_R / 2.0, h),
        (-PREVIEW_R, 0.0),
        (-PREVIEW_R / 2.0, -h),
        (PREVIEW_R / 2.0, -h),
    ]
    .iter()
    .map(|&(x, y)| to(x, y))
    .collect();

    let border_stroke = if selected {
        egui::Stroke::new(3.0_f32, egui::Color32::GOLD)
    } else {
        egui::Stroke::new(1.5_f32, hex_stroke)
    };
    painter.add(egui::Shape::convex_polygon(hex_verts, hex_fill, border_stroke));

    let ctrl = PREVIEW_R * 0.6;
    for ConnectorEdgeSub { a, b } in connectors {
        let (ax, ay) = edge_sub_point(a);
        let (bx, by) = edge_sub_point(b);
        let (nax, nay) = edge_inward_normal(&a.edge);
        let (nbx, nby) = edge_inward_normal(&b.edge);
        painter.add(egui::Shape::CubicBezier(egui::epaint::CubicBezierShape {
            points: [
                to(ax, ay),
                to(ax + ctrl * nax, ay + ctrl * nay),
                to(bx + ctrl * nbx, by + ctrl * nby),
                to(bx, by),
            ],
            closed: false,
            fill: egui::Color32::TRANSPARENT,
            stroke: egui::Stroke::new(2.5_f32, connector_color).into(),
        }));
    }

    response
}

pub fn show(
    ui: &mut egui::Ui,
    game: &mut GameState,
    rendering_data: &RenderingData,
    interaction: &mut BoardInteraction,
    history: &mut GameHistory,
) {
    ui.heading("Player Hand");

    let current_id = game.current_player;

    let Some(player) = game.players.iter().find(|p| p.id == current_id) else {
        return;
    };

    let player_color = rendering_data
        .player_colors
        .get(&current_id)
        .copied()
        .unwrap_or(hexagon_engine::Color::Gray);

    if let Some(result) = &game.result {
        ui.label(egui::RichText::new("The game is finished").strong().heading());
        let (text, color) = match result {
            GameResult::Win(winners) => {
                let names: Vec<String> = winners.iter().map(|p| format!("Player {}", p.0 + 1)).collect();
                (format!("Winner: {}", names.join(", ")), egui::Color32::GOLD)
            }
            GameResult::Draw(players) => {
                let names: Vec<String> = players.iter().map(|p| format!("Player {}", p.0 + 1)).collect();
                (format!("Draw: {}", names.join(", ")), egui::Color32::from_rgb(180, 180, 180))
            }
            GameResult::Loss => ("Loss".to_string(), egui::Color32::from_rgb(200, 60, 60)),
        };
        ui.label(egui::RichText::new(text).strong().color(color).heading());
        return;
    }

    ui.horizontal(|ui| {
        let (rect, _) = ui.allocate_exact_size(egui::vec2(16.0, 16.0), egui::Sense::hover());
        ui.painter().rect_filled(rect, 2.0, color_to_egui(player_color));
        ui.label(format!("Player {}", current_id.0 + 1));
    });

    let disabled = player.is_npc;

    let hand_len = player.hand.len();
    let selected_tile = interaction.selected_tile;
    let hex_fill = color_to_egui(rendering_data.hex_fill);
    let hex_stroke = color_to_egui(rendering_data.hex_stroke);
    let connector_color = color_to_egui(player_color);

    let connectors_per_tile: Vec<Vec<ConnectorEdgeSub>> = game
        .players
        .iter()
        .find(|p| p.id == current_id)
        .map(|p| p.hand.iter().map(|t| t.inner_connectors.clone()).collect())
        .unwrap_or_default();

    ui.add_enabled_ui(!disabled, |ui| {
        ui.horizontal_wrapped(|ui| {
            for i in 0..hand_len {
                ui.vertical(|ui| {
                    let connectors = connectors_per_tile.get(i).map(Vec::as_slice).unwrap_or(&[]);
                    let resp = draw_tile_preview(
                        ui,
                        connectors,
                        selected_tile == Some(i),
                        hex_fill,
                        hex_stroke,
                        connector_color,
                    );
                    if resp.clicked() {
                        interaction.selected_tile = if selected_tile == Some(i) {
                            None
                        } else {
                            Some(i)
                        };
                        interaction.animation_t = 1.0;
                    }

                    ui.horizontal(|ui| {
                        if ui.small_button("↺").clicked() {
                            game.rotate_tile(i, TileRotationDirection::CounterClockwise);
                            interaction.selected_tile = Some(i);
                            interaction.animation_t = 1.0;
                        }
                        if ui.small_button("↻").clicked() {
                            game.rotate_tile(i, TileRotationDirection::Clockwise);
                            interaction.selected_tile = Some(i);
                            interaction.animation_t = 1.0;
                        }
                    });
                });
            }
        });

        if let Some(tile_index) = interaction.selected_tile {
            if ui.button("Play selected tile").clicked() {
                history.undo_stack.push(game.clone());
                history.redo_stack.clear();
                game.play_tile(tile_index);
                interaction.selected_tile = None;
                interaction.selected_hexagon = None;
                interaction.animation_t = 0.0;
            }
        }
    });
}
