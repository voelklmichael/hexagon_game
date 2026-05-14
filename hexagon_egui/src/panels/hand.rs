use hexagon_engine::{
    ConnectorEdgeSub, Edge, EdgeSub, GameResult, GameState, Sub, TileRotationDirection,
};

use crate::app::{BoardInteraction, GameHistory, RenderingData};
use crate::panels::color_to_egui;

fn hex_h(r: f64) -> f64 {
    r * 3.0_f64.sqrt() / 2.0
}

fn edge_sub_point(r: f64, edge_sub: &EdgeSub) -> (f64, f64) {
    let h = hex_h(r);
    let v = [
        (r, 0.0),
        (r / 2.0, h),
        (-r / 2.0, h),
        (-r, 0.0),
        (-r / 2.0, -h),
        (r / 2.0, -h),
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
    r: f64,
    connectors: &[ConnectorEdgeSub],
    selected: bool,
    hex_fill: egui::Color32,
    hex_stroke: egui::Color32,
    connector_color: egui::Color32,
) -> egui::Response {
    let h = hex_h(r);
    let side = r as f32 * 2.4;
    let (rect, response) = ui.allocate_exact_size(egui::vec2(side, side), egui::Sense::click());
    let painter = ui.painter_at(rect);
    let cx = rect.center().x;
    let cy = rect.center().y;

    let to = |wx: f64, wy: f64| egui::pos2(cx + wx as f32, cy + wy as f32);

    let hex_verts: Vec<egui::Pos2> = [
        (r, 0.0),
        (r / 2.0, h),
        (-r / 2.0, h),
        (-r, 0.0),
        (-r / 2.0, -h),
        (r / 2.0, -h),
    ]
    .iter()
    .map(|&(x, y)| to(x, y))
    .collect();

    let border_stroke = if selected {
        egui::Stroke::new(3.0_f32, egui::Color32::GOLD)
    } else {
        egui::Stroke::new(1.5_f32, hex_stroke)
    };
    painter.add(egui::Shape::convex_polygon(
        hex_verts,
        hex_fill,
        border_stroke,
    ));

    let ctrl = r * 0.6;
    for ConnectorEdgeSub { a, b } in connectors {
        let (ax, ay) = edge_sub_point(r, a);
        let (bx, by) = edge_sub_point(r, b);
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
    game: &mut Option<GameState>,
    rendering_data: &RenderingData,
    interaction: &mut BoardInteraction,
    history: &mut GameHistory,
    current_mission: &mut Option<usize>,
    missions_won: &mut std::collections::HashSet<uuid::Uuid>,
    user_id: Option<uuid::Uuid>,
    backend: &mut crate::app::BackendReqwest,
) -> bool {
    ui.heading("Player Hand");
    let mut started = false;

    let can_undo = !history.undo_stack.is_empty();
    let can_redo = !history.redo_stack.is_empty();
    ui.horizontal(|ui| {
        if ui
            .add_enabled(can_undo, egui::Button::new("↩ Undo"))
            .clicked()
            && let Some(prev) = history.undo_stack.pop()
        {
            if let Some(current) = game.take() {
                history.redo_stack.push(current);
            }
            *game = Some(prev);
        }
        if ui
            .add_enabled(can_redo, egui::Button::new("↪ Redo"))
            .clicked()
            && let Some(next) = history.redo_stack.pop()
        {
            if let Some(current) = game.take() {
                history.undo_stack.push(current);
            }
            *game = Some(next);
        }
    });

    if game.is_none() {
        ui.label("No game in progress.");
        return false;
    }

    // Game-over section: collect all data from a scoped borrow so the borrow
    // is released before we write back to *game.
    let game_over_data: Option<(String, egui::Color32, _, _, bool, bool)> = {
        let g = game.as_ref().unwrap();
        g.result.as_ref().map(|result| {
            let (text, color, is_win) = match result {
                GameResult::Win(winners) => {
                    let names: Vec<String> = winners
                        .iter()
                        .map(|p| format!("Player {}", p.0 + 1))
                        .collect();
                    (
                        format!("Winner: {}", names.join(", ")),
                        egui::Color32::GOLD,
                        true,
                    )
                }
                GameResult::Draw(players) => {
                    let names: Vec<String> = players
                        .iter()
                        .map(|p| format!("Player {}", p.0 + 1))
                        .collect();
                    (
                        format!("Draw: {}", names.join(", ")),
                        egui::Color32::from_rgb(180, 180, 180),
                        false,
                    )
                }
                GameResult::Loss => (
                    "Loss".to_string(),
                    egui::Color32::from_rgb(200, 60, 60),
                    false,
                ),
            };
            let opts = g.options.clone();
            let saved = g.clone();
            let can_undo = !history.undo_stack.is_empty();
            (text, color, opts, saved, can_undo, is_win)
        })
    }; // immutable borrow of *game released here

    if let Some((text, color, opts, saved, can_undo, is_win)) = game_over_data {
        if is_win {
            if let Some(cur) = *current_mission {
                if let Some(mission_id) = crate::panels::missions::mission_id(cur) {
                    let is_new = missions_won.insert(mission_id);
                    if is_new {
                        if let Some(uid) = user_id {
                            backend.report_mission_done(uid, mission_id, &saved.statistics);
                        }
                    }
                }
            }
        }

        ui.label(
            egui::RichText::new("The game is finished")
                .strong()
                .heading(),
        );
        ui.label(egui::RichText::new(text).strong().color(color).heading());
        ui.separator();

        let next_mission = current_mission
            .map(|idx| idx + 1)
            .filter(|&next| next < crate::panels::missions::mission_count());

        if let Some(next_idx) = next_mission {
            if ui.button("Start next mission").clicked() {
                history.undo_stack.clear();
                history.redo_stack.clear();
                crate::panels::missions::start_mission(next_idx, game);
                *current_mission = Some(next_idx);
                started = true;
            }
        } else {
            if ui.button("Start new game").clicked() {
                let mut new_opts = opts.clone();
                new_opts.randomize_seed();
                match new_opts.start_game() {
                    Ok(g) => {
                        history.undo_stack.clear();
                        history.redo_stack.clear();
                        *game = Some(g);
                        *current_mission = None;
                        started = true;
                    }
                    Err(e) => eprintln!("New game failed: {e}"),
                }
            }
        }
        let restart_label = if current_mission.is_some() {
            "Restart this mission"
        } else {
            "Restart this game"
        };
        if ui.button(restart_label).clicked() {
            match opts.start_game() {
                Ok(g) => {
                    history.undo_stack.clear();
                    history.redo_stack.clear();
                    *game = Some(g);
                    started = true;
                }
                Err(e) => eprintln!("Restart failed: {e}"),
            }
        }
        if ui
            .add_enabled(can_undo, egui::Button::new("Undo"))
            .clicked()
            && let Some(prev) = history.undo_stack.pop()
        {
            history.redo_stack.push(saved);
            *game = Some(prev);
        }

        if is_win {
            let rect = ui.max_rect();
            let dt = ui.ctx().input(|i| i.stable_dt).min(0.05);
            interaction.confetti.activate();
            interaction.confetti.update_and_draw(dt, rect, ui.painter());
            if interaction.confetti.is_active() {
                ui.ctx().request_repaint();
            }
        }
        return started;
    }

    // Normal hand display — borrow game mutably for the rest of the function.
    let game_state = game.as_mut().unwrap();
    let current_id = game_state.current_player;

    let Some(player) = game_state.players.iter().find(|p| p.id == current_id) else {
        return false;
    };

    let player_color = rendering_data
        .player_colors
        .get(&current_id)
        .copied()
        .unwrap_or(hexagon_engine::Color::Gray);

    ui.horizontal(|ui| {
        let (rect, _) = ui.allocate_exact_size(egui::vec2(16.0, 16.0), egui::Sense::hover());
        ui.painter()
            .rect_filled(rect, 2.0, color_to_egui(player_color));
        ui.label(format!("Player {}", current_id.0 + 1));
    });

    let disabled = player.is_npc;

    let hand_len = player.hand.len();
    let selected_tile = interaction.selected_tile;
    let hex_fill = color_to_egui(rendering_data.hex_fill);
    let hex_stroke = color_to_egui(rendering_data.hex_stroke);
    let connector_color = color_to_egui(player_color);

    let connectors_per_tile: Vec<Vec<ConnectorEdgeSub>> = game_state
        .players
        .iter()
        .find(|p| p.id == current_id)
        .map(|p| p.hand.iter().map(|t| t.inner_connectors.clone()).collect())
        .unwrap_or_default();

    // Compute r here, before the ScrollArea makes available_height() infinite.
    // clip_rect() is the actual visible panel rect; cursor() tracks how much is used.
    let r = {
        let available_w = ui.available_width();
        let sep_h = ui.spacing().item_spacing.y * 2.0 + 1.0;
        let used_h = ui.cursor().top() - ui.clip_rect().top();
        let remaining_h = (ui.clip_rect().height() - used_h).max(1.0);
        let r_from_h = if hand_len > 0 {
            (remaining_h - hand_len as f32 * sep_h) / (hand_len as f32 * 2.5)
        } else {
            f32::MAX
        };
        let r_from_w = available_w * 0.9 / 2.5;
        r_from_h.min(r_from_w).max(10.0) as f64
    };

    ui.add_enabled_ui(!disabled, |ui| {
        egui::ScrollArea::vertical().show(ui, |ui| {
            for i in 0..hand_len {
                let connectors = connectors_per_tile.get(i).map(Vec::as_slice).unwrap_or(&[]);
                let is_selected = selected_tile == Some(i);

                if i > 0 {
                    ui.separator();
                }

                ui.horizontal(|ui| {
                    let resp = draw_tile_preview(
                        ui,
                        r,
                        connectors,
                        is_selected,
                        hex_fill,
                        hex_stroke,
                        connector_color,
                    );
                    if resp.clicked() {
                        interaction.selected_tile = if is_selected { None } else { Some(i) };
                        interaction.animation_t = 1.0;
                    }

                    // Right-side button column, pinned to the hex geometry.
                    // hex bottom from tile-rect top = r*1.2 + hex_h(r)
                    let font_size = r as f32 * 0.3;
                    let btn_h = font_size + 8.0; // font + egui button padding
                    let hex_bottom = r as f32 * 1.2 + hex_h(r) as f32;

                    ui.vertical(|ui| {
                        if ui
                            .button(egui::RichText::new("↺").size(font_size))
                            .clicked()
                        {
                            game_state.rotate_tile(i, TileRotationDirection::CounterClockwise);
                            interaction.selected_tile = Some(i);
                            interaction.animation_t = 1.0;
                        }

                        if is_selected {
                            let gap = ((hex_bottom - 3.0 * btn_h) / 2.0).max(0.0);
                            ui.add_space(gap);
                            if ui
                                .button(egui::RichText::new("➡").size(font_size))
                                .clicked()
                            {
                                history.undo_stack.push(game_state.clone());
                                history.redo_stack.clear();
                                game_state.play_tile(i);
                                interaction.selected_tile = None;
                                interaction.animation_t = 0.0;
                            }
                            ui.add_space(gap);
                        } else {
                            ui.add_space((hex_bottom - 2.0 * btn_h).max(0.0));
                        }

                        if ui
                            .button(egui::RichText::new("↻").size(font_size))
                            .clicked()
                        {
                            game_state.rotate_tile(i, TileRotationDirection::Clockwise);
                            interaction.selected_tile = Some(i);
                            interaction.animation_t = 1.0;
                        }
                    });
                });
            }
        });
    });
    false
}
