use hexagon_engine::{
    ConnectorEdgeSub, Edge, EdgeSub, GameOptions, GameResult, GameState, Sub, TileRotationDirection,
};

use crate::app::{BoardInteraction, GameHistory, MusicState, RenderingData};
use crate::panels::color_to_egui;

fn try_start_music(music: &mut MusicState, player: Option<&crate::music::MusicPlayer>) {
    if !music.started {
        music.started = true;
        if !music.paused
            && let Some(p) = player
        {
            p.play_track(music.current_track);
        }
    }
}

fn hex_h(r: f64) -> f64 {
    r * 3.0_f64.sqrt() / 2.0
}

/// Clockwise index 0–11 around the hexagon perimeter for each EdgeSub position.
fn edge_sub_index(es: &EdgeSub) -> u8 {
    let edge_base = match es.edge {
        Edge::Top => 0,
        Edge::TopLeft => 2,
        Edge::BottomLeft => 4,
        Edge::Bottom => 6,
        Edge::BottomRight => 8,
        Edge::TopRight => 10,
    };
    let sub_offset = match es.sub {
        Sub::Left => 0,
        Sub::Right => 1,
    };
    edge_base + sub_offset
}

/// Rotation-invariant distance (1–6) between two EdgeSub positions.
fn connector_distance(a: &EdgeSub, b: &EdgeSub) -> u8 {
    let d = edge_sub_index(a).abs_diff(edge_sub_index(b));
    d.min(12 - d)
}

/// Maps connector span to a stroke width.
/// Each sub-pairing group uses a non-monotonic lookup over its three possible distances.
fn width_for_connector(distance: u8, a_sub: Sub, b_sub: Sub) -> f32 {
    match (a_sub, b_sub) {
        // Cross-sub: distances 1, 3, 5 → rank 0, 1, 2
        (Sub::Left, Sub::Right) | (Sub::Right, Sub::Left) => {
            [5.0_f32, 1.5, 3.5][(distance - 1) as usize / 2]
        }
        // Left-Left: distances 2, 4, 6 → rank 0, 1, 2
        (Sub::Left, Sub::Left) => [2.5_f32, 6.0, 1.0][(distance as usize / 2) - 1],
        // Right-Right: distances 2, 4, 6 → rank 0, 1, 2
        (Sub::Right, Sub::Right) => [4.0_f32, 2.0, 5.5][(distance as usize / 2) - 1],
    }
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
        let dist = connector_distance(a, b);
        let width = width_for_connector(dist, a.sub, b.sub);
        painter.add(egui::Shape::CubicBezier(egui::epaint::CubicBezierShape {
            points: [
                to(ax, ay),
                to(ax + ctrl * nax, ay + ctrl * nay),
                to(bx + ctrl * nbx, by + ctrl * nby),
                to(bx, by),
            ],
            closed: false,
            fill: egui::Color32::TRANSPARENT,
            stroke: egui::Stroke::new(width, connector_color).into(),
        }));
    }

    response
}

#[allow(clippy::too_many_arguments)]
pub fn show(
    ui: &mut egui::Ui,
    game: &mut Option<GameState>,
    rendering_data: &RenderingData,
    interaction: &mut BoardInteraction,
    history: &mut GameHistory,
    current_mission: &mut Option<usize>,
    missions: &[hexagon_types::MissionEntry],
    missions_won: &mut std::collections::HashSet<uuid::Uuid>,
    music: &mut MusicState,
    music_player: Option<&crate::music::MusicPlayer>,
) {
    let mut started = false;

    let btn_font_size = egui::TextStyle::Body.resolve(ui.style()).size * 1.4;
    let can_undo = !history.undo_stack.is_empty();

    if game.is_none() {
        ui.label("No game in progress.");
        return;
    }

    // Game-over section: collect all data from a scoped borrow so the borrow
    // is released before we write back to *game.
    let game_over_data = {
        let g = game.as_ref().unwrap();
        g.result.as_ref().map(|result| {
            let (text, color, is_win) = match result {
                GameResult::Win(winners) => {
                    let text = if matches!(
                        g.options,
                        GameOptions::Highscore(_) | GameOptions::HighscoreV2(_)
                    ) {
                        "Mission solved".to_string()
                    } else {
                        let names: Vec<String> = winners
                            .iter()
                            .map(|p| format!("Player {}", p.0 + 1))
                            .collect();
                        format!("Winner: {}", names.join(", "))
                    };
                    (text, egui::Color32::GOLD, true)
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
            (text, color, opts, saved, is_win)
        })
    }; // immutable borrow of *game released here

    // Win: render action buttons above the top undo button.
    if let Some((_, _, ref opts, _, true)) = game_over_data {
        if let Some(cur) = *current_mission
            && let Some(mission_id) = crate::panels::missions::mission_id(missions, cur)
        {
            missions_won.insert(mission_id);
        }

        let next_mission = current_mission
            .map(|idx| idx + 1)
            .filter(|&next| next < crate::panels::missions::mission_count(missions));

        if let Some(next_idx) = next_mission {
            if ui
                .button(egui::RichText::new("Start next mission").size(btn_font_size))
                .clicked()
            {
                history.undo_stack.clear();
                history.redo_stack.clear();
                crate::panels::missions::start_mission(missions, next_idx, game);
                *current_mission = Some(next_idx);
                started = true;
            }
        } else if ui
            .button(egui::RichText::new("Start new game").size(btn_font_size))
            .clicked()
        {
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
                Err(e) => tracing::info!("New game failed: {e}"),
            }
        }

        let restart_label = if current_mission.is_some() {
            "Restart this mission"
        } else {
            "Restart this game"
        };
        if ui
            .button(egui::RichText::new(restart_label).size(btn_font_size))
            .clicked()
        {
            match opts.clone().start_game() {
                Ok(g) => {
                    history.undo_stack.clear();
                    history.redo_stack.clear();
                    *game = Some(g);
                    started = true;
                }
                Err(e) => tracing::info!("Restart failed: {e}"),
            }
        }

        ui.separator();
    }

    // Top undo button.
    if ui
        .add_enabled(
            can_undo,
            egui::Button::new(egui::RichText::new("↩ Undo").size(btn_font_size)),
        )
        .clicked()
        && let Some(prev) = history.undo_stack.pop()
    {
        if let Some(current) = game.take() {
            history.redo_stack.push(current);
        }
        *game = Some(prev);
    }

    if let Some((text, color, opts, saved, is_win)) = game_over_data {
        ui.label(
            egui::RichText::new("The game is finished")
                .strong()
                .heading(),
        );
        ui.label(egui::RichText::new(text).strong().color(color).heading());
        let next_mission = current_mission
            .map(|idx| idx + 1)
            .filter(|&next| next < crate::panels::missions::mission_count(missions));

        let is_highscore_loss = !is_win
            && matches!(
                &opts,
                GameOptions::Highscore(_) | GameOptions::HighscoreV2(_)
            );
        if is_highscore_loss {
            if let Some(idx) = *current_mission
                && let Some(entry) = missions.get(idx)
            {
                ui.separator();
                ui.label(egui::RichText::new(&entry.name).strong());
                ui.label(&entry.description);
            }

            ui.separator();

            match &opts {
                GameOptions::Highscore(m) => {
                    let wc = &m.winning_condition;
                    let player = saved.players.first();
                    let target_met = wc
                        .target
                        .map(|t| player.is_some_and(|p| p.current_position.0 == t));
                    show_highscore_conditions(
                        ui,
                        wc.min_distance,
                        wc.min_velocity,
                        target_met,
                        player
                            .and_then(|p| saved.statistics.total_path_weight.get(&p.id).copied())
                            .unwrap_or(0),
                        player
                            .and_then(|p| saved.statistics.max_velocity.get(&p.id).copied())
                            .unwrap_or(0),
                    );
                }
                GameOptions::HighscoreV2(m) => {
                    let wc = &m.winning_condition;
                    for player in saved.players.iter() {
                        let id = player.id;
                        let min_dist = wc.min_distance.get(&id).copied();
                        let min_vel = wc.min_velocity.get(&id).copied();
                        let target_met =
                            wc.target.get(&id).map(|&t| player.current_position.0 == t);
                        if min_dist.is_none() && min_vel.is_none() && target_met.is_none() {
                            continue;
                        }
                        let color = rendering_data
                            .player_colors
                            .get(&id)
                            .copied()
                            .unwrap_or(hexagon_engine::Color::Gray);
                        ui.label(egui::RichText::new("Player ⏺").color(color_to_egui(color)));
                        show_highscore_conditions(
                            ui,
                            min_dist,
                            min_vel,
                            target_met,
                            saved
                                .statistics
                                .total_path_weight
                                .get(&id)
                                .copied()
                                .unwrap_or(0),
                            saved.statistics.max_velocity.get(&id).copied().unwrap_or(0),
                        );
                    }
                }
                _ => {}
            }

            ui.separator();

            show_start_button(
                ui,
                game,
                history,
                current_mission,
                missions,
                &mut started,
                btn_font_size,
                next_mission,
                &opts,
            );

            let restart_label = if current_mission.is_some() {
                "Restart this mission"
            } else {
                "Restart this game"
            };
            if ui
                .button(egui::RichText::new(restart_label).size(btn_font_size))
                .clicked()
            {
                match opts.clone().start_game() {
                    Ok(g) => {
                        history.undo_stack.clear();
                        history.redo_stack.clear();
                        *game = Some(g);
                        started = true;
                    }
                    Err(e) => tracing::info!("Restart failed: {e}"),
                }
            }
        }

        if is_win {
            show_start_button(
                ui,
                game,
                history,
                current_mission,
                missions,
                &mut started,
                btn_font_size,
                next_mission,
                &opts,
            );

            let top = ui.cursor().top();
            let max_rect = ui.max_rect();
            let confetti_rect =
                egui::Rect::from_min_max(egui::pos2(max_rect.left(), top), max_rect.max);
            let dt = ui.ctx().input(|i| i.stable_dt).min(0.05);
            interaction.confetti.activate();
            interaction
                .confetti
                .update_and_draw(dt, confetti_rect, ui.painter());
            if interaction.confetti.is_active() {
                ui.ctx().request_repaint();
            }
        }
        return;
    }

    // Normal hand display — borrow game mutably for the rest of the function.
    let game_state = game.as_mut().unwrap();
    let current_id = game_state.current_player;

    let Some(player) = game_state.players.iter().find(|p| p.id == current_id) else {
        return;
    };

    let player_color = rendering_data
        .player_colors
        .get(&current_id)
        .copied()
        .unwrap_or(hexagon_engine::Color::Gray);

    if game_state.players.iter().filter(|x| !x.is_npc).count() > 1 {
        ui.horizontal(|ui| {
            let (rect, _) = ui.allocate_exact_size(egui::vec2(16.0, 16.0), egui::Sense::hover());
            ui.painter()
                .rect_filled(rect, 2.0, color_to_egui(player_color));
            ui.label(format!("Player {}", current_id.0 + 1));
        });
    }

    let mission_entry = current_mission.and_then(|idx| missions.get(idx));

    match &game_state.options {
        GameOptions::Highscore(m) => {
            let wc = &m.winning_condition;
            let min_dist = wc.min_distance;
            let min_vel = wc.min_velocity;
            let current_dist = min_dist.map(|tgt| {
                let cur = game_state
                    .statistics
                    .total_path_weight
                    .get(&current_id)
                    .copied()
                    .unwrap_or(0);
                (cur, tgt)
            });
            let current_vel = min_vel.map(|tgt| {
                let cur = game_state
                    .statistics
                    .max_velocity
                    .get(&current_id)
                    .copied()
                    .unwrap_or(0);
                (cur, tgt)
            });
            let mission_name = mission_entry
                .map(|e| e.name.clone())
                .unwrap_or_else(|| "Mission".to_string());
            let collapse_id =
                ui.make_persistent_id(("mission_stats_collapsible", mission_entry.map(|e| e.id)));
            let state = egui::collapsing_header::CollapsingState::load_with_default_open(
                ui.ctx(),
                collapse_id,
                true,
            );
            let header_label = if state.is_open() {
                mission_name.clone()
            } else {
                let mut parts = Vec::new();
                if let Some((cur, tgt)) = current_dist {
                    parts.push(format!(
                        "↔ {:.1}/{:.1}",
                        cur as f32 / 1000.0,
                        tgt as f32 / 1000.0
                    ));
                }
                if let Some((cur, tgt)) = current_vel {
                    parts.push(format!(
                        "⚡ {:.1}/{:.1}",
                        cur as f32 / 1000.0,
                        tgt as f32 / 1000.0
                    ));
                }
                if parts.is_empty() {
                    mission_name
                } else {
                    parts.join("  ")
                }
            };
            state
                .show_header(ui, |ui| {
                    ui.label(header_label);
                })
                .body(|ui| {
                    if let Some(entry) = mission_entry {
                        ui.label(&entry.description);
                        ui.separator();
                    }
                    show_highscore_progress(ui, current_dist, current_vel, player.target.is_some());
                });
        }
        GameOptions::HighscoreV2(m) => {
            let wc = &m.winning_condition;
            let mission_name = mission_entry
                .map(|e| e.name.clone())
                .unwrap_or_else(|| "Mission".to_string());
            let collapse_id =
                ui.make_persistent_id(("mission_stats_collapsible", mission_entry.map(|e| e.id)));
            let state = egui::collapsing_header::CollapsingState::load_with_default_open(
                ui.ctx(),
                collapse_id,
                true,
            );
            state
                .show_header(ui, |ui| {
                    ui.label(mission_name);
                })
                .body(|ui| {
                    if let Some(entry) = mission_entry {
                        ui.label(&entry.description);
                        ui.separator();
                    }
                    for p in game_state.players.iter() {
                        let id = p.id;
                        let min_dist = wc.min_distance.get(&id).copied();
                        let min_vel = wc.min_velocity.get(&id).copied();
                        let has_target = p.target.is_some();
                        if min_dist.is_none() && min_vel.is_none() && !has_target {
                            continue;
                        }
                        let color = rendering_data
                            .player_colors
                            .get(&id)
                            .copied()
                            .unwrap_or(hexagon_engine::Color::Gray);
                        ui.label(egui::RichText::new("Player ⏺").color(color_to_egui(color)));
                        let current_dist = min_dist.map(|tgt| {
                            let cur = game_state
                                .statistics
                                .total_path_weight
                                .get(&id)
                                .copied()
                                .unwrap_or(0);
                            (cur, tgt)
                        });
                        let current_vel = min_vel.map(|tgt| {
                            let cur = game_state
                                .statistics
                                .max_velocity
                                .get(&id)
                                .copied()
                                .unwrap_or(0);
                            (cur, tgt)
                        });
                        show_highscore_progress(ui, current_dist, current_vel, has_target);
                    }
                });
        }
        _ => {}
    }

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
        let used_h = ui.cursor().top() - ui.clip_rect().top();
        let available_h = (ui.clip_rect().height() - used_h).max(1.0);

        let count = hand_len;
        let sep_h = ui.spacing().item_spacing.y * 2.0 + 6.0;
        let height_per_tile = if count > 0 {
            (available_h - count.saturating_sub(1) as f32 * sep_h) / count as f32
        } else {
            available_h
        };

        // button height from width-constrained r, floored by egui's interact_size minimum,
        // capped at 20% of per-tile height
        let r_from_w = available_w / 2.4;
        let btn_pad = ui.spacing().button_padding.y * 2.0;
        let interact_h = ui.spacing().interact_size.y;
        let row_h = ui
            .ctx()
            .fonts_mut(|fonts| fonts.row_height(&egui::FontId::proportional(r_from_w * 0.4)));
        let btn_h = (row_h + btn_pad).min(height_per_tile * 0.2).max(interact_h);

        // tile takes the remaining height (minus spacing between tile and button row)
        let item_sp = ui.spacing().item_spacing.y;
        let tile_h = (height_per_tile - btn_h - item_sp).max(1.0);
        let r_from_h = tile_h / 2.4;

        r_from_h.min(r_from_w).max(10.0) as f64
    };

    ui.add_enabled_ui(!disabled, |ui| {
        for i in 0..hand_len {
            let connectors = connectors_per_tile.get(i).map(Vec::as_slice).unwrap_or(&[]);
            let is_selected = selected_tile == Some(i);

            if i > 0 {
                ui.separator();
            }

            ui.vertical(|ui| {
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

                let font_size = r as f32 * 0.4;
                let inset = r as f32 * 0.15;
                ui.columns(3, |cols| {
                    cols[0].horizontal(|ui| {
                        ui.add_space(inset);
                        if ui
                            .button(egui::RichText::new("↺").size(font_size))
                            .clicked()
                        {
                            try_start_music(music, music_player);
                            game_state.rotate_tile(i, TileRotationDirection::CounterClockwise);
                            interaction.selected_tile = Some(i);
                            interaction.animation_t = 1.0;
                        }
                    });
                    cols[1].with_layout(egui::Layout::top_down(egui::Align::Center), |ui| {
                        if is_selected
                            && ui
                                .button(egui::RichText::new("➡").size(font_size))
                                .clicked()
                        {
                            try_start_music(music, music_player);
                            history.undo_stack.push(game_state.clone());
                            history.redo_stack.clear();
                            game_state.play_tile(i);
                            interaction.selected_tile = None;
                            interaction.animation_t = 0.0;
                        }
                    });
                    cols[2].with_layout(egui::Layout::right_to_left(egui::Align::TOP), |ui| {
                        ui.add_space(inset);
                        if ui
                            .button(egui::RichText::new("↻").size(font_size))
                            .clicked()
                        {
                            try_start_music(music, music_player);
                            game_state.rotate_tile(i, TileRotationDirection::Clockwise);
                            interaction.selected_tile = Some(i);
                            interaction.animation_t = 1.0;
                        }
                    });
                });
            });
        }
    });
}

#[allow(clippy::too_many_arguments)]
fn show_highscore_conditions(
    ui: &mut egui::Ui,
    min_dist: Option<u32>,
    min_vel: Option<u32>,
    target_met: Option<bool>,
    cur_dist: u32,
    cur_vel: u32,
) {
    if let Some(tgt) = min_dist {
        let met = cur_dist >= tgt;
        let (check, col) = if met {
            ("✔", egui::Color32::GREEN)
        } else {
            ("❌", egui::Color32::RED)
        };
        ui.label(
            egui::RichText::new(format!(
                "{check} Distance: {:.1}/{:.1}",
                cur_dist as f32 / 1000.0,
                tgt as f32 / 1000.0,
            ))
            .color(col),
        );
    }
    if let Some(tgt) = min_vel {
        let met = cur_vel >= tgt;
        let (check, col) = if met {
            ("✔", egui::Color32::GREEN)
        } else {
            ("❌", egui::Color32::RED)
        };
        ui.label(
            egui::RichText::new(format!(
                "{check} Velocity: {:.1}/{:.1}",
                cur_vel as f32 / 1000.0,
                tgt as f32 / 1000.0,
            ))
            .color(col),
        );
    }
    if let Some(met) = target_met {
        let (check, col) = if met {
            ("✔", egui::Color32::GREEN)
        } else {
            ("❌", egui::Color32::RED)
        };
        ui.label(egui::RichText::new(format!("{check} Reach target")).color(col));
    }
}

fn show_highscore_progress(
    ui: &mut egui::Ui,
    current_dist: Option<(u32, u32)>,
    current_vel: Option<(u32, u32)>,
    has_target: bool,
) {
    if let Some((cur, tgt)) = current_dist {
        let met = cur >= tgt;
        let check = if met { "✔ " } else { "" };
        ui.label(format!(
            "{check}Distance: {:.1}/{:.1}",
            cur as f32 / 1000.0,
            tgt as f32 / 1000.0,
        ));
    }
    if let Some((cur, tgt)) = current_vel {
        let met = cur >= tgt;
        let check = if met { "✔ " } else { "" };
        ui.label(format!(
            "{check}Velocity: {:.1}/{:.1}",
            cur as f32 / 1000.0,
            tgt as f32 / 1000.0,
        ));
    }
    if has_target {
        ui.label("Reach target");
    }
}

#[allow(clippy::too_many_arguments)]
fn show_start_button(
    ui: &mut egui::Ui,
    game: &mut Option<GameState>,
    history: &mut GameHistory,
    current_mission: &mut Option<usize>,
    missions: &[hexagon_types::MissionEntry],
    started: &mut bool,
    btn_font_size: f32,
    next_mission: Option<usize>,
    opts: &GameOptions,
) {
    if let Some(next_idx) = next_mission {
        if ui
            .button(egui::RichText::new("Start next mission").size(btn_font_size))
            .clicked()
        {
            history.undo_stack.clear();
            history.redo_stack.clear();
            crate::panels::missions::start_mission(missions, next_idx, game);
            *current_mission = Some(next_idx);
            *started = true;
        }
    } else if ui
        .button(egui::RichText::new("Start new game").size(btn_font_size))
        .clicked()
    {
        let mut new_opts = opts.clone();
        new_opts.randomize_seed();
        match new_opts.start_game() {
            Ok(g) => {
                history.undo_stack.clear();
                history.redo_stack.clear();
                *game = Some(g);
                *current_mission = None;
                *started = true;
            }
            Err(e) => tracing::info!("New game failed: {e}"),
        }
    }
}
