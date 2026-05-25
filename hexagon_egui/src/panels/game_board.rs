use hexagon_engine::{
    Color, ConnectorDeadEnd, ConnectorEdgeSub, ConnectorKind, ConnectorOnHex, ConnectorOutside,
    Edge, EdgeSub, GameState, HexagonPosition, PlayerData, Sub,
};

use crate::app::{BoardInteraction, RenderingData};
use crate::panels::color_to_egui;

const R: f64 = 50.0;

fn h() -> f64 {
    R * 3.0_f64.sqrt() / 2.0
}

fn hex_center(hex: &HexagonPosition) -> (f64, f64) {
    let h = h();
    let cx = hex.x as f64 * 1.5 * R;
    let cy = hex.y as f64 * 2.0 * h + hex.x as f64 * h;
    (cx, cy)
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

fn edge_sub_point(hex: &HexagonPosition, edge_sub: &EdgeSub) -> (f64, f64) {
    let h = h();
    let (cx, cy) = hex_center(hex);
    let v = [
        (cx + R, cy),
        (cx + R / 2.0, cy + h),
        (cx - R / 2.0, cy + h),
        (cx - R, cy),
        (cx - R / 2.0, cy - h),
        (cx + R / 2.0, cy - h),
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

fn point_on_connector(connector: &ConnectorKind, step: f32) -> (f64, f64) {
    let t = step.clamp(0.0, 1.0) as f64;
    let cubic = |p0: (f64, f64), p1: (f64, f64), p2: (f64, f64), p3: (f64, f64)| {
        let u = 1.0 - t;
        (
            u * u * u * p0.0 + 3.0 * u * u * t * p1.0 + 3.0 * u * t * t * p2.0 + t * t * t * p3.0,
            u * u * u * p0.1 + 3.0 * u * u * t * p1.1 + 3.0 * u * t * t * p2.1 + t * t * t * p3.1,
        )
    };
    match connector {
        ConnectorKind::OnHex(ConnectorOnHex {
            hexagon,
            edge_sub: ConnectorEdgeSub { a, b },
        }) => {
            let (ax, ay) = edge_sub_point(hexagon, a);
            let (bx, by) = edge_sub_point(hexagon, b);
            let ctrl = R * 0.6;
            let (nax, nay) = edge_inward_normal(&a.edge);
            let (nbx, nby) = edge_inward_normal(&b.edge);
            cubic(
                (ax, ay),
                (ax + ctrl * nax, ay + ctrl * nay),
                (bx + ctrl * nbx, by + ctrl * nby),
                (bx, by),
            )
        }
        ConnectorKind::Outside(ConnectorOutside {
            connector_a,
            connector_b,
        })
        | ConnectorKind::HexToHex(ConnectorOutside {
            connector_a,
            connector_b,
        }) => {
            let (ax, ay) = edge_sub_point(&connector_a.hexagon, &connector_a.edge_sub);
            let (bx, by) = edge_sub_point(&connector_b.hexagon, &connector_b.edge_sub);
            let ctrl = R * 0.6;
            let (nax, nay) = edge_inward_normal(&connector_a.edge_sub.edge);
            let (nbx, nby) = edge_inward_normal(&connector_b.edge_sub.edge);
            cubic(
                (ax, ay),
                (ax - ctrl * nax, ay - ctrl * nay),
                (bx - ctrl * nbx, by - ctrl * nby),
                (bx, by),
            )
        }
        ConnectorKind::DeadEnd(ConnectorDeadEnd { position }) => {
            edge_sub_point(&position.hexagon, &position.edge_sub)
        }
    }
}

fn mix_colors(colors: &[Color]) -> egui::Color32 {
    let n = colors.len() as u32;
    let (r, g, b) = colors.iter().fold((0u32, 0u32, 0u32), |(ar, ag, ab), &c| {
        let col = color_to_egui(c);
        (
            ar + col.r() as u32,
            ag + col.g() as u32,
            ab + col.b() as u32,
        )
    });
    egui::Color32::from_rgb((r / n) as u8, (g / n) as u8, (b / n) as u8)
}

fn with_opacity(color: egui::Color32, opacity: f32) -> egui::Color32 {
    egui::Color32::from_rgba_unmultiplied(color.r(), color.g(), color.b(), (255.0 * opacity) as u8)
}

fn draw_arrow(
    painter: &egui::Painter,
    tail: egui::Pos2,
    tip: egui::Pos2,
    head: f32,
    stroke: egui::Stroke,
) {
    let dx = tip.x - tail.x;
    let dy = tip.y - tail.y;
    let len = (dx * dx + dy * dy).sqrt();
    if len < 0.001 {
        return;
    }
    let (ux, uy) = (dx / len, dy / len);
    let (px, py) = (-uy, ux);
    painter.line_segment([tail, tip], stroke);
    painter.line_segment(
        [
            tip,
            egui::pos2(tip.x - ux * head + px * head, tip.y - uy * head + py * head),
        ],
        stroke,
    );
    painter.line_segment(
        [
            tip,
            egui::pos2(tip.x - ux * head - px * head, tip.y - uy * head - py * head),
        ],
        stroke,
    );
}

fn draw_cubic(
    painter: &egui::Painter,
    p0: egui::Pos2,
    p1: egui::Pos2,
    p2: egui::Pos2,
    p3: egui::Pos2,
    stroke: egui::Stroke,
) {
    painter.add(egui::Shape::CubicBezier(egui::epaint::CubicBezierShape {
        points: [p0, p1, p2, p3],
        closed: false,
        fill: egui::Color32::TRANSPARENT,
        stroke: stroke.into(),
    }));
}

fn make_player_data(rendering_data: &RenderingData) -> PlayerData {
    PlayerData {
        colors: rendering_data
            .player_colors
            .iter()
            .map(|(k, v)| (*k, *v))
            .collect(),
        dead_end_color: rendering_data.dead_end_color,
        closed_loop_color: rendering_data.closed_loop_color,
        unused_color: rendering_data.unused_color,
        hex_fill: rendering_data.hex_fill,
        hex_stroke: rendering_data.hex_stroke,
        highlighted_hex_fill: rendering_data.highlighted_hex_fill,
        highlighted_hex_stroke: rendering_data.highlighted_hex_stroke,
    }
}

pub fn show(
    ui: &mut egui::Ui,
    game: &GameState,
    rendering_data: &RenderingData,
    interaction: &mut BoardInteraction,
) {
    let player_data = make_player_data(rendering_data);

    let selected_hexagon = interaction
        .selected_tile
        .is_some()
        .then(|| game.current_player_hexagon())
        .flatten();

    let game = {
        if selected_hexagon.is_some() {
            let mut game = game.clone();
            game.play_tile(interaction.selected_tile.unwrap());
            game
        } else {
            game.clone()
        }
    };
    let game = &game;

    let render_task = game.render_task(selected_hexagon, interaction.animation_t);

    if render_task.hexagons.is_empty() {
        return;
    }

    let h = h();

    // Bounding box in world coordinates, padded so all edges are >= 1.5 R from the rect border
    let (mut min_x, mut min_y) = (f64::MAX, f64::MAX);
    let (mut max_x, mut max_y) = (f64::MIN, f64::MIN);
    for hex in &render_task.hexagons {
        let (cx, cy) = hex_center(hex);
        min_x = min_x.min(cx - R);
        min_y = min_y.min(cy - h);
        max_x = max_x.max(cx + R);
        max_y = max_y.max(cy + h);
    }
    let pad = R * 0.6;
    min_x -= pad;
    min_y -= pad;
    max_x += pad;
    max_y += pad;
    let world_w = (max_x - min_x) as f32;
    let world_h = (max_y - min_y) as f32;

    let (rect, _response) = ui.allocate_exact_size(ui.available_size(), egui::Sense::click());

    let scale = (rect.width() / world_w).min(rect.height() / world_h);
    let offset_x = rect.center().x - (min_x as f32 + world_w / 2.0) * scale;
    let offset_y = rect.center().y - (min_y as f32 + world_h / 2.0) * scale;

    let to_screen = |wx: f64, wy: f64| -> egui::Pos2 {
        egui::pos2(wx as f32 * scale + offset_x, wy as f32 * scale + offset_y)
    };

    let painter = ui.painter_at(rect);

    // Step 1: hexagons
    for hex in &render_task.hexagons {
        let (cx, cy) = hex_center(hex);
        let is_highlighted = render_task
            .hexagon_to_highlight
            .as_ref()
            .is_some_and(|hl| hl == hex);
        let (fill, stroke_color) = if is_highlighted {
            (
                color_to_egui(player_data.highlighted_hex_fill),
                color_to_egui(player_data.highlighted_hex_stroke),
            )
        } else {
            (
                color_to_egui(player_data.hex_fill),
                color_to_egui(player_data.hex_stroke),
            )
        };
        let verts: Vec<egui::Pos2> = [
            (cx + R, cy),
            (cx + R / 2.0, cy + h),
            (cx - R / 2.0, cy + h),
            (cx - R, cy),
            (cx - R / 2.0, cy - h),
            (cx + R / 2.0, cy - h),
        ]
        .iter()
        .map(|&(x, y)| to_screen(x, y))
        .collect();
        painter.add(egui::Shape::convex_polygon(
            verts,
            fill,
            egui::Stroke::new(2.0 * scale, stroke_color),
        ));
    }

    // Redraw highlighted hex border on top so neighbors don't paint over its edges
    if let Some(hl_hex) = &render_task.hexagon_to_highlight {
        let (cx, cy) = hex_center(hl_hex);
        let verts: Vec<egui::Pos2> = [
            (cx + R, cy),
            (cx + R / 2.0, cy + h),
            (cx - R / 2.0, cy + h),
            (cx - R, cy),
            (cx - R / 2.0, cy - h),
            (cx + R / 2.0, cy - h),
        ]
        .iter()
        .map(|&(x, y)| to_screen(x, y))
        .collect();
        painter.add(egui::Shape::convex_polygon(
            verts,
            egui::Color32::TRANSPARENT,
            egui::Stroke::new(
                2.0 * scale,
                color_to_egui(player_data.highlighted_hex_stroke),
            ),
        ));
    }

    // Step 2: connectors
    for uc in &render_task.connectors {
        let player_color = |pid: &hexagon_types::player::PlayerId| -> Color {
            player_data
                .colors
                .get(pid)
                .copied()
                .unwrap_or(player_data.unused_color)
        };

        let base_color = if !uc.used_by.is_empty() {
            let cols: Vec<Color> = uc
                .used_by
                .iter()
                .chain(uc.preview_used_by.iter())
                .map(player_color)
                .collect();
            mix_colors(&cols)
        } else if !uc.is_connected_to_player_start.is_empty() {
            mix_colors(
                &uc.is_connected_to_player_start
                    .iter()
                    .map(player_color)
                    .collect::<Vec<_>>(),
            )
        } else if !uc.is_connected_to_player_target.is_empty() {
            mix_colors(
                &uc.is_connected_to_player_target
                    .iter()
                    .map(player_color)
                    .collect::<Vec<_>>(),
            )
        } else if uc.is_connected_to_dead_end {
            color_to_egui(player_data.dead_end_color)
        } else {
            color_to_egui(player_data.closed_loop_color)
        };

        let opacity = if !uc.preview_used_by.is_empty() {
            0.5
        } else {
            1.0
        };
        let color = with_opacity(base_color, opacity);

        let sw = if !uc.preview_used_by.is_empty() {
            3.0
        } else if !uc.is_connected_to_player_start.is_empty() {
            5.0
        } else if !uc.is_connected_to_player_target.is_empty() {
            1.5
        } else {
            3.0
        } * scale;

        let stroke = egui::Stroke::new(sw, color);

        match &uc.connector {
            ConnectorKind::DeadEnd(ConnectorDeadEnd { position }) => {
                let (px, py) = edge_sub_point(&position.hexagon, &position.edge_sub);
                let (nx, ny) = edge_inward_normal(&position.edge_sub.edge);
                let head = R as f32 * 0.2 * scale;

                if !uc.is_player_start.is_empty() {
                    draw_arrow(
                        &painter,
                        to_screen(px - nx * R * 0.5, py - ny * R * 0.5),
                        to_screen(px, py),
                        head,
                        stroke,
                    );
                } else if !uc.is_player_target.is_empty() {
                    draw_arrow(
                        &painter,
                        to_screen(px, py),
                        to_screen(px - nx * R * 0.5, py - ny * R * 0.5),
                        head,
                        stroke,
                    );
                } else {
                    let (tx, ty) = (-ny, nx);
                    let f = R * 0.2 / 2.0_f64.sqrt();
                    painter.line_segment(
                        [
                            to_screen(px - (tx + nx) * f, py - (ty + ny) * f),
                            to_screen(px + (tx + nx) * f, py + (ty + ny) * f),
                        ],
                        stroke,
                    );
                    painter.line_segment(
                        [
                            to_screen(px - (tx - nx) * f, py - (ty - ny) * f),
                            to_screen(px + (tx - nx) * f, py + (ty - ny) * f),
                        ],
                        stroke,
                    );
                }
            }
            ConnectorKind::OnHex(ConnectorOnHex {
                hexagon,
                edge_sub: ConnectorEdgeSub { a, b },
            }) => {
                let (ax, ay) = edge_sub_point(hexagon, a);
                let (bx, by) = edge_sub_point(hexagon, b);
                let ctrl = R * 0.6;
                let (nax, nay) = edge_inward_normal(&a.edge);
                let (nbx, nby) = edge_inward_normal(&b.edge);
                draw_cubic(
                    &painter,
                    to_screen(ax, ay),
                    to_screen(ax + ctrl * nax, ay + ctrl * nay),
                    to_screen(bx + ctrl * nbx, by + ctrl * nby),
                    to_screen(bx, by),
                    stroke,
                );
            }
            ConnectorKind::Outside(ConnectorOutside {
                connector_a,
                connector_b,
            })
            | ConnectorKind::HexToHex(ConnectorOutside {
                connector_a,
                connector_b,
            }) => {
                let (ax, ay) = edge_sub_point(&connector_a.hexagon, &connector_a.edge_sub);
                let (bx, by) = edge_sub_point(&connector_b.hexagon, &connector_b.edge_sub);
                let ctrl = R * 0.6;
                let (nax, nay) = edge_inward_normal(&connector_a.edge_sub.edge);
                let (nbx, nby) = edge_inward_normal(&connector_b.edge_sub.edge);
                draw_cubic(
                    &painter,
                    to_screen(ax, ay),
                    to_screen(ax - ctrl * nax, ay - ctrl * nay),
                    to_screen(bx - ctrl * nbx, by - ctrl * nby),
                    to_screen(bx, by),
                    stroke,
                );
            }
        }
    }

    // Step 3: player positions
    //
    // Crashed players (hit_fraction.is_some()) share the same screen position.
    // Build a map from quantized position → (pos, colors) for those players.
    // Groups with ≥ 2 members get a single cross in the mixed color; their
    // individual circles are suppressed.
    let player_color = |cpp: &hexagon_engine::CurrentPlayerPosition| -> egui::Color32 {
        color_to_egui(
            player_data
                .colors
                .get(&cpp.player_id)
                .copied()
                .unwrap_or(player_data.unused_color),
        )
    };

    let player_engine_color = |cpp: &hexagon_engine::CurrentPlayerPosition| -> Color {
        player_data
            .colors
            .get(&cpp.player_id)
            .copied()
            .unwrap_or(player_data.unused_color)
    };

    // key → (representative screen pos, per-player colors)
    let mut crash_groups: std::collections::HashMap<(i32, i32), (egui::Pos2, Vec<Color>)> =
        std::collections::HashMap::new();
    for cpp in &render_task.current_player_position {
        if cpp.hit_fraction.is_none() {
            continue;
        }
        let (px, py) = point_on_connector(&cpp.connector, cpp.step);
        let pos = to_screen(px, py);
        let key = (pos.x.round() as i32, pos.y.round() as i32);
        let entry = crash_groups.entry(key).or_insert_with(|| (pos, Vec::new()));
        entry.1.push(player_engine_color(cpp));
    }

    // IDs whose crash group has ≥ 2 players → suppress individual circle
    let mut in_crash_group: std::collections::HashSet<hexagon_types::player::PlayerId> =
        std::collections::HashSet::new();
    for cpp in &render_task.current_player_position {
        if cpp.hit_fraction.is_none() {
            continue;
        }
        let (px, py) = point_on_connector(&cpp.connector, cpp.step);
        let pos = to_screen(px, py);
        let key = (pos.x.round() as i32, pos.y.round() as i32);
        if crash_groups.get(&key).is_some_and(|(_, c)| c.len() >= 2) {
            in_crash_group.insert(cpp.player_id);
        }
    }

    for cpp in &render_task.current_player_position {
        if in_crash_group.contains(&cpp.player_id) {
            continue;
        }
        let (px, py) = point_on_connector(&cpp.connector, cpp.step);
        let color = player_color(cpp);
        let pos = to_screen(px, py);

        if !cpp.is_active
            && !cpp.is_at_start
            && let ConnectorKind::DeadEnd(ConnectorDeadEnd { position }) = &cpp.connector
        {
            let (nx, ny) = edge_inward_normal(&position.edge_sub.edge);
            let tip = to_screen(px - nx * R * 0.5, py - ny * R * 0.5);
            draw_arrow(
                &painter,
                pos,
                tip,
                R as f32 * 0.2 * scale,
                egui::Stroke::new(3.0 * scale, color),
            );
            continue;
        }
        painter.circle_filled(pos, 7.0 * scale, color);
        painter.circle_stroke(
            pos,
            7.0 * scale,
            egui::Stroke::new(1.5 * scale, egui::Color32::WHITE),
        );
    }

    // Draw one X per crash group (≥ 2 players) in the mixed-players color.
    for (_, (pos, colors)) in &crash_groups {
        if colors.len() < 2 {
            continue;
        }
        let mixed = mix_colors(colors);
        let arm = 8.0 * scale;
        let stroke = egui::Stroke::new(3.0 * scale, mixed);
        painter.line_segment(
            [*pos + egui::vec2(-arm, -arm), *pos + egui::vec2(arm, arm)],
            stroke,
        );
        painter.line_segment(
            [*pos + egui::vec2(arm, -arm), *pos + egui::vec2(-arm, arm)],
            stroke,
        );
    }
}
