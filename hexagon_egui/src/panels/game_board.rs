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
        ConnectorKind::OnHex(ConnectorOnHex { hexagon, edge_sub: ConnectorEdgeSub { a, b } }) => {
            let (ax, ay) = edge_sub_point(hexagon, a);
            let (bx, by) = edge_sub_point(hexagon, b);
            let ctrl = R * 0.6;
            let (nax, nay) = edge_inward_normal(&a.edge);
            let (nbx, nby) = edge_inward_normal(&b.edge);
            cubic((ax, ay), (ax + ctrl * nax, ay + ctrl * nay), (bx + ctrl * nbx, by + ctrl * nby), (bx, by))
        }
        ConnectorKind::Outside(ConnectorOutside { connector_a, connector_b })
        | ConnectorKind::HexToHex(ConnectorOutside { connector_a, connector_b }) => {
            let (ax, ay) = edge_sub_point(&connector_a.hexagon, &connector_a.edge_sub);
            let (bx, by) = edge_sub_point(&connector_b.hexagon, &connector_b.edge_sub);
            let ctrl = R * 0.6;
            let (nax, nay) = edge_inward_normal(&connector_a.edge_sub.edge);
            let (nbx, nby) = edge_inward_normal(&connector_b.edge_sub.edge);
            cubic((ax, ay), (ax - ctrl * nax, ay - ctrl * nay), (bx - ctrl * nbx, by - ctrl * nby), (bx, by))
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
        (ar + col.r() as u32, ag + col.g() as u32, ab + col.b() as u32)
    });
    egui::Color32::from_rgb((r / n) as u8, (g / n) as u8, (b / n) as u8)
}

fn with_opacity(color: egui::Color32, opacity: f32) -> egui::Color32 {
    egui::Color32::from_rgba_unmultiplied(color.r(), color.g(), color.b(), (255.0 * opacity) as u8)
}

fn draw_arrow(painter: &egui::Painter, tail: egui::Pos2, tip: egui::Pos2, head: f32, stroke: egui::Stroke) {
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
        [tip, egui::pos2(tip.x - ux * head + px * head, tip.y - uy * head + py * head)],
        stroke,
    );
    painter.line_segment(
        [tip, egui::pos2(tip.x - ux * head - px * head, tip.y - uy * head - py * head)],
        stroke,
    );
}

fn draw_cubic(painter: &egui::Painter, p0: egui::Pos2, p1: egui::Pos2, p2: egui::Pos2, p3: egui::Pos2, stroke: egui::Stroke) {
    painter.add(egui::Shape::CubicBezier(egui::epaint::CubicBezierShape {
        points: [p0, p1, p2, p3],
        closed: false,
        fill: egui::Color32::TRANSPARENT,
        stroke: stroke.into(),
    }));
}

fn make_player_data(rendering_data: &RenderingData) -> PlayerData {
    PlayerData {
        colors: rendering_data.player_colors.iter().map(|(k, v)| (*k, *v)).collect(),
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

    let preview_hex = interaction
        .selected_tile
        .is_some()
        .then(|| game.current_player_hexagon())
        .flatten();

    let effective_selected_hexagon = preview_hex.or(interaction.selected_hexagon);
    let render_task = game.render_task(effective_selected_hexagon, interaction.animation_t);

    if render_task.hexagons.is_empty() {
        return;
    }

    let h = h();

    // Bounding box in world coordinates
    let (mut min_x, mut min_y) = (f64::MAX, f64::MAX);
    let (mut max_x, mut max_y) = (f64::MIN, f64::MIN);
    for hex in &render_task.hexagons {
        let (cx, cy) = hex_center(hex);
        min_x = min_x.min(cx - R);
        min_y = min_y.min(cy - h);
        max_x = max_x.max(cx + R);
        max_y = max_y.max(cy + h);
    }
    let world_w = (max_x - min_x) as f32;
    let world_h = (max_y - min_y) as f32;

    let desired_size = egui::vec2(640.0, 480.0);
    let (rect, response) = ui.allocate_exact_size(desired_size, egui::Sense::click());

    let scale = (rect.width() / world_w).min(rect.height() / world_h) * 0.85;
    let offset_x = rect.center().x - (min_x as f32 + world_w / 2.0) * scale;
    let offset_y = rect.center().y - (min_y as f32 + world_h / 2.0) * scale;

    let to_screen = |wx: f64, wy: f64| -> egui::Pos2 {
        egui::pos2(wx as f32 * scale + offset_x, wy as f32 * scale + offset_y)
    };

    let painter = ui.painter_at(rect);
    painter.rect_filled(rect, 0.0, egui::Color32::WHITE);

    // Step 1: hexagons
    for hex in &render_task.hexagons {
        let (cx, cy) = hex_center(hex);
        let is_highlighted = render_task.hexagon_to_highlight.as_ref().is_some_and(|hl| hl == hex);
        let (fill, stroke_color) = if is_highlighted {
            (color_to_egui(player_data.highlighted_hex_fill), color_to_egui(player_data.highlighted_hex_stroke))
        } else {
            (color_to_egui(player_data.hex_fill), color_to_egui(player_data.hex_stroke))
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
        painter.add(egui::Shape::convex_polygon(verts, fill, egui::Stroke::new(2.0 * scale, stroke_color)));
    }

    // Step 2: connectors
    for uc in &render_task.connectors {
        let player_color = |pid: &hexagon_engine::PlayerId| -> Color {
            player_data.colors.get(pid).copied().unwrap_or(player_data.unused_color)
        };

        let base_color = if !uc.used_by.is_empty() {
            let cols: Vec<Color> = uc.used_by.iter().chain(uc.preview_used_by.iter()).map(player_color).collect();
            mix_colors(&cols)
        } else if !uc.is_connected_to_player_start.is_empty() {
            mix_colors(&uc.is_connected_to_player_start.iter().map(player_color).collect::<Vec<_>>())
        } else if !uc.is_connected_to_player_target.is_empty() {
            mix_colors(&uc.is_connected_to_player_target.iter().map(player_color).collect::<Vec<_>>())
        } else if uc.is_connected_to_dead_end {
            color_to_egui(player_data.dead_end_color)
        } else {
            color_to_egui(player_data.closed_loop_color)
        };

        let opacity = if !uc.preview_used_by.is_empty() { 0.5 } else { 1.0 };
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
                    draw_arrow(&painter, to_screen(px - nx * R * 0.5, py - ny * R * 0.5), to_screen(px, py), head, stroke);
                } else if !uc.is_connected_to_player_target.is_empty() {
                    draw_arrow(&painter, to_screen(px, py), to_screen(px - nx * R * 0.5, py - ny * R * 0.5), head, stroke);
                } else {
                    let (tx, ty) = (-ny, nx);
                    let f = R * 0.2 / 2.0_f64.sqrt();
                    painter.line_segment(
                        [to_screen(px - (tx + nx) * f, py - (ty + ny) * f), to_screen(px + (tx + nx) * f, py + (ty + ny) * f)],
                        stroke,
                    );
                    painter.line_segment(
                        [to_screen(px - (tx - nx) * f, py - (ty - ny) * f), to_screen(px + (tx - nx) * f, py + (ty - ny) * f)],
                        stroke,
                    );
                }
            }
            ConnectorKind::OnHex(ConnectorOnHex { hexagon, edge_sub: ConnectorEdgeSub { a, b } }) => {
                let (ax, ay) = edge_sub_point(hexagon, a);
                let (bx, by) = edge_sub_point(hexagon, b);
                let ctrl = R * 0.6;
                let (nax, nay) = edge_inward_normal(&a.edge);
                let (nbx, nby) = edge_inward_normal(&b.edge);
                draw_cubic(&painter,
                    to_screen(ax, ay),
                    to_screen(ax + ctrl * nax, ay + ctrl * nay),
                    to_screen(bx + ctrl * nbx, by + ctrl * nby),
                    to_screen(bx, by),
                    stroke,
                );
            }
            ConnectorKind::Outside(ConnectorOutside { connector_a, connector_b })
            | ConnectorKind::HexToHex(ConnectorOutside { connector_a, connector_b }) => {
                let (ax, ay) = edge_sub_point(&connector_a.hexagon, &connector_a.edge_sub);
                let (bx, by) = edge_sub_point(&connector_b.hexagon, &connector_b.edge_sub);
                let ctrl = R * 0.6;
                let (nax, nay) = edge_inward_normal(&connector_a.edge_sub.edge);
                let (nbx, nby) = edge_inward_normal(&connector_b.edge_sub.edge);
                draw_cubic(&painter,
                    to_screen(ax, ay),
                    to_screen(ax - ctrl * nax, ay - ctrl * nay),
                    to_screen(bx - ctrl * nbx, by - ctrl * nby),
                    to_screen(bx, by),
                    stroke,
                );
            }
        }
    }

    // Step 2b: ghost overlay for selected tile
    if let (Some(hex), Some(tile_index)) = (preview_hex, interaction.selected_tile) {
        if let Some(player) = game.players.iter().find(|p| p.id == game.current_player) {
            if let Some(tile) = player.hand.get(tile_index) {
                let ctrl = R * 0.6;
                let ghost_color = with_opacity(
                    color_to_egui(player_data.colors.get(&game.current_player).copied().unwrap_or(player_data.unused_color)),
                    0.4,
                );
                let ghost_stroke = egui::Stroke::new(3.0 * scale, ghost_color);
                for conn in &tile.inner_connectors {
                    let fake_hex = HexagonPosition { x: hex.x, y: hex.y };
                    let a = EdgeSub { edge: conn.a.edge, sub: conn.a.sub };
                    let b = EdgeSub { edge: conn.b.edge, sub: conn.b.sub };
                    let (ax, ay) = edge_sub_point(&fake_hex, &a);
                    let (bx, by) = edge_sub_point(&fake_hex, &b);
                    let (nax, nay) = edge_inward_normal(&a.edge);
                    let (nbx, nby) = edge_inward_normal(&b.edge);
                    draw_cubic(&painter,
                        to_screen(ax, ay),
                        to_screen(ax + ctrl * nax, ay + ctrl * nay),
                        to_screen(bx + ctrl * nbx, by + ctrl * nby),
                        to_screen(bx, by),
                        ghost_stroke,
                    );
                }
            }
        }
    }

    // Step 3: player positions
    for cpp in &render_task.current_player_position {
        let (px, py) = point_on_connector(&cpp.connector, cpp.step);
        let color = color_to_egui(player_data.colors.get(&cpp.player_id).copied().unwrap_or(player_data.unused_color));
        let pos = to_screen(px, py);

        if !cpp.is_active {
            if let ConnectorKind::DeadEnd(ConnectorDeadEnd { position }) = &cpp.connector {
                let (nx, ny) = edge_inward_normal(&position.edge_sub.edge);
                let tip = to_screen(px - nx * R * 0.5, py - ny * R * 0.5);
                draw_arrow(&painter, pos, tip, R as f32 * 0.2 * scale, egui::Stroke::new(3.0 * scale, color));
                continue;
            }
        }
        painter.circle_filled(pos, 7.0 * scale, color);
        painter.circle_stroke(pos, 7.0 * scale, egui::Stroke::new(1.5 * scale, egui::Color32::WHITE));
    }

    // Click → hexagon selection
    if response.clicked() {
        if let Some(screen_pos) = response.interact_pointer_pos() {
            let wx = (screen_pos.x - offset_x) as f64 / scale as f64;
            let wy = (screen_pos.y - offset_y) as f64 / scale as f64;

            let hit = render_task.hexagons.iter().min_by_key(|hex| {
                let (cx, cy) = hex_center(hex);
                let dx = cx - wx;
                let dy = cy - wy;
                ((dx * dx + dy * dy) * 1000.0) as i64
            });

            if let Some(hex) = hit {
                let (cx, cy) = hex_center(hex);
                let dx = cx - wx;
                let dy = cy - wy;
                if (dx * dx + dy * dy).sqrt() < R {
                    interaction.selected_hexagon = if interaction.selected_hexagon == Some(*hex) {
                        None
                    } else {
                        Some(*hex)
                    };
                }
            }
        }
    }
}
