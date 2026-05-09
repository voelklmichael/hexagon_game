use std::collections::HashMap;

use svg::node::element::path::Data;
use svg::node::element::{Circle, Path, Polygon};

use crate::*;
pub struct RenderTask {
    pub hexagons: Vec<HexagonPosition>,
    pub hexagon_to_highlight: Option<HexagonPosition>,
    pub connectors: Vec<UsedConnector>,
    pub current_player_position: Vec<CurrentPlayerPosition>,
}

impl GameState {
    pub fn render_task(
        &self,
        selected_hexagon: Option<HexagonPosition>,
        animation: f32,
    ) -> RenderTask {
        let Self {
            board,
            players,
            current_player,
            rng: _,
            options,
            statistics: _,
            result: _,
        } = self;

        RenderTask {
            hexagons: board.hexagons.clone(),
            connectors: compute_used_connectors(board, players, selected_hexagon.is_some()),
            hexagon_to_highlight: selected_hexagon,
            current_player_position: players
                .iter()
                .map(|p| compute_player_position(animation, board, p))
                .collect(),
        }
    }
}

fn compute_used_connectors(
    board: &Board,
    players: &[Player],
    is_preview: bool,
) -> Vec<UsedConnector> {
    // Fast connector lookup by id
    let connector_map: HashMap<ConnectorId, &Connector> =
        board.connectors.iter().map(|c| (c.id, c)).collect();
    // Positions a connector exposes (used to link adjacent connectors)
    let positions_of = |c: &Connector| -> Vec<ConnectorPosition> {
        match &c.kind {
            ConnectorKind::DeadEnd(d) => vec![d.position.clone()],
            ConnectorKind::OnHex(d) => vec![
                ConnectorPosition {
                    hexagon: d.hexagon,
                    edge_sub: d.edge_sub.a.clone(),
                },
                ConnectorPosition {
                    hexagon: d.hexagon,
                    edge_sub: d.edge_sub.b.clone(),
                },
            ],
            ConnectorKind::Outside(d) | ConnectorKind::HexToHex(d) => {
                vec![d.connector_a.clone(), d.connector_b.clone()]
            }
        }
    };
    // position → connector ids that touch it
    let mut pos_to_connectors: HashMap<ConnectorPosition, Vec<ConnectorId>> = HashMap::new();
    for c in &board.connectors {
        for pos in positions_of(c) {
            pos_to_connectors.entry(pos).or_default().push(c.id);
        }
    }
    // BFS: build connected components
    let mut component_of: HashMap<ConnectorId, usize> = HashMap::new();
    let mut components: Vec<Vec<ConnectorId>> = Vec::new();
    for c in &board.connectors {
        if component_of.contains_key(&c.id) {
            continue;
        }
        let comp_idx = components.len();
        components.push(Vec::new());
        let mut stack = vec![c.id];
        while let Some(id) = stack.pop() {
            if component_of.contains_key(&id) {
                continue;
            }
            component_of.insert(id, comp_idx);
            components[comp_idx].push(id);
            for pos in positions_of(connector_map[&id]) {
                for &nid in pos_to_connectors.get(&pos).into_iter().flatten() {
                    if !component_of.contains_key(&nid) {
                        stack.push(nid);
                    }
                }
            }
        }
    }
    // Per-component properties
    let comp_has_dead_end: Vec<bool> = components
        .iter()
        .map(|comp| {
            comp.iter()
                .any(|id| matches!(connector_map[id].kind, ConnectorKind::DeadEnd(_)))
        })
        .collect();
    let comp_player_start: Vec<Vec<PlayerId>> = components
        .iter()
        .map(|comp| {
            players
                .iter()
                .filter_map(|p| {
                    let start = p.history.first()?.connectors.first()?;
                    comp.contains(&start.id).then_some(p.id)
                })
                .collect()
        })
        .collect();
    let comp_player_target: Vec<Vec<PlayerId>> = components
        .iter()
        .map(|comp| {
            players
                .iter()
                .filter_map(|p| comp.contains(&p.target?).then_some(p.id))
                .collect()
        })
        .collect();
    let mut start_connector_map: HashMap<ConnectorId, Vec<PlayerId>> = HashMap::new();
    for p in players.iter() {
        if let Some(start) = p.history.first().and_then(|t| t.connectors.first()) {
            start_connector_map.entry(start.id).or_default().push(p.id);
        }
    }
    let mut used_by_map: HashMap<ConnectorId, Vec<PlayerId>> = HashMap::new();
    let mut preview_used_by_map: HashMap<ConnectorId, Vec<PlayerId>> = HashMap::new();
    for player in players.iter() {
        let (committed, preview) = if is_preview {
            let split = player.history.len().saturating_sub(1);
            (&player.history[..split], player.history.last())
        } else {
            (&player.history[..], None)
        };
        for turn in committed {
            for hc in &turn.connectors {
                used_by_map.entry(hc.id).or_default().push(player.id);
            }
        }
        if let Some(turn) = preview {
            for hc in &turn.connectors {
                preview_used_by_map
                    .entry(hc.id)
                    .or_default()
                    .push(player.id);
            }
        }
    }
    board
        .connectors
        .iter()
        .map(|c| {
            let ci = component_of[&c.id];
            UsedConnector {
                connector: c.kind.clone(),
                used_by: used_by_map.get(&c.id).cloned().unwrap_or_default(),
                preview_used_by: preview_used_by_map.get(&c.id).cloned().unwrap_or_default(),
                is_connected_to_dead_end: comp_has_dead_end[ci],
                is_connected_to_player_start: comp_player_start[ci].clone(),
                is_connected_to_player_target: comp_player_target[ci].clone(),
                is_player_start: start_connector_map.get(&c.id).cloned().unwrap_or_default(),
            }
        })
        .collect()
}

fn compute_player_position(animation: f32, board: &Board, p: &Player) -> CurrentPlayerPosition {
    let last = p.history.last().expect(
        "History is empty. \
                    This should never happen, \
                    because the startpoint is added to the history",
    );
    let lookup = |id: &ConnectorId| {
        board
            .connectors
            .iter()
            .find(|c| &c.id == id)
            .expect("connector id from history not found in board")
            .kind
            .clone()
    };
    let (connector, step) = {
        if last.connectors.is_empty() {
            if let Some(last) = p.history.iter().rev().find(|c| !c.connectors.is_empty()) {
                let hc = last.connectors.last().unwrap();
                let step = if hc.end == ConnectorEnd::StartedAtA {
                    1.0
                } else {
                    0.0
                };
                (lookup(&hc.id), step)
            } else {
                panic!(
                    "History consists only of empty entries. \
                                This should never happen, \
                                because the startpoint is added to the history",
                )
            }
        } else {
            // TODO: this needs to be improved
            let total_weight: u32 = last.connectors.iter().map(|hc| hc.weight).sum();
            let mut remaining = (animation * total_weight as f32).clamp(0.0, total_weight as f32);
            let mut chosen = last.connectors.last().unwrap();
            for hc in &last.connectors {
                if remaining <= hc.weight as f32 {
                    chosen = hc;
                    break;
                }
                remaining -= hc.weight as f32;
            }
            let progress = (remaining / chosen.weight as f32).clamp(0.0, 1.0);
            let step = if chosen.end == ConnectorEnd::StartedAtA {
                progress
            } else {
                1.0 - progress
            };
            (lookup(&chosen.id), step)
        }
    };
    CurrentPlayerPosition {
        player_id: p.id,
        connector,
        step,
        is_active: p.is_active,
    }
}

pub struct UsedConnector {
    pub connector: ConnectorKind,
    pub used_by: Vec<PlayerId>,
    pub preview_used_by: Vec<PlayerId>,
    pub is_connected_to_dead_end: bool,
    pub is_connected_to_player_start: Vec<PlayerId>,
    pub is_connected_to_player_target: Vec<PlayerId>,
    pub is_player_start: Vec<PlayerId>,
}

pub struct CurrentPlayerPosition {
    pub player_id: PlayerId,
    pub connector: ConnectorKind,
    // this is a number between 0 and 1
    // it is used for animations
    pub step: f32,
    pub is_active: bool,
}

#[derive(
    Clone, Copy, Debug, PartialEq, serde::Serialize, serde::Deserialize, strum::VariantArray,
)]
pub enum Color {
    White,
    Gray,
    Golden,
    Beige,
    DarkGray,
    Moccasin,
    DarkOrange,
    Green,
    Blue,
    Red,
    Purple,
    Cyan,
    Pink,
    Teal,
    Black,
}

impl Color {
    pub fn to_svg_string(self) -> &'static str {
        match self {
            Color::White => "#FFFFFF",
            Color::Gray => "#808080",
            Color::Golden => "#FFD700",
            Color::Beige => "#F5F5DC",
            Color::DarkGray => "#555555",
            Color::Moccasin => "#FFE4B5",
            Color::DarkOrange => "#FF8C00",
            Color::Green => "#228B22",
            Color::Blue => "#1E90FF",
            Color::Red => "#DC143C",
            Color::Purple => "#800080",
            Color::Cyan => "#00CED1",
            Color::Pink => "#FF69B4",
            Color::Teal => "#008080",
            Color::Black => "#000000",
        }
    }

    fn to_rgb(self) -> (u8, u8, u8) {
        match self {
            Color::White => (0xFF, 0xFF, 0xFF),
            Color::Gray => (0x80, 0x80, 0x80),
            Color::Golden => (0xFF, 0xD7, 0x00),
            Color::Beige => (0xF5, 0xF5, 0xDC),
            Color::DarkGray => (0x55, 0x55, 0x55),
            Color::Moccasin => (0xFF, 0xE4, 0xB5),
            Color::DarkOrange => (0xFF, 0x8C, 0x00),
            Color::Green => (0x22, 0x8B, 0x22),
            Color::Blue => (0x1E, 0x90, 0xFF),
            Color::Red => (0xDC, 0x14, 0x3C),
            Color::Purple => (0x80, 0x00, 0x80),
            Color::Cyan => (0x00, 0xCE, 0xD1),
            Color::Pink => (0xFF, 0x69, 0xB4),
            Color::Teal => (0x00, 0x80, 0x80),
            Color::Black => (0x00, 0x00, 0x00),
        }
    }
}

fn mix_colors(colors: &[Color]) -> String {
    let n = colors.len() as u32;
    let (r, g, b) = colors.iter().fold((0u32, 0u32, 0u32), |(ar, ag, ab), &c| {
        let (cr, cg, cb) = c.to_rgb();
        (ar + cr as u32, ag + cg as u32, ab + cb as u32)
    });
    format!("#{:02X}{:02X}{:02X}", r / n, g / n, b / n)
}

pub struct PlayerData {
    pub colors: HashMap<PlayerId, Color>,
    pub dead_end_color: Color,
    pub closed_loop_color: Color,
    pub unused_color: Color,
    pub hex_fill: Color,
    pub hex_stroke: Color,
    pub highlighted_hex_fill: Color,
    pub highlighted_hex_stroke: Color,
}

fn hex_center(hex: &HexagonPosition, r: f64, h: f64) -> (f64, f64) {
    let q = hex.x as f64;
    let rv = hex.y as f64;
    // axial → screen: q-axis is horizontal, r-axis is tilted 60° (contributes q*h to cy)
    let cx = q * 1.5 * r;
    let cy = rv * 2.0 * h + q * h;
    (cx, cy)
}

// Unit vector pointing from the edge midpoint toward the hex center
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

fn edge_sub_point(hex: &HexagonPosition, edge_sub: &EdgeSub, r: f64, h: f64) -> (f64, f64) {
    let (cx, cy) = hex_center(hex, r, h);
    // Flat-top hex vertices: Right, BottomRight, BottomLeft, Left, TopLeft, TopRight
    let v = [
        (cx + r, cy),
        (cx + r / 2.0, cy + h),
        (cx - r / 2.0, cy + h),
        (cx - r, cy),
        (cx - r / 2.0, cy - h),
        (cx + r / 2.0, cy - h),
    ];
    // Each edge A→B going CCW; Left = 1/4 from A, Right = 3/4 from A
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

fn point_on_connector(connector: &ConnectorKind, step: f32, r: f64, h: f64) -> (f64, f64) {
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
            let (ax, ay) = edge_sub_point(hexagon, a, r, h);
            let (bx, by) = edge_sub_point(hexagon, b, r, h);
            let ctrl = r * 0.6;
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
        }) => {
            let (ax, ay) = edge_sub_point(&connector_a.hexagon, &connector_a.edge_sub, r, h);
            let (bx, by) = edge_sub_point(&connector_b.hexagon, &connector_b.edge_sub, r, h);
            let ctrl = r * 0.6;
            let (nax, nay) = edge_inward_normal(&connector_a.edge_sub.edge);
            let (nbx, nby) = edge_inward_normal(&connector_b.edge_sub.edge);
            cubic(
                (ax, ay),
                (ax - ctrl * nax, ay - ctrl * nay),
                (bx - ctrl * nbx, by - ctrl * nby),
                (bx, by),
            )
        }
        ConnectorKind::HexToHex(ConnectorOutside {
            connector_a,
            connector_b,
        }) => {
            let (ax, ay) = edge_sub_point(&connector_a.hexagon, &connector_a.edge_sub, r, h);
            let (bx, by) = edge_sub_point(&connector_b.hexagon, &connector_b.edge_sub, r, h);
            let ctrl = r * 0.6;
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
            edge_sub_point(&position.hexagon, &position.edge_sub, r, h)
        }
    }
}

impl RenderTask {
    pub fn render(&self, player_data: &PlayerData) -> Result<svg::Document, String> {
        let r = 50.0_f64;
        let h = r * 3.0_f64.sqrt() / 2.0;

        let (mut min_x, mut min_y) = (f64::MAX, f64::MAX);
        let (mut max_x, mut max_y) = (f64::MIN, f64::MIN);
        for hex in &self.hexagons {
            let (cx, cy) = hex_center(hex, r, h);
            min_x = min_x.min(cx - r);
            min_y = min_y.min(cy - h);
            max_x = max_x.max(cx + r);
            max_y = max_y.max(cy + h);
        }

        self.render_with_view(player_data, min_x, max_x, min_y, max_y)
    }

    pub fn render_with_view(
        &self,
        player_data: &PlayerData,
        min_x: f64,
        max_x: f64,
        min_y: f64,
        max_y: f64,
    ) -> Result<svg::Document, String> {
        use svg::Document;

        let r = 50.0_f64;
        let h = r * 3.0_f64.sqrt() / 2.0;

        let mut document = Document::new();

        let Self {
            hexagons,
            hexagon_to_highlight,
            connectors,
            current_player_position,
        } = self;

        // Step1: add the hexagon boundaries to the svg
        // fill also in the background
        // note: for the highlighted hexagon, use a different boundary color and a different background color
        {
            for hex in hexagons {
                let (cx, cy) = hex_center(hex, r, h);

                let vertices = [
                    (cx + r, cy),
                    (cx + r / 2.0, cy + h),
                    (cx - r / 2.0, cy + h),
                    (cx - r, cy),
                    (cx - r / 2.0, cy - h),
                    (cx + r / 2.0, cy - h),
                ];
                let points = vertices
                    .iter()
                    .map(|(x, y)| format!("{:.2},{:.2}", x, y))
                    .collect::<Vec<_>>()
                    .join(" ");

                let is_highlighted = hexagon_to_highlight
                    .as_ref()
                    .is_some_and(|hl| hl.x == hex.x && hl.y == hex.y);

                let (fill, stroke) = if is_highlighted {
                    (
                        player_data.highlighted_hex_fill.to_svg_string(),
                        player_data.highlighted_hex_stroke.to_svg_string(),
                    )
                } else {
                    (
                        player_data.hex_fill.to_svg_string(),
                        player_data.hex_stroke.to_svg_string(),
                    )
                };

                let polygon = Polygon::new()
                    .set("points", points)
                    .set("fill", fill)
                    .set("stroke", stroke)
                    .set("stroke-width", 2);

                document = document.add(polygon);
            }
        }

        // Step2: add the connectors
        {
            for connector in connectors {
                let UsedConnector {
                    connector,
                    used_by,
                    preview_used_by,
                    is_connected_to_dead_end,
                    is_connected_to_player_start,
                    is_connected_to_player_target,
                    is_player_start,
                } = connector;

                let player_color = |pid: &PlayerId| -> Color {
                    player_data
                        .colors
                        .get(pid)
                        .copied()
                        .unwrap_or(player_data.unused_color)
                };
                let color: String = if !used_by.is_empty() {
                    let colors: Vec<Color> = used_by
                        .iter()
                        .chain(preview_used_by.iter())
                        .map(player_color)
                        .collect();
                    mix_colors(&colors)
                } else if !is_connected_to_player_start.is_empty() {
                    mix_colors(
                        &is_connected_to_player_start
                            .iter()
                            .map(player_color)
                            .collect::<Vec<_>>(),
                    )
                } else if !is_connected_to_player_target.is_empty() {
                    mix_colors(
                        &is_connected_to_player_target
                            .iter()
                            .map(player_color)
                            .collect::<Vec<_>>(),
                    )
                } else if *is_connected_to_dead_end {
                    player_data.dead_end_color.to_svg_string().to_string()
                } else {
                    player_data.closed_loop_color.to_svg_string().to_string()
                };
                let opacity: f64 = if !preview_used_by.is_empty() {
                    0.5
                } else {
                    1.0
                };
                let stroke_width: f64 = if !preview_used_by.is_empty() {
                    3.0
                } else if !is_connected_to_player_start.is_empty() {
                    5.0
                } else if !is_connected_to_player_target.is_empty() {
                    1.5
                } else {
                    3.0
                };

                match connector {
                    ConnectorKind::DeadEnd(ConnectorDeadEnd { position }) => {
                        let (px, py) = edge_sub_point(&position.hexagon, &position.edge_sub, r, h);
                        let (nx, ny) = edge_inward_normal(&position.edge_sub.edge);

                        let make_arrow = |tail: (f64, f64), tip: (f64, f64)| {
                            let (dx, dy) = (tip.0 - tail.0, tip.1 - tail.1);
                            let len = (dx * dx + dy * dy).sqrt();
                            let (dx, dy) = (dx / len, dy / len);
                            let (perp_x, perp_y) = (-dy, dx);
                            let head = r * 0.2;
                            Data::new()
                                .move_to(tail)
                                .line_to(tip)
                                .move_to((
                                    tip.0 - dx * head + perp_x * head,
                                    tip.1 - dy * head + perp_y * head,
                                ))
                                .line_to(tip)
                                .line_to((
                                    tip.0 - dx * head - perp_x * head,
                                    tip.1 - dy * head - perp_y * head,
                                ))
                        };

                        let data = if !is_player_start.is_empty() {
                            // arrow from outside pointing toward the edge — player enters here
                            make_arrow((px - nx * r * 0.5, py - ny * r * 0.5), (px, py))
                        } else if !is_connected_to_player_target.is_empty() {
                            // arrow pointing outward (away from hex) — player exits here
                            make_arrow((px, py), (px - nx * r * 0.5, py - ny * r * 0.5))
                        } else {
                            // X at 45° to the edge
                            let (tx, ty) = (-ny, nx);
                            let f = r * 0.2 / 2.0_f64.sqrt();
                            Data::new()
                                .move_to((px - (tx + nx) * f, py - (ty + ny) * f))
                                .line_to((px + (tx + nx) * f, py + (ty + ny) * f))
                                .move_to((px - (tx - nx) * f, py - (ty - ny) * f))
                                .line_to((px + (tx - nx) * f, py + (ty - ny) * f))
                        };

                        let path_elem = Path::new()
                            .set("d", data)
                            .set("fill", "none")
                            .set("stroke", color)
                            .set("stroke-width", stroke_width)
                            .set("opacity", opacity);
                        document = document.add(path_elem);
                    }
                    ConnectorKind::OnHex(ConnectorOnHex {
                        hexagon,
                        edge_sub: ConnectorEdgeSub { a, b },
                    }) => {
                        let (ax, ay) = edge_sub_point(hexagon, a, r, h);
                        let (bx, by) = edge_sub_point(hexagon, b, r, h);
                        let ctrl = r * 0.6;
                        let (nax, nay) = edge_inward_normal(&a.edge);
                        let (nbx, nby) = edge_inward_normal(&b.edge);
                        let data = Data::new().move_to((ax, ay)).cubic_curve_to((
                            ax + ctrl * nax,
                            ay + ctrl * nay,
                            bx + ctrl * nbx,
                            by + ctrl * nby,
                            bx,
                            by,
                        ));
                        let path_elem = Path::new()
                            .set("d", data)
                            .set("fill", "none")
                            .set("stroke", color)
                            .set("stroke-width", stroke_width)
                            .set("opacity", opacity);
                        document = document.add(path_elem);
                    }
                    ConnectorKind::Outside(ConnectorOutside {
                        connector_a,
                        connector_b,
                    })
                    | ConnectorKind::HexToHex(ConnectorOutside {
                        connector_a,
                        connector_b,
                    }) => {
                        let (ax, ay) =
                            edge_sub_point(&connector_a.hexagon, &connector_a.edge_sub, r, h);
                        let (bx, by) =
                            edge_sub_point(&connector_b.hexagon, &connector_b.edge_sub, r, h);
                        let ctrl = r * 0.6;
                        let (nax, nay) = edge_inward_normal(&connector_a.edge_sub.edge);
                        let (nbx, nby) = edge_inward_normal(&connector_b.edge_sub.edge);
                        let data = Data::new().move_to((ax, ay)).cubic_curve_to((
                            ax - ctrl * nax,
                            ay - ctrl * nay,
                            bx - ctrl * nbx,
                            by - ctrl * nby,
                            bx,
                            by,
                        ));
                        let path_elem = Path::new()
                            .set("d", data)
                            .set("fill", "none")
                            .set("stroke", color)
                            .set("stroke-width", stroke_width)
                            .set("opacity", opacity);
                        document = document.add(path_elem);
                    }
                }
            }
        }

        // Step3: render current player positions
        for cpp in current_player_position {
            let (px, py) = point_on_connector(&cpp.connector, cpp.step, r, h);
            let color = player_data
                .colors
                .get(&cpp.player_id)
                .copied()
                .unwrap_or(player_data.unused_color)
                .to_svg_string();
            if !cpp.is_active
                && let ConnectorKind::DeadEnd(ConnectorDeadEnd { position }) = &cpp.connector
            {
                let (nx, ny) = edge_inward_normal(&position.edge_sub.edge);
                let tip = (px - nx * r * 0.5, py - ny * r * 0.5);
                let (dx, dy) = (tip.0 - px, tip.1 - py);
                let len = (dx * dx + dy * dy).sqrt();
                let (dx, dy) = (dx / len, dy / len);
                let (perp_x, perp_y) = (-dy, dx);
                let head = r * 0.2;
                let data = Data::new()
                    .move_to((px, py))
                    .line_to(tip)
                    .move_to((
                        tip.0 - dx * head + perp_x * head,
                        tip.1 - dy * head + perp_y * head,
                    ))
                    .line_to(tip)
                    .line_to((
                        tip.0 - dx * head - perp_x * head,
                        tip.1 - dy * head - perp_y * head,
                    ));
                let arrow = Path::new()
                    .set("fill", "none")
                    .set("stroke", color)
                    .set("stroke-width", 3)
                    .set("stroke-linecap", "round")
                    .set("d", data);
                document = document.add(arrow);
                continue;
            }
            let circle = Circle::new()
                .set("cx", px)
                .set("cy", py)
                .set("r", 7)
                .set("fill", color)
                .set("stroke", "white")
                .set("stroke-width", 1.5);
            document = document.add(circle);
        }

        if !hexagons.is_empty() {
            let (w, h_box) = ((max_x - min_x) * 1.3, (max_y - min_y) * 1.3);
            let (vx, vy) = (
                min_x - (max_x - min_x) * 0.15,
                min_y - (max_y - min_y) * 0.15,
            );
            document = document
                .set("viewBox", format!("{vx:.2} {vy:.2} {w:.2} {h_box:.2}"))
                .set("width", format!("{w:.2}"))
                .set("height", format!("{h_box:.2}"));
        }

        Ok(document)
    }
}

#[cfg(test)]
mod tests {
    use strum::IntoEnumIterator;

    use crate::game_options::{GameOptionsStandard, OuterConnectors};

    use super::*;

    fn pos(x: i32, y: i32) -> HexagonPosition {
        HexagonPosition { x, y }
    }
    fn es(edge: Edge, sub: Sub) -> EdgeSub {
        EdgeSub { edge, sub }
    }
    fn cp(x: i32, y: i32, edge: Edge, sub: Sub) -> ConnectorPosition {
        ConnectorPosition {
            hexagon: pos(x, y),
            edge_sub: es(edge, sub),
        }
    }

    const P0: PlayerId = PlayerId(0); // Green
    const P1: PlayerId = PlayerId(1); // Red
    const P2: PlayerId = PlayerId(2); // Blue

    fn uc(
        connector: ConnectorKind,
        used_by: Vec<PlayerId>,
        preview_used_by: Vec<PlayerId>,
        is_connected_to_dead_end: bool,
        is_connected_to_player_start: Option<PlayerId>,
        is_connected_to_player_target: Option<PlayerId>,
        is_player_start: Option<PlayerId>,
    ) -> UsedConnector {
        UsedConnector {
            connector,
            used_by,
            preview_used_by,
            is_connected_to_dead_end,
            is_connected_to_player_start: is_connected_to_player_start.into_iter().collect(),
            is_connected_to_player_target: is_connected_to_player_target.into_iter().collect(),
            is_player_start: is_player_start.into_iter().collect(),
        }
    }

    #[test]
    pub fn test_render_all_cases() {
        use Edge::*;
        use Sub::*;

        let rendertask = RenderTask {
            hexagons: vec![
                pos(0, 0),
                pos(1, 0),
                pos(2, 0),
                pos(0, 1),
                pos(1, 1),
                pos(2, 1),
            ],
            hexagon_to_highlight: Some(pos(1, 0)),
            connectors: vec![
                // --- DeadEnd: inward arrow (player_start → Red, thick) ---
                uc(
                    ConnectorKind::DeadEnd(ConnectorDeadEnd {
                        position: cp(0, 0, Top, Left),
                    }),
                    vec![],
                    vec![],
                    false,
                    Some(P1),
                    None,
                    Some(P1),
                ),
                // --- DeadEnd: outward arrow (player_target → Blue, thin) ---
                uc(
                    ConnectorKind::DeadEnd(ConnectorDeadEnd {
                        position: cp(2, 0, Top, Right),
                    }),
                    vec![],
                    vec![],
                    false,
                    None,
                    Some(P2),
                    None,
                ),
                // --- DeadEnd: X in dead_end_color (Gray) ---
                uc(
                    ConnectorKind::DeadEnd(ConnectorDeadEnd {
                        position: cp(0, 1, Bottom, Left),
                    }),
                    vec![],
                    vec![],
                    true,
                    None,
                    None,
                    None,
                ),
                // --- DeadEnd: X in unused_color (Golden) ---
                uc(
                    ConnectorKind::DeadEnd(ConnectorDeadEnd {
                        position: cp(2, 1, Bottom, Right),
                    }),
                    vec![],
                    vec![],
                    false,
                    None,
                    None,
                    None,
                ),
                // --- OnHex: used_by single player (Red, normal) ---
                uc(
                    ConnectorKind::OnHex(ConnectorOnHex {
                        hexagon: pos(1, 0),
                        edge_sub: ConnectorEdgeSub {
                            a: es(BottomLeft, Left),
                            b: es(BottomRight, Right),
                        },
                    }),
                    vec![P1],
                    vec![],
                    false,
                    None,
                    None,
                    None,
                ),
                // --- OnHex: used_by two players + preview (mixed Red+Blue+Blue, translucent) ---
                uc(
                    ConnectorKind::OnHex(ConnectorOnHex {
                        hexagon: pos(0, 0),
                        edge_sub: ConnectorEdgeSub {
                            a: es(TopLeft, Left),
                            b: es(TopRight, Right),
                        },
                    }),
                    vec![P1, P2],
                    vec![P2],
                    false,
                    None,
                    None,
                    None,
                ),
                // --- OnHex: player_start (Red, thick) ---
                uc(
                    ConnectorKind::OnHex(ConnectorOnHex {
                        hexagon: pos(2, 0),
                        edge_sub: ConnectorEdgeSub {
                            a: es(BottomLeft, Left),
                            b: es(BottomRight, Left),
                        },
                    }),
                    vec![],
                    vec![],
                    false,
                    Some(P1),
                    None,
                    None,
                ),
                // --- OnHex: player_target (Blue, thin) ---
                uc(
                    ConnectorKind::OnHex(ConnectorOnHex {
                        hexagon: pos(0, 1),
                        edge_sub: ConnectorEdgeSub {
                            a: es(TopLeft, Left),
                            b: es(TopRight, Right),
                        },
                    }),
                    vec![],
                    vec![],
                    false,
                    None,
                    Some(P2),
                    None,
                ),
                // --- OnHex: dead_end (Gray, normal) ---
                uc(
                    ConnectorKind::OnHex(ConnectorOnHex {
                        hexagon: pos(1, 1),
                        edge_sub: ConnectorEdgeSub {
                            a: es(Top, Left),
                            b: es(Bottom, Right),
                        },
                    }),
                    vec![],
                    vec![],
                    true,
                    None,
                    None,
                    None,
                ),
                // --- OnHex: unused (Golden, normal) ---
                uc(
                    ConnectorKind::OnHex(ConnectorOnHex {
                        hexagon: pos(2, 1),
                        edge_sub: ConnectorEdgeSub {
                            a: es(TopLeft, Right),
                            b: es(TopRight, Left),
                        },
                    }),
                    vec![],
                    vec![],
                    false,
                    None,
                    None,
                    None,
                ),
                // --- Outside: used_by single + preview (Red, translucent) ---
                uc(
                    ConnectorKind::Outside(ConnectorOutside {
                        connector_a: cp(0, 0, BottomRight, Right),
                        connector_b: cp(1, 0, TopLeft, Left),
                    }),
                    vec![P1],
                    vec![P1],
                    false,
                    None,
                    None,
                    None,
                ),
                // --- Outside: player_start (Red, thick) ---
                uc(
                    ConnectorKind::Outside(ConnectorOutside {
                        connector_a: cp(1, 1, TopRight, Left),
                        connector_b: cp(2, 0, BottomLeft, Right),
                    }),
                    vec![],
                    vec![],
                    false,
                    Some(P1),
                    None,
                    None,
                ),
                // --- Outside: player_target (Blue, thin) ---
                uc(
                    ConnectorKind::Outside(ConnectorOutside {
                        connector_a: cp(2, 1, TopRight, Right),
                        connector_b: cp(2, 0, BottomRight, Left),
                    }),
                    vec![],
                    vec![],
                    false,
                    None,
                    Some(P2),
                    None,
                ),
                // --- Outside: unused (Golden, normal) ---
                uc(
                    ConnectorKind::Outside(ConnectorOutside {
                        connector_a: cp(1, 0, BottomRight, Left),
                        connector_b: cp(2, 0, BottomLeft, Left),
                    }),
                    vec![],
                    vec![],
                    false,
                    None,
                    None,
                    None,
                ),
            ],
            current_player_position: vec![],
        };

        let player_data = PlayerData {
            colors: HashMap::from([(P1, Color::Red), (P2, Color::Blue)]),
            dead_end_color: Color::Gray,
            closed_loop_color: Color::Teal,
            unused_color: Color::Golden,
            hex_fill: Color::Beige,
            hex_stroke: Color::DarkGray,
            highlighted_hex_fill: Color::Moccasin,
            highlighted_hex_stroke: Color::DarkOrange,
        };

        let svg = rendertask.render(&player_data).unwrap();
        let path = format!("{}/../target/test.svg", env!("CARGO_MANIFEST_DIR"));
        dbg!(&path);
        std::fs::write(path, svg.to_string()).unwrap();
    }

    #[test]
    pub fn test_render_player_positions() {
        use Edge::*;
        use Sub::*;

        let make_connector = || {
            ConnectorKind::OnHex(ConnectorOnHex {
                hexagon: pos(0, 0),
                edge_sub: ConnectorEdgeSub {
                    a: es(BottomLeft, Left),
                    b: es(BottomRight, Right),
                },
            })
        };

        let rendertask = RenderTask {
            hexagons: vec![pos(0, 0)],
            hexagon_to_highlight: None,
            connectors: vec![uc(
                make_connector(),
                vec![],
                vec![],
                false,
                None,
                None,
                None,
            )],
            current_player_position: (0..10)
                .map(|i| CurrentPlayerPosition {
                    player_id: PlayerId(i + 1),
                    connector: make_connector(),
                    step: i as f32 / 9.0,
                    is_active: true,
                })
                .collect(),
        };

        let colors = [
            Color::Red,
            Color::Blue,
            Color::Green,
            Color::DarkOrange,
            Color::Purple,
            Color::Cyan,
            Color::Pink,
            Color::Golden,
            Color::Gray,
            Color::Moccasin,
        ];
        let player_data = PlayerData {
            colors: HashMap::from_iter((1..=10).map(|i| (PlayerId(i), colors[i as usize - 1]))),
            dead_end_color: Color::Gray,
            closed_loop_color: Color::Teal,
            unused_color: Color::Golden,
            hex_fill: Color::Beige,
            hex_stroke: Color::DarkGray,
            highlighted_hex_fill: Color::Moccasin,
            highlighted_hex_stroke: Color::DarkOrange,
        };

        let svg = rendertask.render(&player_data).unwrap();
        let path = format!(
            "{}/../target/test_player_positions.svg",
            env!("CARGO_MANIFEST_DIR")
        );
        dbg!(&path);
        std::fs::write(path, svg.to_string()).unwrap();
    }

    #[test]
    pub fn test_board_size() {
        for outer_connector in OuterConnectors::iter() {
            for radius in 1..10 {
                let Board {
                    hexagons,
                    connectors,
                } = Board::create_board(radius, outer_connector.clone()).unwrap();
                let connectors = connectors
                    .into_iter()
                    .map(|c| {
                        let is_connected_to_dead_end = match &c.kind {
                            ConnectorKind::DeadEnd(_) => true,
                            ConnectorKind::HexToHex(_)
                            | ConnectorKind::OnHex(_)
                            | ConnectorKind::Outside(_) => false,
                        };
                        UsedConnector {
                            connector: c.kind,
                            used_by: [].into(),
                            preview_used_by: [].into(),
                            is_connected_to_dead_end,
                            is_connected_to_player_start: Default::default(),
                            is_connected_to_player_target: Default::default(),
                            is_player_start: Default::default(),
                        }
                    })
                    .collect();
                let rendertask = RenderTask {
                    hexagons,
                    hexagon_to_highlight: None,
                    connectors,
                    current_player_position: [].into(),
                };

                let player_data = PlayerData {
                    colors: HashMap::from([(P0, Color::Red), (P1, Color::Blue)]),
                    dead_end_color: Color::Gray,
                    unused_color: Color::Golden,
                    hex_fill: Color::Beige,
                    hex_stroke: Color::DarkGray,
                    highlighted_hex_fill: Color::Moccasin,
                    highlighted_hex_stroke: Color::DarkOrange,
                    closed_loop_color: Color::Teal,
                };

                let svg = rendertask.render(&player_data).unwrap();
                let path = format!(
                    "{}/../target/board_{radius}_{outer_connector:?}.svg",
                    env!("CARGO_MANIFEST_DIR")
                );
                dbg!(&path);
                std::fs::write(path, svg.to_string()).unwrap();
            }
        }
    }

    #[test]
    pub fn test_play_tile() {
        let options = GameOptionsStandard {
            board_radius: 2,
            outer_connectors: OuterConnectors::ReducedDeathEnds,
            random_seed: 0,
            player_count: 2,
            collision_mode: game_options::CollisionMode::PassThrough,
            winning_condition: game_options::WinningConditionStandard::HighestVelocity,
            hand_size: 3,
        };
        let mut game = options.start_game().unwrap();

        let player_data = PlayerData {
            colors: HashMap::from([(P0, Color::Green), (P1, Color::Red)]),
            dead_end_color: Color::Gray,
            closed_loop_color: Color::Teal,
            unused_color: Color::Golden,
            hex_fill: Color::Beige,
            hex_stroke: Color::DarkGray,
            highlighted_hex_fill: Color::Moccasin,
            highlighted_hex_stroke: Color::DarkOrange,
        };
        for tile_count in 0..3 {
            let count = 10;
            for time in 0..=count {
                let rendertask = game.render_task(None, time as f32 / (count as f32));
                let svg = rendertask.render(&player_data).unwrap();
                let path = format!(
                    "{}/../target/play_tile_step_{tile_count}_time_{time}.svg",
                    env!("CARGO_MANIFEST_DIR")
                );
                std::fs::write(path, svg.to_string()).unwrap();
            }
            let json_path = format!(
                "{}/../target/play_tile_step_{tile_count}.json",
                env!("CARGO_MANIFEST_DIR")
            );
            std::fs::write(json_path, serde_json::to_string_pretty(&game).unwrap()).unwrap();
            game.play_tile(0);
        }
    }
}
