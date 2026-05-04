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

pub struct UsedConnector {
    pub connector: ConnectorKind,
    pub used_by: Vec<PlayerId>,
    pub preview_used_by: Vec<PlayerId>,
    pub is_connected_to_dead_end: bool,
    pub is_connected_to_player_start: Option<PlayerId>,
    pub is_connected_to_player_target: Option<PlayerId>,
}

pub struct CurrentPlayerPosition {
    pub player_id: PlayerId,
    pub connector: ConnectorKind,
    // this is a number between 0 and 1
    // it is used for animations
    pub step: f32,
}

#[derive(Clone, Copy)]
pub enum Color {
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
}

impl Color {
    pub fn to_svg_string(self) -> &'static str {
        match self {
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
        }
    }

    fn to_rgb(self) -> (u8, u8, u8) {
        match self {
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
        ConnectorKind::DeadEnd(ConnectorDeadEnd { position }) => {
            edge_sub_point(&position.hexagon, &position.edge_sub, r, h)
        }
    }
}

impl RenderTask {
    pub fn render(&self, player_data: &PlayerData) -> Result<svg::Document, String> {
        use svg::Document;

        let mut document = Document::new();

        let Self {
            hexagons,
            hexagon_to_highlight,
            connectors,
            current_player_position,
        } = self;

        const R: f64 = 50.0;
        let h = R * 3.0_f64.sqrt() / 2.0;

        // Step1: add the hexagon boundaries to the svg
        // fill also in the background
        // note: for the highlighted hexagon, use a different boundary color and a different background color
        // also, compute the bounding box
        let (min_x, max_x, min_y, max_y) = {
            let (mut min_x, mut min_y) = (f64::MAX, f64::MAX);
            let (mut max_x, mut max_y) = (f64::MIN, f64::MIN);

            for hex in hexagons {
                let (cx, cy) = hex_center(hex, R, h);
                min_x = min_x.min(cx - R);
                min_y = min_y.min(cy - h);
                max_x = max_x.max(cx + R);
                max_y = max_y.max(cy + h);

                let vertices = [
                    (cx + R, cy),
                    (cx + R / 2.0, cy + h),
                    (cx - R / 2.0, cy + h),
                    (cx - R, cy),
                    (cx - R / 2.0, cy - h),
                    (cx + R / 2.0, cy - h),
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
            (min_x, max_x, min_y, max_y)
        };

        // Step2: add the connectors
        {
            for connector in connectors {
                let UsedConnector {
                    connector,
                    used_by,
                    preview_used_by: previews_used_by,
                    is_connected_to_dead_end,
                    is_connected_to_player_start,
                    is_connected_to_player_target,
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
                        .chain(previews_used_by.iter())
                        .map(player_color)
                        .collect();
                    mix_colors(&colors)
                } else if let Some(pid) = is_connected_to_player_start.as_ref() {
                    player_color(pid).to_svg_string().to_string()
                } else if let Some(pid) = is_connected_to_player_target.as_ref() {
                    player_color(pid).to_svg_string().to_string()
                } else if *is_connected_to_dead_end {
                    player_data.dead_end_color.to_svg_string().to_string()
                } else {
                    player_data.unused_color.to_svg_string().to_string()
                };
                let opacity: f64 = if !previews_used_by.is_empty() {
                    0.5
                } else {
                    1.0
                };
                let stroke_width: f64 = if !previews_used_by.is_empty() {
                    3.0
                } else if is_connected_to_player_start.is_some() {
                    5.0
                } else if is_connected_to_player_target.is_some() {
                    1.5
                } else {
                    3.0
                };

                match connector {
                    ConnectorKind::DeadEnd(ConnectorDeadEnd { position }) => {
                        let (px, py) = edge_sub_point(&position.hexagon, &position.edge_sub, R, h);
                        let (nx, ny) = edge_inward_normal(&position.edge_sub.edge);

                        let make_arrow = |tail: (f64, f64), tip: (f64, f64)| {
                            let (dx, dy) = (tip.0 - tail.0, tip.1 - tail.1);
                            let len = (dx * dx + dy * dy).sqrt();
                            let (dx, dy) = (dx / len, dy / len);
                            let (perp_x, perp_y) = (-dy, dx);
                            let head = R * 0.2;
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

                        let data = if is_connected_to_player_start.is_some() {
                            // arrow from outside pointing inward, tip at edge-sub point
                            make_arrow((px - nx * R * 0.5, py - ny * R * 0.5), (px, py))
                        } else if is_connected_to_player_target.is_some() {
                            // arrow from edge-sub point pointing outward
                            make_arrow((px, py), (px - nx * R * 0.5, py - ny * R * 0.5))
                        } else {
                            // X at 45° to the edge
                            let (tx, ty) = (-ny, nx);
                            let f = R * 0.2 / 2.0_f64.sqrt();
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
                        let (ax, ay) = edge_sub_point(hexagon, a, R, h);
                        let (bx, by) = edge_sub_point(hexagon, b, R, h);
                        let ctrl = R * 0.6;
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
                    }) => {
                        let (ax, ay) =
                            edge_sub_point(&connector_a.hexagon, &connector_a.edge_sub, R, h);
                        let (bx, by) =
                            edge_sub_point(&connector_b.hexagon, &connector_b.edge_sub, R, h);
                        let ctrl = R * 0.6;
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
            let (px, py) = point_on_connector(&cpp.connector, cpp.step, R, h);
            let color = player_data
                .colors
                .get(&cpp.player_id)
                .copied()
                .unwrap_or(player_data.unused_color)
                .to_svg_string();
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
            let (w, h_box) = ((max_x - min_x) * 1.1, (max_y - min_y) * 1.1);
            let (vx, vy) = (
                min_x - (max_x - min_x) * 0.05,
                min_y - (max_y - min_y) * 0.05,
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

    use crate::game_options::OuterConnectors;

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

    const P1: PlayerId = PlayerId(1); // Red
    const P2: PlayerId = PlayerId(2); // Blue

    fn uc(
        connector: ConnectorKind,
        used_by: Vec<PlayerId>,
        preview_used_by: Vec<PlayerId>,
        is_connected_to_dead_end: bool,
        is_connected_to_player_start: Option<PlayerId>,
        is_connected_to_player_target: Option<PlayerId>,
    ) -> UsedConnector {
        UsedConnector {
            connector,
            used_by,
            preview_used_by,
            is_connected_to_dead_end,
            is_connected_to_player_start,
            is_connected_to_player_target,
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
                ),
            ],
            current_player_position: vec![],
        };

        let player_data = PlayerData {
            colors: HashMap::from([(P1, Color::Red), (P2, Color::Blue)]),
            dead_end_color: Color::Gray,
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
            connectors: vec![uc(make_connector(), vec![], vec![], false, None, None)],
            current_player_position: (0..10)
                .map(|i| CurrentPlayerPosition {
                    player_id: PlayerId(i + 1),
                    connector: make_connector(),
                    step: i as f32 / 9.0,
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
                            ConnectorKind::OnHex(_) | ConnectorKind::Outside(_) => false,
                        };
                        UsedConnector {
                            connector: c.kind,
                            used_by: [].into(),
                            preview_used_by: [].into(),
                            is_connected_to_dead_end,
                            is_connected_to_player_start: None,
                            is_connected_to_player_target: None,
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
                    colors: HashMap::from([(P1, Color::Red), (P2, Color::Blue)]),
                    dead_end_color: Color::Gray,
                    unused_color: Color::Golden,
                    hex_fill: Color::Beige,
                    hex_stroke: Color::DarkGray,
                    highlighted_hex_fill: Color::Moccasin,
                    highlighted_hex_stroke: Color::DarkOrange,
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
}
