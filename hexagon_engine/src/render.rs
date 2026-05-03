use std::collections::HashMap;

use svg::node::element::path::Data;
use svg::node::element::{Circle, Path, Polygon};

use crate::*;
pub struct RenderTask {
    pub hexagons: Vec<HexagonPosition>,
    pub hexagon_to_highlight: Option<HexagonPosition>,
    pub connectors: Vec<UsedConnector>,
}

pub struct UsedConnector {
    pub connector: ConnectorKind,
    pub used_by: Vec<PlayerId>,
    pub previews_used_by: Vec<PlayerId>,
    pub is_connected_to_dead_end: bool,
    pub is_connected_to_player_start: Option<PlayerId>,
    pub is_connected_to_player_target: Option<PlayerId>,
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
}

impl Color {
    pub fn to_svg_string(self) -> &'static str {
        match self {
            Color::Gray       => "#808080",
            Color::Golden     => "#FFD700",
            Color::Beige      => "#F5F5DC",
            Color::DarkGray   => "#555555",
            Color::Moccasin   => "#FFE4B5",
            Color::DarkOrange => "#FF8C00",
            Color::Green      => "#228B22",
            Color::Blue       => "#1E90FF",
            Color::Red        => "#DC143C",
        }
    }
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
    let cx = r + hex.x as f64 * 1.5 * r;
    let cy = h + hex.y as f64 * 2.0 * h + if hex.x % 2 != 0 { h } else { 0.0 };
    (cx, cy)
}

// Unit vector pointing from the edge midpoint toward the hex center
fn edge_inward_normal(edge: &Edge) -> (f64, f64) {
    let s = 3.0_f64.sqrt() / 2.0;
    match edge {
        Edge::Top         => ( 0.0,  1.0),
        Edge::TopLeft     => ( s,    0.5),
        Edge::BottomLeft  => ( s,   -0.5),
        Edge::Bottom      => ( 0.0, -1.0),
        Edge::BottomRight => (-s,   -0.5),
        Edge::TopRight    => (-s,    0.5),
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

impl RenderTask {
    pub fn render(&self, player_data: &PlayerData) -> Result<svg::Document, String> {
        use svg::Document;

        let mut document = Document::new();

        let Self {
            hexagons,
            hexagon_to_highlight,
            connectors,
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
                    previews_used_by,
                    is_connected_to_dead_end,
                    is_connected_to_player_start,
                    is_connected_to_player_target,
                } = connector;
                match connector {
                    ConnectorKind::DeadEnd(ConnectorDeadEnd { position }) => {
                        let (px, py) = edge_sub_point(&position.hexagon, &position.edge_sub, R, h);
                        let circle = Circle::new()
                            .set("cx", px)
                            .set("cy", py)
                            .set("r", 5)
                            .set("fill", "#808080");
                        document = document.add(circle);
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
                        let data = Data::new()
                            .move_to((ax, ay))
                            .cubic_curve_to((
                                ax + ctrl * nax, ay + ctrl * nay,
                                bx + ctrl * nbx, by + ctrl * nby,
                                bx, by,
                            ));
                        let path_elem = Path::new()
                            .set("d", data)
                            .set("fill", "none")
                            .set("stroke", "#808080")
                            .set("stroke-width", 3);
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
                        // control points go outward (away from each hex)
                        let data = Data::new()
                            .move_to((ax, ay))
                            .cubic_curve_to((
                                ax - ctrl * nax, ay - ctrl * nay,
                                bx - ctrl * nbx, by - ctrl * nby,
                                bx, by,
                            ));
                        let path_elem = Path::new()
                            .set("d", data)
                            .set("fill", "none")
                            .set("stroke", "#808080")
                            .set("stroke-width", 3);
                        document = document.add(path_elem);
                    }
                }
            }
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
    use super::*;

    #[test]
    pub fn test_render_simple() {
        let rendertask = RenderTask {
            hexagons: [
                HexagonPosition { x: 0, y: 0 },
                HexagonPosition { x: 1, y: 0 },
                HexagonPosition { x: 0, y: 1 },
                HexagonPosition { x: 1, y: 1 },
            ]
            .into(),
            hexagon_to_highlight: Some(HexagonPosition { x: 0, y: 1 }),
            connectors: [
                UsedConnector {
                    connector: ConnectorKind::DeadEnd(ConnectorDeadEnd {
                        position: ConnectorPosition {
                            hexagon: HexagonPosition { x: 0, y: 1 },
                            edge_sub: EdgeSub {
                                edge: Edge::Bottom,
                                sub: Sub::Left,
                            },
                        },
                    }),
                    used_by: Default::default(),
                    previews_used_by: Default::default(),
                    is_connected_to_dead_end: true,
                    is_connected_to_player_start: None,
                    is_connected_to_player_target: None,
                },
                UsedConnector {
                    connector: ConnectorKind::OnHex(ConnectorOnHex {
                        hexagon: HexagonPosition { x: 1, y: 1 },
                        edge_sub: ConnectorEdgeSub {
                            a: EdgeSub {
                                edge: Edge::Bottom,
                                sub: Sub::Left,
                            },
                            b: EdgeSub {
                                edge: Edge::BottomRight,
                                sub: Sub::Right,
                            },
                        },
                    }),
                    used_by: Default::default(),
                    previews_used_by: Default::default(),
                    is_connected_to_dead_end: true,
                    is_connected_to_player_start: None,
                    is_connected_to_player_target: None,
                },
                UsedConnector {
                    connector: ConnectorKind::Outside(ConnectorOutside {
                        connector_a: ConnectorPosition {
                            hexagon: HexagonPosition { x: 1, y: 0 },
                            edge_sub: EdgeSub {
                                edge: Edge::Top,
                                sub: Sub::Right,
                            },
                        },
                        connector_b: ConnectorPosition {
                            hexagon: HexagonPosition { x: 0, y: 0 },
                            edge_sub: EdgeSub {
                                edge: Edge::TopRight,
                                sub: Sub::Left,
                            },
                        },
                    }),
                    used_by: Default::default(),
                    previews_used_by: Default::default(),
                    is_connected_to_dead_end: true,
                    is_connected_to_player_start: None,
                    is_connected_to_player_target: None,
                },
            ]
            .into(),
        };
        let player_data = PlayerData {
            colors: Default::default(),
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
}
