use std::collections::HashMap;

use svg::node::element::Polygon;

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

// a color like #ff0000
#[derive(Clone, Copy)]
pub struct Color(&'static str);
impl Color {
    pub const GRAY: Color = Color("#808080");
    pub const GOLDEN: Color = Color("#FFD700");
    pub const BEIGE: Color = Color("#F5F5DC");
    pub const DARK_GRAY: Color = Color("#555555");
    pub const MOCCASIN: Color = Color("#FFE4B5");
    pub const DARK_ORANGE: Color = Color("#FF8C00");

    fn as_str(self) -> &'static str {
        self.0
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
                let cx = R + hex.x as f64 * 1.5 * R;
                let cy = h + hex.y as f64 * 2.0 * h + if hex.x % 2 != 0 { h } else { 0.0 };
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
                        player_data.highlighted_hex_fill.as_str(),
                        player_data.highlighted_hex_stroke.as_str(),
                    )
                } else {
                    (
                        player_data.hex_fill.as_str(),
                        player_data.hex_stroke.as_str(),
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
            ]
            .into(),
        };
        let player_data = PlayerData {
            colors: Default::default(),
            dead_end_color: Color::GRAY,
            unused_color: Color::GOLDEN,
            hex_fill: Color::BEIGE,
            hex_stroke: Color::DARK_GRAY,
            highlighted_hex_fill: Color::MOCCASIN,
            highlighted_hex_stroke: Color::DARK_ORANGE,
        };
        let svg = rendertask.render(&player_data).unwrap();
        let path = format!("{}/../target/test.svg", env!("CARGO_MANIFEST_DIR"));
        dbg!(&path);
        std::fs::write(path, svg.to_string()).unwrap();
    }
}
