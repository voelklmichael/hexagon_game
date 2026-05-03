use std::collections::HashMap;
use std::path::Path;

const HEX_SIZE: f64 = 40.0;
const PADDING: f64 = 50.0;

impl super::HexagonBoard {
    pub fn render(&self, output_file: &Path) {
        let Self {
            hexagons,
            connectors,
        } = self;

        let active: Vec<_> = hexagons.iter().filter(|h| !h.was_removed).collect();

        if active.is_empty() {
            std::fs::write(
                output_file,
                r#"<svg xmlns="http://www.w3.org/2000/svg"></svg>"#,
            )
            .unwrap();
            return;
        }

        let centers: Vec<(f64, f64)> = active
            .iter()
            .map(|h| axial_to_pixel(h.position.x, h.position.y))
            .collect();

        let min_x = centers
            .iter()
            .map(|(x, _)| *x)
            .fold(f64::INFINITY, f64::min);
        let max_x = centers
            .iter()
            .map(|(x, _)| *x)
            .fold(f64::NEG_INFINITY, f64::max);
        let min_y = centers
            .iter()
            .map(|(_, y)| *y)
            .fold(f64::INFINITY, f64::min);
        let max_y = centers
            .iter()
            .map(|(_, y)| *y)
            .fold(f64::NEG_INFINITY, f64::max);

        let offset_x = -min_x + PADDING + HEX_SIZE;
        let offset_y = -min_y + PADDING + HEX_SIZE;
        let width = max_x - min_x + 2.0 * (PADDING + HEX_SIZE);
        let height = max_y - min_y + 2.0 * (PADDING + HEX_SIZE);

        // Map hexagon IDs to their pixel centers for connector lookup
        let hex_map: HashMap<u32, (f64, f64)> = active
            .iter()
            .zip(centers.iter())
            .map(|(h, &cp)| (h.id.0, cp))
            .collect();

        let mut svg = format!(
            "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"{:.0}\" height=\"{:.0}\">\n",
            width, height
        );

        for (cx, cy) in centers {
            let cx = cx + offset_x;
            let cy = cy + offset_y;
            let verts = hex_vertices(cx, cy);
            let pts: String = verts
                .iter()
                .map(|(vx, vy)| format!("{vx:.1},{vy:.1}"))
                .collect::<Vec<_>>()
                .join(" ");
            svg.push_str(&format!(
                "  <polygon points=\"{pts}\" fill=\"#e8d5b7\" stroke=\"#8b6914\" stroke-width=\"2\"/>\n"
            ));
        }

        for connector in connectors {
            match connector {
                crate::HexagonConnector::Direct(d) => {
                    if d.was_removed {
                        continue;
                    }
                    if let (Some(&a_pos), Some(&b_pos)) = (
                        hex_map.get(&d.connector_a.hexagon.0),
                        hex_map.get(&d.connector_b.hexagon.0),
                    ) {
                        let a_cx = a_pos.0 + offset_x;
                        let a_cy = a_pos.1 + offset_y;
                        let b_cx = b_pos.0 + offset_x;
                        let b_cy = b_pos.1 + offset_y;
                        let m1 = get_sub_edge_midpoint(
                            a_cx,
                            a_cy,
                            &d.connector_a.edge,
                            &d.connector_a.sub,
                        );
                        let m2 = get_sub_edge_midpoint(
                            b_cx,
                            b_cy,
                            &d.connector_b.edge,
                            &d.connector_b.sub,
                        );

                        let x1 = a_cx + (m1.0 - a_cx) * 0.85;
                        let y1 = a_cy + (m1.1 - a_cy) * 0.85;
                        let x2 = b_cx + (m2.0 - b_cx) * 0.85;
                        let y2 = b_cy + (m2.1 - b_cy) * 0.85;

                        svg.push_str(&format!(
                            "  <line x1=\"{x1:.1}\" y1=\"{y1:.1}\" x2=\"{x2:.1}\" y2=\"{y2:.1}\" stroke=\"#5a3e0a\" stroke-width=\"2\" stroke-linecap=\"round\"/>\n"
                        ));
                    }
                }
                crate::HexagonConnector::DeadEnd(d) => {
                    if d.was_removed {
                        continue;
                    }
                    if let Some(&a_pos) = hex_map.get(&d.hexagon_a.hexagon.0) {
                        let a_cx = a_pos.0 + offset_x;
                        let a_cy = a_pos.1 + offset_y;
                        let m =
                            get_sub_edge_midpoint(a_cx, a_cy, &d.hexagon_a.edge, &d.hexagon_a.sub);
                        let x = m.0;
                        let y = m.1;
                        let size = 4.0;
                        svg.push_str(&format!(
                            "  <line x1=\"{:.1}\" y1=\"{:.1}\" x2=\"{:.1}\" y2=\"{:.1}\" stroke=\"#8b0000\" stroke-width=\"2\" stroke-linecap=\"round\"/>\n",
                            x - size, y - size, x + size, y + size
                        ));
                        svg.push_str(&format!(
                            "  <line x1=\"{:.1}\" y1=\"{:.1}\" x2=\"{:.1}\" y2=\"{:.1}\" stroke=\"#8b0000\" stroke-width=\"2\" stroke-linecap=\"round\"/>\n",
                            x - size, y + size, x + size, y - size
                        ));
                    }
                }
            }
        }

        svg.push_str("</svg>\n");
        std::fs::write(output_file, svg).unwrap();
    }
}

// Flat-top axial coordinates: q = position.x (east), r = position.y (120° from east)
fn axial_to_pixel(q: i32, r: i32) -> (f64, f64) {
    let x = HEX_SIZE * 1.5 * q as f64;
    let y = HEX_SIZE * (3.0_f64.sqrt() / 2.0 * q as f64 + 3.0_f64.sqrt() * r as f64);
    (x, y)
}

// Flat-top hexagon: vertices at 0°, 60°, 120°, 180°, 240°, 300°
fn hex_vertices(cx: f64, cy: f64) -> [(f64, f64); 6] {
    let mut v = [(0.0, 0.0); 6];
    for i in 0..6 {
        let angle = std::f64::consts::PI / 3.0 * i as f64;
        v[i] = (cx + HEX_SIZE * angle.cos(), cy + HEX_SIZE * angle.sin());
    }
    v
}

fn get_sub_edge_midpoint(
    cx: f64,
    cy: f64,
    edge: &crate::HexagonEdge,
    sub: &crate::HexagonEdgeSub,
) -> (f64, f64) {
    let i = match edge {
        crate::HexagonEdge::BottomRight => 0,
        crate::HexagonEdge::Bottom => 1,
        crate::HexagonEdge::BottomLeft => 2,
        crate::HexagonEdge::TopLeft => 3,
        crate::HexagonEdge::Top => 4,
        crate::HexagonEdge::TopRight => 5,
    };
    let angle1 = std::f64::consts::PI / 3.0 * i as f64;
    let angle2 = std::f64::consts::PI / 3.0 * (i + 1) as f64;
    let v1 = (cx + HEX_SIZE * angle1.cos(), cy + HEX_SIZE * angle1.sin());
    let v2 = (cx + HEX_SIZE * angle2.cos(), cy + HEX_SIZE * angle2.sin());

    match sub {
        crate::HexagonEdgeSub::Left => ((v1.0 * 3.0 + v2.0) / 4.0, (v1.1 * 3.0 + v2.1) / 4.0),
        crate::HexagonEdgeSub::Right => ((v1.0 + v2.0 * 3.0) / 4.0, (v1.1 + v2.1 * 3.0) / 4.0),
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    #[test]
    fn test_board_construction() {
        for radius in 2..8 {
            for add_outer_connectors in [true, false] {
                let options = crate::BoardConstructionOptionsSimple {
                    radius,
                    add_outer_connectors,
                };
                let board = options.construct().unwrap();
                let ouputfile = format!(
                    "{}/target/simple_{radius}_{add_outer_connectors}.svg",
                    env!("CARGO_MANIFEST_DIR")
                );

                let path = Path::new(&ouputfile);
                board.render(path);
            }
        }
    }
}
