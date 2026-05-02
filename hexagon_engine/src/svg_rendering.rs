use std::path::Path;

const HEX_SIZE: f64 = 40.0;
const PADDING: f64 = 50.0;

impl super::HexagonBoard {
    pub fn render(&self, output_file: &Path) {
        let Self {
            hexagons,
            connectors: _,
        } = self;

        let active: Vec<_> = hexagons.iter().filter(|h| !h.was_removed).collect();

        if active.is_empty() {
            std::fs::write(output_file, r#"<svg xmlns="http://www.w3.org/2000/svg"></svg>"#)
                .unwrap();
            return;
        }

        let centers: Vec<(f64, f64)> = active
            .iter()
            .map(|h| axial_to_pixel(h.position.x, h.position.y))
            .collect();

        let min_x = centers.iter().map(|(x, _)| *x).fold(f64::INFINITY, f64::min);
        let max_x = centers.iter().map(|(x, _)| *x).fold(f64::NEG_INFINITY, f64::max);
        let min_y = centers.iter().map(|(_, y)| *y).fold(f64::INFINITY, f64::min);
        let max_y = centers.iter().map(|(_, y)| *y).fold(f64::NEG_INFINITY, f64::max);

        let offset_x = -min_x + PADDING + HEX_SIZE;
        let offset_y = -min_y + PADDING + HEX_SIZE;
        let width = max_x - min_x + 2.0 * (PADDING + HEX_SIZE);
        let height = max_y - min_y + 2.0 * (PADDING + HEX_SIZE);

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

#[cfg(test)]
mod tests {
    use std::path::Path;

    #[test]
    fn test_board_construction() {
        for radius in 2..5 {
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
