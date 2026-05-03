use std::collections::HashMap;
use std::path::Path;

const HEX_SIZE: f64 = 40.0;
const PADDING: f64 = 50.0;
const ARROW_LEN: f64 = 22.0;
const PLAYER_COLORS: &[&str] = &["#1a6ef5", "#2db352", "#e8401c", "#9932cc", "#daa520"];

impl super::HexagonBoard {
    pub fn render(&self, output_file: &Path) {
        let svg = render_svg(self, |_, _, _| String::new());
        std::fs::write(output_file, svg).unwrap();
    }
}

impl super::game_state::GameState {
    pub fn render(&self, output_file: &Path) {
        let players = &self.players;
        let svg = render_svg(&self.board, |offset_x, offset_y, hex_map| {
            build_player_svg(players, offset_x, offset_y, hex_map)
        });
        std::fs::write(output_file, svg).unwrap();
    }
}

fn build_player_svg(
    players: &[super::game_state::Player],
    offset_x: f64,
    offset_y: f64,
    hex_map: &HashMap<u32, (f64, f64)>,
) -> String {
    let mut defs = String::from("  <defs>\n");
    let mut elements = String::new();

    for (i, player) in players.iter().enumerate() {
        let color = PLAYER_COLORS[i % PLAYER_COLORS.len()];

        defs.push_str(&format!(
            "    <marker id=\"arr-{i}\" markerWidth=\"10\" markerHeight=\"7\" \
             refX=\"10\" refY=\"3.5\" orient=\"auto\" markerUnits=\"userSpaceOnUse\">\
             <polygon points=\"10 3.5 0 0 0 7\" fill=\"{color}\"/></marker>\n"
        ));

        // Start arrow: tail outside the board, tip at the player's initial dead-end position
        if let Some(start_pos) = player_start_position(player) {
            if let Some(&(hx, hy)) = hex_map.get(&start_pos.hexagon.0) {
                let cx = hx + offset_x;
                let cy = hy + offset_y;
                let m = get_sub_edge_midpoint(cx, cy, &start_pos.edge_sub.edge, &start_pos.edge_sub.sub);
                let (ndx, ndy) = outward_dir(cx, cy, m);
                let tail_x = m.0 + ndx * ARROW_LEN;
                let tail_y = m.1 + ndy * ARROW_LEN;
                elements.push_str(&format!(
                    "  <line x1=\"{tail_x:.1}\" y1=\"{tail_y:.1}\" x2=\"{:.1}\" y2=\"{:.1}\" \
                     stroke=\"{color}\" stroke-width=\"3\" stroke-linecap=\"round\" \
                     marker-end=\"url(#arr-{i})\"/>\n",
                    m.0, m.1
                ));
            }
        }

        // Target arrow: tail at target position, tip pointing outward
        if let Some(target) = &player.target {
            if let Some(&(hx, hy)) = hex_map.get(&target.hexagon.0) {
                let cx = hx + offset_x;
                let cy = hy + offset_y;
                let m = get_sub_edge_midpoint(cx, cy, &target.edge_sub.edge, &target.edge_sub.sub);
                let (ndx, ndy) = outward_dir(cx, cy, m);
                let tip_x = m.0 + ndx * ARROW_LEN;
                let tip_y = m.1 + ndy * ARROW_LEN;
                elements.push_str(&format!(
                    "  <line x1=\"{:.1}\" y1=\"{:.1}\" x2=\"{tip_x:.1}\" y2=\"{tip_y:.1}\" \
                     stroke=\"{color}\" stroke-width=\"3\" stroke-dasharray=\"4,3\" \
                     stroke-linecap=\"round\" marker-end=\"url(#arr-{i})\"/>\n",
                    m.0, m.1
                ));
            }
        }
    }

    defs.push_str("  </defs>\n");
    defs + &elements
}

fn player_start_position(
    player: &super::game_state::Player,
) -> Option<&super::HexagonConnectorPosition> {
    let first_turn = player.history.first()?;
    match first_turn.history.first()? {
        super::HexagonConnector::DeadEnd(d) => Some(&d.hexagon_a),
        _ => None,
    }
}

fn outward_dir(cx: f64, cy: f64, m: (f64, f64)) -> (f64, f64) {
    let dx = m.0 - cx;
    let dy = m.1 - cy;
    let len = (dx * dx + dy * dy).sqrt();
    (dx / len, dy / len)
}

fn render_svg(
    board: &super::HexagonBoard,
    extra_fn: impl FnOnce(f64, f64, &HashMap<u32, (f64, f64)>) -> String,
) -> String {
    let active: Vec<_> = board.hexagons.iter().filter(|h| !h.was_removed).collect();

    if active.is_empty() {
        return "<svg xmlns=\"http://www.w3.org/2000/svg\"></svg>\n".to_string();
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

    let hex_map: HashMap<u32, (f64, f64)> = active
        .iter()
        .zip(centers.iter())
        .map(|(h, &cp)| (h.id.0, cp))
        .collect();

    let mut svg = format!(
        "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"{:.0}\" height=\"{:.0}\">\n",
        width, height
    );

    for (cx, cy) in &centers {
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

    for connector in &board.connectors {
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
                        &d.connector_a.edge_sub.edge,
                        &d.connector_a.edge_sub.sub,
                    );
                    let m2 = get_sub_edge_midpoint(
                        b_cx,
                        b_cy,
                        &d.connector_b.edge_sub.edge,
                        &d.connector_b.edge_sub.sub,
                    );

                    let x1 = a_cx + (m1.0 - a_cx) * 0.85;
                    let y1 = a_cy + (m1.1 - a_cy) * 0.85;
                    let x2 = b_cx + (m2.0 - b_cx) * 0.85;
                    let y2 = b_cy + (m2.1 - b_cy) * 0.85;

                    if matches!(d.kind, crate::HexagonConnectorDirectKind::Outside) {
                        let mid_x = (m1.0 + m2.0) / 2.0;
                        let mid_y = (m1.1 + m2.1) / 2.0;
                        let hex_mid_x = (a_cx + b_cx) / 2.0;
                        let hex_mid_y = (a_cy + b_cy) / 2.0;
                        let cp_x = mid_x + (mid_x - hex_mid_x) * 0.4;
                        let cp_y = mid_y + (mid_y - hex_mid_y) * 0.4;
                        svg.push_str(&format!(
                            "  <path d=\"M {x1:.1} {y1:.1} Q {cp_x:.1} {cp_y:.1} {x2:.1} {y2:.1}\" \
                             fill=\"none\" stroke=\"purple\" stroke-width=\"2\" stroke-linecap=\"round\"/>\n"
                        ));
                    } else {
                        svg.push_str(&format!(
                            "  <line x1=\"{x1:.1}\" y1=\"{y1:.1}\" x2=\"{x2:.1}\" y2=\"{y2:.1}\" \
                             stroke=\"#5a3e0a\" stroke-width=\"2\" stroke-linecap=\"round\"/>\n"
                        ));
                    }
                }
            }
            crate::HexagonConnector::DeadEnd(d) => {
                if d.was_removed {
                    continue;
                }
                if let Some(&a_pos) = hex_map.get(&d.hexagon_a.hexagon.0) {
                    let a_cx = a_pos.0 + offset_x;
                    let a_cy = a_pos.1 + offset_y;
                    let m = get_sub_edge_midpoint(
                        a_cx,
                        a_cy,
                        &d.hexagon_a.edge_sub.edge,
                        &d.hexagon_a.edge_sub.sub,
                    );
                    let x = m.0;
                    let y = m.1;
                    let size = 4.0;
                    svg.push_str(&format!(
                        "  <line x1=\"{:.1}\" y1=\"{:.1}\" x2=\"{:.1}\" y2=\"{:.1}\" \
                         stroke=\"#8b0000\" stroke-width=\"2\" stroke-linecap=\"round\"/>\n",
                        x - size,
                        y - size,
                        x + size,
                        y + size
                    ));
                    svg.push_str(&format!(
                        "  <line x1=\"{:.1}\" y1=\"{:.1}\" x2=\"{:.1}\" y2=\"{:.1}\" \
                         stroke=\"#8b0000\" stroke-width=\"2\" stroke-linecap=\"round\"/>\n",
                        x - size,
                        y + size,
                        x + size,
                        y - size
                    ));
                }
            }
        }
    }

    svg.push_str(&extra_fn(offset_x, offset_y, &hex_map));
    svg.push_str("</svg>\n");
    svg
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
    sub: &crate::HexagonSub,
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
        crate::HexagonSub::Left => ((v1.0 * 3.0 + v2.0) / 4.0, (v1.1 * 3.0 + v2.1) / 4.0),
        crate::HexagonSub::Right => ((v1.0 + v2.0 * 3.0) / 4.0, (v1.1 + v2.1 * 3.0) / 4.0),
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use crate::{
        HexagonPosition, game_state::DeliveryGameOptions, random_tile::random_tile_fully_connected,
    };

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

    #[test]
    fn test_board_with_tile() {
        let options = crate::BoardConstructionOptionsSimple {
            radius: 2,
            add_outer_connectors: true,
        };
        let mut board = options.construct().unwrap();
        let mut rng = crate::Rng::new(123);
        let tile = random_tile_fully_connected(&mut rng);
        board.play_tile(HexagonPosition { x: 0, y: 0 }, tile);
        let ouputfile = format!("{}/target/played_board.json", env!("CARGO_MANIFEST_DIR"));
        let path = Path::new(&ouputfile);
        let json = serde_json::to_string_pretty(&board).unwrap();
        std::fs::write(path, json).unwrap();

        let ouputfile = format!("{}/target/played_board.svg", env!("CARGO_MANIFEST_DIR"));
        let path = Path::new(&ouputfile);
        board.render(path);
    }

    #[test]
    fn test_game_with_tile() {
        let options = crate::BoardConstructionOptionsSimple {
            radius: 2,
            add_outer_connectors: true,
        };
        let options = DeliveryGameOptions {
            board_options: options,
            random_salt: 123,
            hand_size: 3,
            npc_count: 2,
            player_has_target: true,
            npcs_have_target: true,
        };
        let game = options.start_game();
        let ouputfile = format!("{}/target/played_board.json", env!("CARGO_MANIFEST_DIR"));
        let path = Path::new(&ouputfile);
        let json = serde_json::to_string_pretty(&game).unwrap();
        std::fs::write(path, json).unwrap();

        let ouputfile = format!("{}/target/game_state.svg", env!("CARGO_MANIFEST_DIR"));
        let path = Path::new(&ouputfile);
        game.render(path);
    }
}
