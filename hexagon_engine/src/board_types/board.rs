use std::collections::HashSet;

use crate::game_options::OuterConnectors;

use super::*;

impl Board {
    pub fn create_board(radius: usize, outer_connectors: OuterConnectors) -> Result<Self, String> {
        if radius == 0 {
            return Err("Radius must be at least 1".to_string());
        }

        let hexagons = Self::generate_hexagons(radius);
        let hex_set: HashSet<(i32, i32)> = hexagons.iter().map(|h| (h.x, h.y)).collect();

        // For ReducedDeathEnds: compute which outer positions get outside connectors.
        // Those positions are NOT dead ends.
        let outside_pairs: Vec<(ConnectorPosition, ConnectorPosition)> =
            match outer_connectors {
                OuterConnectors::OnlyDeathEnds => vec![],
                OuterConnectors::ReducedDeathEnds => {
                    Self::compute_outside_pairs(&hex_set, &hexagons)
                }
            };

        let outside_keys: HashSet<(i32, i32, u8, u8)> = outside_pairs
            .iter()
            .flat_map(|(a, b)| {
                [
                    (a.hexagon.x, a.hexagon.y, edge_idx(a.edge_sub.edge), sub_idx(a.edge_sub.sub)),
                    (b.hexagon.x, b.hexagon.y, edge_idx(b.edge_sub.edge), sub_idx(b.edge_sub.sub)),
                ]
            })
            .collect();

        let mut connectors = Vec::new();
        let mut id = 0u32;

        for hex in &hexagons {
            for (edge, neighbor) in Self::neighbors(hex) {
                if !hex_set.contains(&(neighbor.x, neighbor.y)) {
                    for sub in [Sub::Left, Sub::Right] {
                        if outside_keys.contains(&(hex.x, hex.y, edge_idx(edge), sub_idx(sub))) {
                            continue;
                        }
                        connectors.push(Connector {
                            id: ConnectorId(id),
                            kind: ConnectorKind::DeadEnd(ConnectorDeadEnd {
                                position: ConnectorPosition {
                                    hexagon: *hex,
                                    edge_sub: EdgeSub { edge, sub },
                                },
                            }),
                            weight: 1,
                        });
                        id += 1;
                    }
                }
            }
        }

        for (pos_a, pos_b) in outside_pairs {
            connectors.push(Connector {
                id: ConnectorId(id),
                kind: ConnectorKind::Outside(ConnectorOutside {
                    connector_a: pos_a,
                    connector_b: pos_b,
                }),
                weight: 1,
            });
            id += 1;
        }

        Ok(Board { hexagons, connectors })
    }

    pub fn get_dead_ends(&self) -> Vec<(&ConnectorPosition, &ConnectorId)> {
        self.connectors
            .iter()
            .filter_map(|c| {
                if let ConnectorKind::DeadEnd(d) = &c.kind {
                    Some((&d.position, &c.id))
                } else {
                    None
                }
            })
            .collect()
    }

    fn generate_hexagons(radius: usize) -> Vec<HexagonPosition> {
        let mut hexagons = Vec::new();
        if radius % 2 == 1 {
            // Odd radius: regular hexagonal board centered on a hex.
            // rings = (radius-1)/2; iterate with r bounds that follow the 60° hex axes.
            let rings = (radius / 2) as i32;
            for q in -rings..=rings {
                let r_min = (-rings).max(-q - rings);
                let r_max = rings.min(-q + rings);
                for r in r_min..=r_max {
                    hexagons.push(HexagonPosition { x: q, y: r });
                }
            }
        } else {
            // Even radius: equilateral-triangle shaped board.
            // n = radius/2; each edge spans n steps → (n+1)(n+2)/2 hexes total.
            // Constraint in axial coords: q >= 0, r >= 0, q+r <= n.
            let n = (radius / 2) as i32;
            for q in 0..=n {
                for r in 0..=(n - q) {
                    hexagons.push(HexagonPosition { x: q, y: r });
                }
            }
        }
        hexagons
    }

    // For each inner edge E between board hexes A and B, at the two shared boundary
    // vertices, connect the closest outer sub-positions via a ConnectorOutside.
    //
    // At the START vertex of E: Right-sub-of-prev(E,A) ↔ Left-sub-of-adj_start(E,B)
    // At the END   vertex of E: Left-sub-of-next(E,A)  ↔ Right-sub-of-adj_end(E,B)
    //
    // Only process each inner edge once (pa < pb lexicographically).
    fn compute_outside_pairs(
        hex_set: &HashSet<(i32, i32)>,
        hexagons: &[HexagonPosition],
    ) -> Vec<(ConnectorPosition, ConnectorPosition)> {
        let mut pairs = Vec::new();

        for hex_a in hexagons {
            let pa = (hex_a.x, hex_a.y);
            for (edge_e, hex_b) in Self::neighbors(hex_a) {
                let pb = (hex_b.x, hex_b.y);
                if !hex_set.contains(&pb) || pa >= pb {
                    continue;
                }

                // START vertex of edge_e
                let (prev_a, start_b) = Self::adjacent_at_start(edge_e);
                if !hex_set.contains(&Self::neighbor_dir(pa, prev_a))
                    && !hex_set.contains(&Self::neighbor_dir(pb, start_b))
                {
                    pairs.push((
                        ConnectorPosition {
                            hexagon: *hex_a,
                            edge_sub: EdgeSub { edge: prev_a, sub: Sub::Right },
                        },
                        ConnectorPosition {
                            hexagon: hex_b,
                            edge_sub: EdgeSub { edge: start_b, sub: Sub::Left },
                        },
                    ));
                }

                // END vertex of edge_e
                let (next_a, end_b) = Self::adjacent_at_end(edge_e);
                if !hex_set.contains(&Self::neighbor_dir(pa, next_a))
                    && !hex_set.contains(&Self::neighbor_dir(pb, end_b))
                {
                    pairs.push((
                        ConnectorPosition {
                            hexagon: *hex_a,
                            edge_sub: EdgeSub { edge: next_a, sub: Sub::Left },
                        },
                        ConnectorPosition {
                            hexagon: hex_b,
                            edge_sub: EdgeSub { edge: end_b, sub: Sub::Right },
                        },
                    ));
                }
            }
        }

        pairs
    }

    // (prev edge of A, adjacent-start edge of B) at the START vertex of edge E.
    // Derived from the flat-top axial vertex geometry.
    fn adjacent_at_start(e: Edge) -> (Edge, Edge) {
        match e {
            Edge::Top         => (Edge::TopRight,   Edge::BottomRight),
            Edge::TopLeft     => (Edge::Top,         Edge::TopRight),
            Edge::BottomLeft  => (Edge::TopLeft,     Edge::Top),
            Edge::Bottom      => (Edge::BottomLeft,  Edge::TopLeft),
            Edge::BottomRight => (Edge::Bottom,      Edge::BottomLeft),
            Edge::TopRight    => (Edge::BottomRight, Edge::Bottom),
        }
    }

    // (next edge of A, adjacent-end edge of B) at the END vertex of edge E.
    fn adjacent_at_end(e: Edge) -> (Edge, Edge) {
        match e {
            Edge::Top         => (Edge::TopLeft,    Edge::BottomLeft),
            Edge::TopLeft     => (Edge::BottomLeft, Edge::Bottom),
            Edge::BottomLeft  => (Edge::Bottom,     Edge::BottomRight),
            Edge::Bottom      => (Edge::BottomRight, Edge::TopRight),
            Edge::BottomRight => (Edge::TopRight,   Edge::Top),
            Edge::TopRight    => (Edge::Top,        Edge::TopLeft),
        }
    }

    fn neighbor_dir(pos: (i32, i32), edge: Edge) -> (i32, i32) {
        let (q, r) = pos;
        match edge {
            Edge::Top         => (q,     r - 1),
            Edge::TopLeft     => (q - 1, r),
            Edge::BottomLeft  => (q - 1, r + 1),
            Edge::Bottom      => (q,     r + 1),
            Edge::BottomRight => (q + 1, r),
            Edge::TopRight    => (q + 1, r - 1),
        }
    }

    // flat-top hexagon neighbor directions in axial (q, r) coordinates
    fn neighbors(pos: &HexagonPosition) -> [(Edge, HexagonPosition); 6] {
        let (q, r) = (pos.x, pos.y);
        [
            (Edge::Top,         HexagonPosition { x: q,     y: r - 1 }),
            (Edge::TopLeft,     HexagonPosition { x: q - 1, y: r     }),
            (Edge::BottomLeft,  HexagonPosition { x: q - 1, y: r + 1 }),
            (Edge::Bottom,      HexagonPosition { x: q,     y: r + 1 }),
            (Edge::BottomRight, HexagonPosition { x: q + 1, y: r     }),
            (Edge::TopRight,    HexagonPosition { x: q + 1, y: r - 1 }),
        ]
    }
}

fn edge_idx(edge: Edge) -> u8 {
    match edge {
        Edge::Top => 0, Edge::TopLeft => 1, Edge::BottomLeft => 2,
        Edge::Bottom => 3, Edge::BottomRight => 4, Edge::TopRight => 5,
    }
}

fn sub_idx(sub: Sub) -> u8 {
    match sub { Sub::Left => 0, Sub::Right => 1 }
}
