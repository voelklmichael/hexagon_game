use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::{
    Hexagon, HexagonBoard, HexagonConnector, HexagonConnectorDeadEnd, HexagonConnectorDirect,
    HexagonConnectorDirectKind, HexagonConnectorId, HexagonConnectorPosition, HexagonEdge,
    HexagonEdgeSub, HexagonId, HexagonSub,
};

#[derive(Deserialize, Serialize)]
pub struct BoardConstructionOptionsSimple {
    pub radius: u32,
    pub add_outer_connectors: bool,
}

impl BoardConstructionOptionsSimple {
    pub fn construct(self) -> Result<HexagonBoard, String> {
        let Self {
            radius,
            add_outer_connectors,
        } = self;

        if radius == 0 {
            return Err(format!(
                "Board diameter (radius) must be >= 1, received: {radius}"
            ));
        }

        #[derive(Default)]
        struct IdGenerator {
            current: u32,
        }
        impl IdGenerator {
            fn next(&mut self) -> u32 {
                self.current += 1;
                self.current
            }
        }
        let mut id_generator = IdGenerator::default();

        let hexagons = {
            let mut hexagons = Vec::new();
            if radius % 2 == 1 {
                // We use the efficient ring-walking approach.
                hexagons.push(Hexagon::new(id_generator.next(), 0, 0));
                let ring_limit = (radius + 1) / 2;

                for r in 1..ring_limit {
                    let mut x = 0;
                    let mut y = -(r as i32);
                    let directions = [(1, 0), (0, 1), (-1, 1), (-1, 0), (0, -1), (1, -1)];
                    for (dx, dy) in directions {
                        for _ in 0..r {
                            x += dx;
                            y += dy;
                            hexagons.push(Hexagon::new(id_generator.next(), x, y));
                        }
                    }
                }
            } else {
                // An even diameter D=2k has k tiles from the center to the edge.
                // We define a range that captures the 3-tile junction at the center.
                let k = (radius / 2) as i32;

                // We iterate through the bounding box of the hexagon and filter by cube constraints
                // for a corner-centered symmetry: q in [1-k, k], r in [-k, k-1], s in [1-k, k]
                for q in (1 - k)..=k {
                    for r in (-k)..=(k - 1) {
                        let s = -q - r;
                        if s >= (1 - k) && s <= k {
                            hexagons.push(Hexagon::new(id_generator.next(), q, r));
                        }
                    }
                }
            }
            hexagons
        };
        let mut connectors = vec![];

        let pos_map: HashMap<(i32, i32), u32> = hexagons
            .iter()
            .map(|h| ((h.position.x, h.position.y), h.id.0))
            .collect();

        use HexagonEdge as HE;
        let directions = [
            ((1_i32, 0_i32), HE::BottomRight),
            ((0, 1), HE::Bottom),
            ((-1, 1), HE::BottomLeft),
            ((-1, 0), HE::TopLeft),
            ((0, -1), HE::Top),
            ((1, -1), HE::TopRight),
        ];

        // (hexagon_id, dir_idx, sub_idx) for every position covered by a Direct connector
        let mut used_positions = std::collections::HashSet::new();
        // add direct edge2edge connectors
        for h in &hexagons {
            for (dir_idx, ((dx, dy), edge_a)) in directions.iter().enumerate() {
                let edge_b = edge_a.invert();
                let nx = h.position.x + dx;
                let ny = h.position.y + dy;
                if let Some(&neighbor_id) = pos_map.get(&(nx, ny)) {
                    if neighbor_id > h.id.0 {
                        let opp_dir_idx = (dir_idx + 3) % 6;
                        for (sub_idx, sub) in [HexagonSub::Left, HexagonSub::Right]
                            .into_iter()
                            .enumerate()
                        {
                            used_positions.insert((h.id.0, dir_idx, sub_idx));
                            used_positions.insert((neighbor_id, opp_dir_idx, 1 - sub_idx));
                            connectors.push(HexagonConnector::Direct(HexagonConnectorDirect {
                                id: HexagonConnectorId(id_generator.next()),
                                connector_a: HexagonConnectorPosition {
                                    hexagon: HexagonId(h.id.0),
                                    edge_sub: HexagonEdgeSub { edge: edge_a.clone(), sub: sub.clone() },
                                },
                                connector_b: HexagonConnectorPosition {
                                    hexagon: HexagonId(neighbor_id),
                                    edge_sub: HexagonEdgeSub { edge: edge_b.clone(), sub: sub.invert() },
                                },
                                was_removed: false,
                                weight: 1,
                                kind: HexagonConnectorDirectKind::Edge2Edge,
                            }));
                        }
                    }
                }
            }
        }

        if add_outer_connectors {
            for h in &hexagons {
                for dir_idx in 0..6_usize {
                    let next_dir_idx = (dir_idx + 1) % 6;
                    let dx = &directions[dir_idx].0;
                    let next_dx = &directions[next_dir_idx].0;

                    let current_neighbor = pos_map
                        .get(&(h.position.x + dx.0, h.position.y + dx.1))
                        .copied();
                    let next_neighbor = pos_map
                        .get(&(h.position.x + next_dx.0, h.position.y + next_dx.1))
                        .copied();

                    // left endpoint (used → unused): h's Left sub of next_dir_idx
                    // connects to neighbor's Right sub of (dir_idx+2)%6
                    if let (Some(n_id), None) = (current_neighbor, next_neighbor) {
                        if h.id.0 < n_id {
                            let n_dir = (dir_idx + 2) % 6;
                            used_positions.insert((h.id.0, next_dir_idx, 0));
                            used_positions.insert((n_id, n_dir, 1));
                            connectors.push(HexagonConnector::Direct(HexagonConnectorDirect {
                                id: HexagonConnectorId(id_generator.next()),
                                connector_a: HexagonConnectorPosition {
                                    hexagon: HexagonId(h.id.0),
                                    edge_sub: HexagonEdgeSub { edge: directions[next_dir_idx].1.clone(), sub: HexagonSub::Left },
                                },
                                connector_b: HexagonConnectorPosition {
                                    hexagon: HexagonId(n_id),
                                    edge_sub: HexagonEdgeSub { edge: directions[n_dir].1.clone(), sub: HexagonSub::Right },
                                },
                                was_removed: false,
                                weight: 1,
                                kind: HexagonConnectorDirectKind::Outside,
                            }));
                        }
                    }

                    // right endpoint (unused → used): h's Right sub of dir_idx
                    // connects to neighbor's Left sub of (dir_idx+5)%6
                    if let (None, Some(n_id)) = (current_neighbor, next_neighbor) {
                        if h.id.0 < n_id {
                            let n_dir = (dir_idx + 5) % 6;
                            used_positions.insert((h.id.0, dir_idx, 1));
                            used_positions.insert((n_id, n_dir, 0));
                            connectors.push(HexagonConnector::Direct(HexagonConnectorDirect {
                                id: HexagonConnectorId(id_generator.next()),
                                connector_a: HexagonConnectorPosition {
                                    hexagon: HexagonId(h.id.0),
                                    edge_sub: HexagonEdgeSub { edge: directions[dir_idx].1.clone(), sub: HexagonSub::Right },
                                },
                                connector_b: HexagonConnectorPosition {
                                    hexagon: HexagonId(n_id),
                                    edge_sub: HexagonEdgeSub { edge: directions[n_dir].1.clone(), sub: HexagonSub::Left },
                                },
                                was_removed: false,
                                weight: 1,
                                kind: HexagonConnectorDirectKind::Outside,
                            }));
                        }
                    }
                }
            }
        }

        // add dead ends
        for h in &hexagons {
            for (dir_idx, (_, edge)) in directions.iter().enumerate() {
                for sub_idx in 0..2_usize {
                    if !used_positions.contains(&(h.id.0, dir_idx, sub_idx)) {
                        let sub = if sub_idx == 0 {
                            HexagonSub::Left
                        } else {
                            HexagonSub::Right
                        };
                        connectors.push(HexagonConnector::DeadEnd(HexagonConnectorDeadEnd {
                            id: HexagonConnectorId(id_generator.next()),
                            hexagon_a: HexagonConnectorPosition {
                                hexagon: HexagonId(h.id.0),
                                edge_sub: HexagonEdgeSub { edge: edge.clone(), sub },
                            },
                            was_removed: false,
                        }));
                    }
                }
            }
        }

        Ok(HexagonBoard::new(hexagons, connectors))
    }
}
