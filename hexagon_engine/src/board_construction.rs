use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::{
    Hexagon, HexagonBoard, HexagonConnector, HexagonConnectorDirect, HexagonConnectorDirectKind,
    HexagonConnectorId, HexagonConnectorPosition, HexagonEdge, HexagonEdgeSub, HexagonId,
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
            add_outer_connectors: _,
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

        for h in &hexagons {
            for ((dx, dy), edge_a) in &directions {
                let edge_b = edge_a.invert();
                let nx = h.position.x + dx;
                let ny = h.position.y + dy;
                if let Some(&neighbor_id) = pos_map.get(&(nx, ny)) {
                    if neighbor_id > h.id.0 {
                        for sub in [HexagonEdgeSub::Left, HexagonEdgeSub::Right] {
                            connectors.push(HexagonConnector::Direct(HexagonConnectorDirect {
                                id: HexagonConnectorId(id_generator.next()),
                                connector_a: HexagonConnectorPosition {
                                    hexagon: HexagonId(h.id.0),
                                    edge: edge_a.clone(),
                                    sub: sub.clone(),
                                },
                                connector_b: HexagonConnectorPosition {
                                    hexagon: HexagonId(neighbor_id),
                                    edge: edge_b.clone(),
                                    sub: sub.invert(),
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

        Ok(HexagonBoard::new(hexagons, connectors))
    }
}
