use serde::{Deserialize, Serialize};

use crate::{Hexagon, HexagonBoard};

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

        // Step 1: Validation
        if radius < 1 {
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

        // Step 2: Initialize collection
        let mut hexagons = Vec::new();
        let mut connectors = vec![];

        // Step 3: Branch based on Parity
        if radius % 2 == 1 {
            // Step 4a: Odd Diameter - Centered on tile (0,0)
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
            // Step 4b: Even Diameter - Centered on a corner (vertex)
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

        // Step 5: Finalize the board construction
        Ok(HexagonBoard::new(hexagons, connectors))
    }
}
