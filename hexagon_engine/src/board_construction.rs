use serde::{Deserialize, Serialize};

use crate::{Hexagon, HexagonBoard, Rng};

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
        if radius <= 1 {
            return Err(format!("Radius must be at least 2, received: {radius}"));
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

        let mut hexagons = [Hexagon::new(id_generator.next(), 0, 0)].to_vec();
        let mut connectors = vec![];

        // For each radius from 1 up to the requested radius - 1
        for r in 1..radius {
            let mut x = 0;
            let mut y = -(r as i32);

            // Directions to traverse a ring in axial coordinates
            let directions = [(1, 0), (0, 1), (-1, 1), (-1, 0), (0, -1), (1, -1)];
            for (dx, dy) in directions {
                for _ in 0..r {
                    x += dx;
                    y += dy;
                    hexagons.push(Hexagon::new(id_generator.next(), x, y));
                }
            }
        }

        Ok(HexagonBoard::new(hexagons, connectors))
    }
}
