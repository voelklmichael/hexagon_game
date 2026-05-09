use strum::IntoEnumIterator;

use super::*;
use crate::random_number_generator::RandomNumberGenerator;

impl Tile {
    pub(crate) fn create_fully_connected(rng: &mut RandomNumberGenerator) -> Tile {
        let mut edges = Vec::new();
        for edge in Edge::iter() {
            for sub in Sub::iter() {
                edges.push(EdgeSub { edge, sub });
            }
        }
        let mut inner_connectors = Vec::new();
        while edges.len() > 1 {
            let start = edges.pop().unwrap();
            let end = rng.select_random_element(&mut edges).unwrap();
            inner_connectors.push(ConnectorEdgeSub { a: start, b: end });
        }
        assert!(edges.is_empty());

        Tile { inner_connectors }
    }
    pub fn rotate(&mut self, direction: TileRotationDirection) {
        self.inner_connectors.iter_mut().for_each(|x| {
            x.a.edge.rotate(direction);
            x.b.edge.rotate(direction);
        });
    }
}
