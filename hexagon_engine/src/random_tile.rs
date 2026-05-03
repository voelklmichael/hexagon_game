use strum::IntoEnumIterator;

use crate::{HexagonEdge, HexagonEdgeSub, HexagonSub, Rng};

pub fn random_tile_fully_connected(rng: &mut Rng) {
    let mut unused_subs = Vec::new();
    for edge in HexagonEdge::iter() {
        for sub in HexagonSub::iter() {
            unused_subs.push(HexagonEdgeSub { edge, sub });
        }
    }
    let mut connectors = Vec::new();
    while let Some(start) = unused_subs.pop() {
        let end = rng.select_random_element(&mut unused_subs);
        connectors.push((start, end));
    }
}
