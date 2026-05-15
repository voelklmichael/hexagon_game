use std::collections::HashSet;

use serde::{Deserialize, Serialize};

use crate::game_options::OuterConnectors;
use crate::rng::RandomNumberGenerator;

#[derive(Clone, Copy, Debug, strum::EnumIter, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Edge {
    Top,
    TopLeft,
    BottomLeft,
    Bottom,
    BottomRight,
    TopRight,
}

#[derive(Clone, Copy, Debug, strum::EnumIter, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Sub {
    Left,
    Right,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct EdgeSub {
    pub edge: Edge,
    pub sub: Sub,
}

#[derive(Clone, Copy, Serialize, Deserialize)]
pub enum TileRotationDirection {
    Clockwise,
    CounterClockwise,
}

impl Edge {
    pub fn rotate(&mut self, direction: TileRotationDirection) {
        *self = match direction {
            TileRotationDirection::Clockwise => match self {
                Edge::Top => Edge::TopRight,
                Edge::TopRight => Edge::BottomRight,
                Edge::BottomRight => Edge::Bottom,
                Edge::Bottom => Edge::BottomLeft,
                Edge::BottomLeft => Edge::TopLeft,
                Edge::TopLeft => Edge::Top,
            },
            TileRotationDirection::CounterClockwise => match self {
                Edge::Top => Edge::TopLeft,
                Edge::TopLeft => Edge::BottomLeft,
                Edge::BottomLeft => Edge::Bottom,
                Edge::Bottom => Edge::BottomRight,
                Edge::BottomRight => Edge::TopRight,
                Edge::TopRight => Edge::Top,
            },
        };
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct HexagonPosition {
    pub x: i32,
    pub y: i32,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ConnectorPosition {
    pub hexagon: HexagonPosition,
    pub edge_sub: EdgeSub,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Board {
    pub hexagons: Vec<HexagonPosition>,
    pub connectors: Vec<Connector>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ConnectorId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ConnectorEnd {
    StartedAtA,
    StartedAtB,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Connector {
    pub id: ConnectorId,
    pub kind: ConnectorKind,
    pub weight: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ConnectorKind {
    DeadEnd(ConnectorDeadEnd),
    HexToHex(ConnectorOutside),
    OnHex(ConnectorOnHex),
    Outside(ConnectorOutside),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConnectorDeadEnd {
    pub position: ConnectorPosition,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConnectorOnHex {
    pub hexagon: HexagonPosition,
    pub edge_sub: ConnectorEdgeSub,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConnectorEdgeSub {
    pub a: EdgeSub,
    pub b: EdgeSub,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConnectorOutside {
    pub connector_a: ConnectorPosition,
    pub connector_b: ConnectorPosition,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Tile {
    pub inner_connectors: Vec<ConnectorEdgeSub>,
}

impl Tile {
    pub fn create_fully_connected(rng: &mut RandomNumberGenerator) -> Self {
        use strum::IntoEnumIterator;
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
        Self { inner_connectors }
    }

    pub fn rotate(&mut self, direction: TileRotationDirection) {
        self.inner_connectors.iter_mut().for_each(|x| {
            x.a.edge.rotate(direction);
            x.b.edge.rotate(direction);
        });
    }
}

impl Board {
    pub fn create_board(radius: usize, outer_connectors: OuterConnectors) -> Result<Self, String> {
        if radius == 0 {
            return Err("Radius must be at least 1".to_string());
        }
        let hexagons = Self::generate_hexagons(radius);
        let hex_set: HashSet<(i32, i32)> = hexagons.iter().map(|h| (h.x, h.y)).collect();
        let outside_pairs: Vec<(ConnectorPosition, ConnectorPosition)> = match outer_connectors {
            OuterConnectors::OnlyDeathEnds => vec![],
            OuterConnectors::ReducedDeathEnds => Self::compute_outside_pairs(&hex_set, &hexagons),
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
                weight: 500,
            });
            id += 1;
        }

        for hex_a in &hexagons {
            let pa = (hex_a.x, hex_a.y);
            for (edge_e, hex_b) in Self::neighbors(hex_a) {
                let pb = (hex_b.x, hex_b.y);
                if !hex_set.contains(&pb) || pa >= pb {
                    continue;
                }
                let opp = Self::opposite_edge(edge_e);
                for (sub_a, sub_b) in [(Sub::Left, Sub::Right), (Sub::Right, Sub::Left)] {
                    connectors.push(Connector {
                        id: ConnectorId(id),
                        kind: ConnectorKind::HexToHex(ConnectorOutside {
                            connector_a: ConnectorPosition {
                                hexagon: *hex_a,
                                edge_sub: EdgeSub { edge: edge_e, sub: sub_a },
                            },
                            connector_b: ConnectorPosition {
                                hexagon: hex_b,
                                edge_sub: EdgeSub { edge: opp, sub: sub_b },
                            },
                        }),
                        weight: 1,
                    });
                    id += 1;
                }
            }
        }

        Ok(Board { hexagons, connectors })
    }

    pub fn get_dead_ends(&self) -> Vec<ConnectorId> {
        self.connectors
            .iter()
            .filter_map(|c| {
                if let ConnectorKind::DeadEnd(_) = &c.kind {
                    Some(c.id)
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn position_of(&self, id: ConnectorId, end: ConnectorEnd) -> ConnectorPosition {
        let connector = self
            .connectors
            .iter()
            .find(|c| c.id == id)
            .expect("connector not found");
        match &connector.kind {
            ConnectorKind::DeadEnd(c) => c.position.clone(),
            ConnectorKind::OnHex(c) => ConnectorPosition {
                hexagon: c.hexagon,
                edge_sub: match end {
                    ConnectorEnd::StartedAtA => c.edge_sub.a.clone(),
                    ConnectorEnd::StartedAtB => c.edge_sub.b.clone(),
                },
            },
            ConnectorKind::Outside(c) | ConnectorKind::HexToHex(c) => match end {
                ConnectorEnd::StartedAtA => c.connector_a.clone(),
                ConnectorEnd::StartedAtB => c.connector_b.clone(),
            },
        }
    }

    pub fn compute_path_starting_from(
        &self,
        current_position: &(ConnectorId, ConnectorEnd),
    ) -> Vec<(ConnectorId, ConnectorEnd, u32)> {
        let mut current_position = self.position_of(current_position.0, current_position.1);
        let mut steps = vec![];
        if let Some((next, end, id, weight)) = self.connectors.iter().find_map(|x| match &x.kind {
            ConnectorKind::OnHex(c) if c.hexagon == current_position.hexagon => {
                if c.edge_sub.a == current_position.edge_sub {
                    Some((&c.edge_sub.b, ConnectorEnd::StartedAtA, x.id, x.weight))
                } else if c.edge_sub.b == current_position.edge_sub {
                    Some((&c.edge_sub.a, ConnectorEnd::StartedAtB, x.id, x.weight))
                } else {
                    None
                }
            }
            _ => None,
        }) {
            steps.push((id, end, weight));
            current_position = ConnectorPosition {
                hexagon: current_position.hexagon,
                edge_sub: next.clone(),
            };
        } else {
            return steps;
        };
        while let Some((next, end, id, weight)) = self
            .connectors
            .iter()
            .filter(|x| !steps.iter().any(|(id, _, _)| id == &x.id))
            .find_map(|x| match &x.kind {
                ConnectorKind::OnHex(c) if c.hexagon == current_position.hexagon => {
                    if c.edge_sub.a == current_position.edge_sub {
                        Some((
                            Some(ConnectorPosition {
                                hexagon: current_position.hexagon,
                                edge_sub: c.edge_sub.b.clone(),
                            }),
                            ConnectorEnd::StartedAtA,
                            x.id,
                            x.weight,
                        ))
                    } else if c.edge_sub.b == current_position.edge_sub {
                        Some((
                            Some(ConnectorPosition {
                                hexagon: current_position.hexagon,
                                edge_sub: c.edge_sub.a.clone(),
                            }),
                            ConnectorEnd::StartedAtB,
                            x.id,
                            x.weight,
                        ))
                    } else {
                        None
                    }
                }
                ConnectorKind::DeadEnd(c) if c.position == current_position => {
                    Some((None, ConnectorEnd::StartedAtA, x.id, x.weight))
                }
                ConnectorKind::Outside(c) if c.connector_a == current_position => Some((
                    Some(c.connector_b.clone()),
                    ConnectorEnd::StartedAtA,
                    x.id,
                    x.weight,
                )),
                ConnectorKind::Outside(c) if c.connector_b == current_position => Some((
                    Some(c.connector_a.clone()),
                    ConnectorEnd::StartedAtB,
                    x.id,
                    x.weight,
                )),
                ConnectorKind::HexToHex(c) if c.connector_a == current_position => Some((
                    Some(c.connector_b.clone()),
                    ConnectorEnd::StartedAtA,
                    x.id,
                    x.weight,
                )),
                ConnectorKind::HexToHex(c) if c.connector_b == current_position => Some((
                    Some(c.connector_a.clone()),
                    ConnectorEnd::StartedAtB,
                    x.id,
                    x.weight,
                )),
                _ => None,
            })
        {
            steps.push((id, end, weight));
            if let Some(next) = next {
                current_position = next;
            }
        }
        steps
    }

    fn generate_hexagons(radius: usize) -> Vec<HexagonPosition> {
        let mut hexagons = Vec::new();
        if radius % 2 == 1 {
            let rings = (radius / 2) as i32;
            for q in -rings..=rings {
                let r_min = (-rings).max(-q - rings);
                let r_max = rings.min(-q + rings);
                for r in r_min..=r_max {
                    hexagons.push(HexagonPosition { x: q, y: r });
                }
            }
        } else {
            let n = (radius / 2) as i32;
            for q in 0..=n {
                for r in 0..=(n - q) {
                    hexagons.push(HexagonPosition { x: q, y: r });
                }
            }
        }
        hexagons
    }

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
                let (prev_a, start_b) = Self::adjacent_at_start(edge_e);
                if !hex_set.contains(&Self::neighbor_dir(pa, prev_a))
                    && !hex_set.contains(&Self::neighbor_dir(pb, start_b))
                {
                    pairs.push((
                        ConnectorPosition { hexagon: *hex_a, edge_sub: EdgeSub { edge: prev_a, sub: Sub::Right } },
                        ConnectorPosition { hexagon: hex_b,  edge_sub: EdgeSub { edge: start_b, sub: Sub::Left } },
                    ));
                }
                let (next_a, end_b) = Self::adjacent_at_end(edge_e);
                if !hex_set.contains(&Self::neighbor_dir(pa, next_a))
                    && !hex_set.contains(&Self::neighbor_dir(pb, end_b))
                {
                    pairs.push((
                        ConnectorPosition { hexagon: *hex_a, edge_sub: EdgeSub { edge: next_a, sub: Sub::Left } },
                        ConnectorPosition { hexagon: hex_b,  edge_sub: EdgeSub { edge: end_b,  sub: Sub::Right } },
                    ));
                }
            }
        }
        pairs
    }

    fn adjacent_at_start(e: Edge) -> (Edge, Edge) {
        match e {
            Edge::Top => (Edge::TopRight, Edge::BottomRight),
            Edge::TopLeft => (Edge::Top, Edge::TopRight),
            Edge::BottomLeft => (Edge::TopLeft, Edge::Top),
            Edge::Bottom => (Edge::BottomLeft, Edge::TopLeft),
            Edge::BottomRight => (Edge::Bottom, Edge::BottomLeft),
            Edge::TopRight => (Edge::BottomRight, Edge::Bottom),
        }
    }

    fn adjacent_at_end(e: Edge) -> (Edge, Edge) {
        match e {
            Edge::Top => (Edge::TopLeft, Edge::BottomLeft),
            Edge::TopLeft => (Edge::BottomLeft, Edge::Bottom),
            Edge::BottomLeft => (Edge::Bottom, Edge::BottomRight),
            Edge::Bottom => (Edge::BottomRight, Edge::TopRight),
            Edge::BottomRight => (Edge::TopRight, Edge::Top),
            Edge::TopRight => (Edge::Top, Edge::TopLeft),
        }
    }

    fn opposite_edge(e: Edge) -> Edge {
        match e {
            Edge::Top => Edge::Bottom,
            Edge::Bottom => Edge::Top,
            Edge::TopLeft => Edge::BottomRight,
            Edge::BottomRight => Edge::TopLeft,
            Edge::BottomLeft => Edge::TopRight,
            Edge::TopRight => Edge::BottomLeft,
        }
    }

    fn neighbor_dir(pos: (i32, i32), edge: Edge) -> (i32, i32) {
        let (q, r) = pos;
        match edge {
            Edge::Top => (q, r - 1),
            Edge::TopLeft => (q - 1, r),
            Edge::BottomLeft => (q - 1, r + 1),
            Edge::Bottom => (q, r + 1),
            Edge::BottomRight => (q + 1, r),
            Edge::TopRight => (q + 1, r - 1),
        }
    }

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
        Edge::Top => 0,
        Edge::TopLeft => 1,
        Edge::BottomLeft => 2,
        Edge::Bottom => 3,
        Edge::BottomRight => 4,
        Edge::TopRight => 5,
    }
}

fn sub_idx(sub: Sub) -> u8 {
    match sub {
        Sub::Left => 0,
        Sub::Right => 1,
    }
}
