use derive_aliases::derive;

#[derive(..SerdeClone)]
#[derive(PartialEq)]
pub struct HexagonId(pub u32);

#[derive(..SerdeClone)]
#[derive(PartialEq)]
pub struct HexagonConnectorId(pub u32);

// coordinate position on a hexagon tile
// x is left to right, y is 120° to it.
#[derive(..SerdeClone)]
pub struct HexagonPosition {
    pub x: i32,
    pub y: i32,
}

#[derive(..SerdeClone)]
pub struct Hexagon {
    pub id: HexagonId,
    pub position: HexagonPosition,
    pub was_removed: bool,
}
impl Hexagon {
    pub(crate) fn new(id: u32, x: i32, y: i32) -> Self {
        Self {
            id: HexagonId(id),
            position: HexagonPosition { x, y },
            was_removed: false,
        }
    }
}

#[derive(..SerdeClone)]
pub struct Hexagon2HexagonConnector {
    pub id: HexagonConnectorId,
    pub hexagon_a: HexagonId,
    pub hexagon_b: HexagonId,
    pub was_removed: bool,
}

#[derive(..SerdeClone)]
#[derive(Copy, PartialEq, strum::VariantArray, strum::EnumIter)]
pub enum HexagonEdge {
    Top,
    TopRight,
    BottomRight,
    Bottom,
    BottomLeft,
    TopLeft,
}
impl HexagonEdge {
    pub fn invert(&self) -> Self {
        match self {
            Self::Top => Self::Bottom,
            Self::TopRight => Self::BottomLeft,
            Self::TopLeft => Self::BottomRight,
            Self::BottomLeft => Self::TopRight,
            Self::BottomRight => Self::TopLeft,
            Self::Bottom => Self::Top,
        }
    }

    fn rotate_cw(&self) -> Self {
        match self {
            Self::Top => Self::TopRight,
            Self::TopRight => Self::BottomRight,
            Self::BottomRight => Self::Bottom,
            Self::Bottom => Self::BottomLeft,
            Self::BottomLeft => Self::TopLeft,
            Self::TopLeft => Self::Top,
        }
    }

    fn rotate_ccw(&self) -> Self {
        match self {
            Self::Top => Self::TopLeft,
            Self::TopLeft => Self::BottomLeft,
            Self::BottomLeft => Self::Bottom,
            Self::Bottom => Self::BottomRight,
            Self::BottomRight => Self::TopRight,
            Self::TopRight => Self::Top,
        }
    }
}

#[derive(..SerdeClone)]
#[derive(Copy, PartialEq, strum::VariantArray, strum::EnumIter)]
pub enum HexagonSub {
    Left,
    Right,
}
impl HexagonSub {
    pub fn invert(&self) -> Self {
        match self {
            Self::Left => Self::Right,
            Self::Right => Self::Left,
        }
    }
}

#[derive(..SerdeClone)]
#[derive(PartialEq)]
pub struct HexagonConnectorPosition {
    pub hexagon: HexagonId,
    pub edge_sub: HexagonEdgeSub,
}

#[derive(..SerdeClone)]
#[derive(PartialEq)]
pub struct HexagonEdgeSub {
    pub edge: HexagonEdge,
    pub sub: HexagonSub,
}

#[derive(..SerdeClone)]
#[derive(PartialEq)]
pub struct HexagonConnectorDirect {
    pub id: HexagonConnectorId,
    pub connector_a: HexagonConnectorPosition,
    pub connector_b: HexagonConnectorPosition,
    pub was_removed: bool,
    pub weight: u32,
    pub kind: HexagonConnectorDirectKind,
}

#[derive(..SerdeClone)]
#[derive(PartialEq)]
pub enum HexagonConnectorDirectKind {
    Edge2Edge,
    Teleport,
    Outside,
    OnHex,
}

#[derive(..SerdeClone)]
pub struct HexagonConnectorDeadEnd {
    pub id: HexagonConnectorId,
    pub hexagon_a: HexagonConnectorPosition,
    pub was_removed: bool,
}

#[derive(..SerdeClone)]
pub enum HexagonConnector {
    Direct(HexagonConnectorDirect),
    DeadEnd(HexagonConnectorDeadEnd),
}

#[derive(..SerdeClone)]
pub struct HexagonBoard {
    pub hexagons: Vec<Hexagon>,
    pub connectors: Vec<HexagonConnector>,
}
impl HexagonBoard {
    pub(crate) fn new(hexagons: Vec<Hexagon>, connectors: Vec<HexagonConnector>) -> Self {
        Self {
            hexagons,
            connectors,
        }
    }
    pub fn get_dead_ends(&self) -> Vec<&HexagonConnectorDeadEnd> {
        self.connectors
            .iter()
            .filter_map(|c| match c {
                HexagonConnector::DeadEnd(d) if !d.was_removed => Some(d),
                _ => None,
            })
            .collect()
    }

    pub fn play_tile(&mut self, position: HexagonPosition, tile: HexagonTile) {
        let id = &self
            .hexagons
            .iter()
            .find(|x| !x.was_removed && x.position.x == position.x && x.position.y == position.y)
            .expect("No matching hexagon found")
            .id;

        let mut next_connector_id = self
            .connectors
            .iter()
            .map(|c| match c {
                HexagonConnector::Direct(d) => d.id.0,
                HexagonConnector::DeadEnd(d) => d.id.0,
            })
            .max()
            .unwrap_or(0)
            + 1;

        for (a, b) in tile.inner_connectors {
            self.connectors
                .push(HexagonConnector::Direct(HexagonConnectorDirect {
                    id: HexagonConnectorId(next_connector_id),
                    connector_a: HexagonConnectorPosition {
                        hexagon: HexagonId(id.0),
                        edge_sub: a,
                    },
                    connector_b: HexagonConnectorPosition {
                        hexagon: HexagonId(id.0),
                        edge_sub: b,
                    },
                    was_removed: false,
                    weight: 1,
                    kind: HexagonConnectorDirectKind::OnHex,
                }));
            next_connector_id += 1;
        }
    }
}

#[derive(..SerdeClone)]
pub struct HexagonTile {
    pub inner_connectors: Vec<(HexagonEdgeSub, HexagonEdgeSub)>,
}
impl HexagonTile {
    pub fn rotate_left(&mut self) {
        for (start, end) in &mut self.inner_connectors {
            start.edge = start.edge.rotate_ccw();
            end.edge = end.edge.rotate_ccw();
        }
    }

    pub fn rotate_right(&mut self) {
        for (start, end) in &mut self.inner_connectors {
            start.edge = start.edge.rotate_cw();
            end.edge = end.edge.rotate_cw();
        }
    }
}
