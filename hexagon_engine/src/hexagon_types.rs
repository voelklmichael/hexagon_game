use derive_aliases::derive;

#[derive(..SerdeClone)]
pub struct HexagonId(u32);

#[derive(..SerdeClone)]
pub struct HexagonConnectorId(u32);

// coordinate position on a hexagon tile
// x is left to right, y is 120° to it.
#[derive(..SerdeClone)]
pub struct HexagonPosition {
    x: i32,
    y: i32,
}

#[derive(..SerdeClone)]
pub struct Hexagon {
    id: HexagonId,
    position: HexagonPosition,
    was_removed: bool,
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
    id: HexagonConnectorId,
    hexagon_a: HexagonId,
    hexagon_b: HexagonId,
    was_removed: bool,
}

#[derive(..SerdeClone)]
pub enum HexagonEdge {
    Top,
    TopRight,
    BottomRight,
    Bottom,
    BottomLeft,
    TopLeft,
}

#[derive(..SerdeClone)]
pub enum HexagonEdgeSub {
    Left,
    Right,
}

#[derive(..SerdeClone)]
pub struct HexagonConnectorPosition {
    hexagon: HexagonId,
    edge: HexagonEdge,
    sub: HexagonEdgeSub,
}

#[derive(..SerdeClone)]
pub struct HexagonConnectorDirect {
    id: HexagonConnectorId,
    connector_a: HexagonConnectorPosition,
    connector_b: HexagonConnectorPosition,
    was_removed: bool,
    weight: u32,
    kind: HexagonConnectorDirectKind,
}

#[derive(..SerdeClone)]
pub enum HexagonConnectorDirectKind {
    Edge2Edge,
    Teleporter,
    Outside,
}

#[derive(..SerdeClone)]
pub struct HexagonConnectorDeadEnd {
    id: HexagonConnectorId,
    hexagon_a: HexagonConnectorPosition,
    was_removed: bool,
}

#[derive(..SerdeClone)]
pub enum HexagonConnector {
    Direct(HexagonConnectorDirect),
    DeadEnd(HexagonConnectorDeadEnd),
}

#[derive(..SerdeClone)]
pub struct HexagonBoard {
    hexagons: Vec<Hexagon>,
    connectors: Vec<HexagonConnector>,
}
impl HexagonBoard {
    pub(crate) fn new(hexagons: Vec<Hexagon>, connectors: Vec<HexagonConnector>) -> Self {
        Self {
            hexagons,
            connectors,
        }
    }
}
