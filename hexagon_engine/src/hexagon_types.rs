use derive_aliases::derive;

#[derive(..SerdeClone)]
pub struct HexagonId(pub u32);

#[derive(..SerdeClone)]
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
#[derive(Copy, strum::VariantArray, strum::EnumIter)]
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
}

#[derive(..SerdeClone)]
#[derive(Copy, strum::VariantArray, strum::EnumIter)]
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
pub struct HexagonConnectorPosition {
    pub hexagon: HexagonId,
    pub edge_sub: HexagonSub,
}

pub struct HexagonEdgeSub {
    pub edge: HexagonEdge,
    pub sub: HexagonSub,
}

#[derive(..SerdeClone)]
pub struct HexagonConnectorDirect {
    pub id: HexagonConnectorId,
    pub connector_a: HexagonConnectorPosition,
    pub connector_b: HexagonConnectorPosition,
    pub was_removed: bool,
    pub weight: u32,
    pub kind: HexagonConnectorDirectKind,
}

#[derive(..SerdeClone)]
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
}

pub struct HexagonTile {
    inner_connectors: Vec<HexagonEdgeSub>,
}
