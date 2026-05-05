use serde::{Deserialize, Serialize};

mod board;
mod tile;

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

#[derive(Serialize, Deserialize)]
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

#[derive(Debug, Serialize, Deserialize)]
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

#[derive(Serialize, Deserialize)]
pub struct Tile {
    pub inner_connectors: Vec<ConnectorEdgeSub>,
}
