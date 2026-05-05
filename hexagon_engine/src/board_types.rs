use serde::de;

mod board;
mod tile;

#[derive(Clone, Copy, Debug, strum::EnumIter, PartialEq)]
pub enum Edge {
    Top,
    TopLeft,
    BottomLeft,
    Bottom,
    BottomRight,
    TopRight,
}

#[derive(Clone, Copy, Debug, strum::EnumIter, PartialEq)]
pub enum Sub {
    Left,
    Right,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EdgeSub {
    pub edge: Edge,
    pub sub: Sub,
}

#[derive(Clone, Copy)]
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct HexagonPosition {
    pub x: i32,
    pub y: i32,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConnectorPosition {
    pub hexagon: HexagonPosition,
    pub edge_sub: EdgeSub,
}

pub struct Board {
    pub hexagons: Vec<HexagonPosition>,
    pub connectors: Vec<Connector>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ConnectorId(pub u32);

#[derive(Debug)]
pub struct Connector {
    pub id: ConnectorId,
    pub kind: ConnectorKind,
    pub weight: u32,
}

#[derive(Debug)]
pub enum ConnectorKind {
    DeadEnd(ConnectorDeadEnd),
    HexToHex(ConnectorOutside),
    OnHex(ConnectorOnHex),
    Outside(ConnectorOutside),
}

#[derive(Debug)]
pub struct ConnectorDeadEnd {
    pub position: ConnectorPosition,
}

#[derive(Debug)]
pub struct ConnectorOnHex {
    pub hexagon: HexagonPosition,
    pub edge_sub: ConnectorEdgeSub,
}

#[derive(Debug)]
pub struct ConnectorEdgeSub {
    pub a: EdgeSub,
    pub b: EdgeSub,
}

#[derive(Debug)]
pub struct ConnectorOutside {
    pub connector_a: ConnectorPosition,
    pub connector_b: ConnectorPosition,
}

pub struct Tile {
    pub inner_connectors: Vec<ConnectorEdgeSub>,
}
