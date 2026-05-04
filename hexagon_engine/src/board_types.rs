mod board;
mod tile;

#[derive(Clone, Copy, Debug, strum::EnumIter)]
pub enum Edge {
    Top,
    TopLeft,
    BottomLeft,
    Bottom,
    BottomRight,
    TopRight,
}

#[derive(Clone, Copy, Debug, strum::EnumIter)]
pub enum Sub {
    Left,
    Right,
}

#[derive(Clone)]
pub struct EdgeSub {
    pub edge: Edge,
    pub sub: Sub,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct HexagonPosition {
    pub x: i32,
    pub y: i32,
}

#[derive(Clone)]
pub struct ConnectorPosition {
    pub hexagon: HexagonPosition,
    pub edge_sub: EdgeSub,
}

pub struct Board {
    pub hexagons: Vec<HexagonPosition>,
    pub connectors: Vec<Connector>,
}

#[derive(Clone)]
pub struct ConnectorId(pub u32);

pub struct Connector {
    pub id: ConnectorId,
    pub kind: ConnectorKind,
    pub weight: u32,
}

pub enum ConnectorKind {
    DeadEnd(ConnectorDeadEnd),
    OnHex(ConnectorOnHex),
    Outside(ConnectorOutside),
}
pub struct ConnectorDeadEnd {
    pub position: ConnectorPosition,
}

pub struct ConnectorOnHex {
    pub hexagon: HexagonPosition,
    pub edge_sub: ConnectorEdgeSub,
}

pub struct ConnectorEdgeSub {
    pub a: EdgeSub,
    pub b: EdgeSub,
}

pub struct ConnectorOutside {
    pub connector_a: ConnectorPosition,
    pub connector_b: ConnectorPosition,
}

pub struct Tile {
    pub inner_connectors: Vec<ConnectorEdgeSub>,
}
