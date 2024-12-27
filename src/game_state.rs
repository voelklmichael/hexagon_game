mod random_state;
use std::collections::{HashMap, HashSet};

use itertools::Itertools;

use crate::bimap::BiMap;
type Color = egui::Color32;

#[derive(Debug, Default, serde::Serialize, serde::Deserialize)]
pub struct PlayerStatistics {
    moved: Vec<u32>,
}
impl PlayerStatistics {
    fn moved(&mut self, moved: u32) {
        self.moved.push(moved);
    }

    pub(crate) fn total_move(&self) -> u32 {
        self.moved.iter().cloned().sum()
    }
    pub(crate) fn max_move(&self) -> u32 {
        self.moved.iter().cloned().max().unwrap_or(0)
    }
}
#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub struct GameState {
    pub config: GameConfiguration,
    pub board: Board,
    pub player_positions: HashMap<PlayerId, Option<ConnectorPosition>>,
    pub player_statistics: HashMap<PlayerId, PlayerStatistics>,
    pub current_player: PlayerId,
    pub colors: ColorMap,
    random_state: random_state::Rand,
    pub board_usage: BoardUsage,
    pub player_ids: Vec<PlayerId>,
    pub player_tiles: HashMap<PlayerId, Vec<u16>>,
    pub used_tiles: Vec<u16>,
}

#[derive(Debug, serde::Deserialize, serde::Serialize, Clone)]
pub struct GameConfiguration {
    pub player_count: usize,
    pub random_seed: u64,
    pub board_size: usize,
    pub tiles_per_player: usize,
}
impl GameConfiguration {
    pub(crate) fn validate(&self) -> Result<(), String> {
        if self.player_count == 0 {
            return Err("Player Count has to be larger than 0".into());
        } else if self.player_count > 6 {
            return Err("Player count too large (Max: 6)".into());
        } else if self.tiles_per_player == 0 {
            return Err("Tiles/Player has to be larger than 0".into());
        } else if self.tiles_per_player > 10 {
            return Err("Tiles/Player too large (Max: 10)".into());
        } else if self.board_size == 0 {
            return Err("Board Size has to be larger than 0".into());
        } else if self.board_size > 30 {
            return Err("Board Size too large (Max: 30)".into());
        }

        Ok(())
    }
}
impl Default for GameConfiguration {
    fn default() -> Self {
        Self {
            player_count: 2,
            random_seed: 0,
            board_size: 6,
            tiles_per_player: 3,
        }
    }
}

impl GameState {
    pub fn new(config: GameConfiguration) -> Result<Self, String> {
        let GameConfiguration {
            player_count,
            random_seed,
            board_size,
            tiles_per_player,
        } = config;
        let player_ids = (0..player_count).map(|x| x + 1).map(PlayerId).collect_vec();
        let current_player = player_ids[0].clone();
        let colors = ColorMap::new(&player_ids);
        let board = Board::new(board_size);

        let mut board_usage = BoardUsage::default();
        let mut possible_outer_connectors = board.connections.outer_connectors.clone();
        let mut random_state = random_state::Rand::with_seed(random_seed);
        let mut player_positions = HashMap::new();
        let mut player_statistics = HashMap::new();
        let mut used_tiles = Vec::new();
        let mut player_tiles = HashMap::new();
        for id in &player_ids {
            let start = random_state.select_rand_element(&mut possible_outer_connectors);
            while let Some(index) = possible_outer_connectors
                .iter()
                .position(|x| x.coordinate == start.coordinate)
            {
                possible_outer_connectors.remove(index);
            }
            player_positions.insert(id.clone(), Some(start));
            player_statistics.insert(id.clone(), Default::default());
            board_usage
                .used_connectors
                .insert(id.clone(), [start].into_iter().collect());
            let mut tiles = Vec::new();
            for _ in 0..tiles_per_player {
                let tile = random_state.get_tile(&mut used_tiles);
                tiles.push(tile);
            }
            player_tiles.insert(id.clone(), tiles);
        }
        Ok(Self {
            board,
            player_positions,
            player_ids,
            current_player,
            colors,
            random_state,
            board_usage,
            player_statistics,
            player_tiles,
            used_tiles,
            config,
        })
    }

    pub(crate) fn get_color(&self, players: Vec<PlayerId>) -> Color {
        if players.is_empty() {
            self.colors.unused_color
        } else {
            let n = players.len() as u32;
            let (r, g, b) = players
                .into_iter()
                .map(|pid| self.colors.player_colors.get(&pid).unwrap())
                .fold((0u32, 0u32, 0u32), |(r, g, b), c| {
                    let [rr, gg, bb, _] = c.to_array();
                    (r + rr as u32, g + gg as u32, b + bb as u32)
                });
            let (r, g, b) = (r / n, g / n, b / n);
            Color::from_rgb(r as u8, g as u8, b as u8)
        }
    }

    /// returns true if game is done
    #[must_use]
    pub(crate) fn play_by_code(&mut self, code: &HexagonCode) -> bool {
        // place tile
        let (permutation, new_tile) = {
            let pid = &self.current_player;
            let current_position = self.player_positions.get(pid).unwrap().unwrap();
            self.board
                .fields
                .iter_mut()
                .find(|x| x.position == current_position.coordinate)
                .unwrap()
                .hexagon = Some(code.clone());
            let HexagonCode { code, rotation } = *code;
            let rotation = (6 + rotation - current_position.connector.0 / 2) % 6;
            let permutation: [_; 12] = {
                let permutation = crate::permutations::int_to_array(code);
                let permutation = crate::permutations::rotate(permutation, rotation);
                permutation
            };
            for i in 0..12u8 {
                let j = permutation[i as usize];
                if j < i {
                    continue;
                }
                assert_ne!(i, j);
                self.board.connections.inner_connectors.insert_new(
                    ConnectorPosition {
                        coordinate: current_position.coordinate,
                        connector: ConnectorCoordinate(i),
                    },
                    ConnectorPosition {
                        coordinate: current_position.coordinate,
                        connector: ConnectorCoordinate(j),
                    },
                );
            }
            (permutation, current_position.coordinate)
        };

        // update tile list
        {
            let tiles = self.player_tiles.get_mut(&self.current_player).unwrap();
            let index = tiles.iter().position(|x| x == &code.code).unwrap();
            tiles.remove(index);
            let tile = self.random_state.get_tile(&mut self.used_tiles);
            tiles.push(tile);
        }

        // move all players
        for pid in &self.player_ids {
            let Some(current_position) = self.player_positions.get(pid).unwrap() else {
                continue;
            };
            if current_position.coordinate != new_tile {
                continue;
            }
            let next_connector = permutation[current_position.connector.0 as usize];
            let next_connector = ConnectorCoordinate(next_connector);
            let used = self.board_usage.used_connectors.get_mut(pid).unwrap();
            let mut next = ConnectorPosition {
                coordinate: current_position.coordinate,
                connector: next_connector,
            };
            let mut moved_out = false;
            let mut moved = 1;
            loop {
                assert!(used.insert(next));
                if self.board.connections.outer_connectors.contains(&next) {
                    *self.player_positions.get_mut(pid).unwrap() = None;
                    moved_out = true;
                    break;
                } else if let Some(remaining) =
                    self.board.connections.remaining_connectors.get(&next)
                    && !used.contains(remaining)
                {
                    moved += 1;
                    next = *remaining;
                } else if let Some(field) = self.board.connections.field_connectors.get(&next)
                    && !used.contains(field)
                {
                    next = *field;
                } else if let Some(inner) = self.board.connections.inner_connectors.get(&next)
                    && !used.contains(inner)
                {
                    moved += 1;
                    next = *inner;
                } else {
                    break;
                }
            }
            self.player_statistics.get_mut(pid).unwrap().moved(moved);
            if !moved_out {
                *self.player_positions.get_mut(pid).unwrap() = Some(next);
            }
        }

        // determine next player - if impossible, game is over
        let offset = self
            .player_ids
            .iter()
            .position(|p| p == &self.current_player)
            .unwrap();
        if let Some(pid) = self
            .player_ids
            .iter()
            .cycle()
            .skip(offset + 1)
            .take(self.player_ids.len())
            .filter(|pid| self.player_positions.get(pid).unwrap().is_some())
            .next()
        {
            self.current_player = pid.clone();
            false
        } else {
            true
        }
    }

    #[must_use]
    pub(crate) fn get_player_position_color(&self) -> Option<(ConnectorPosition, egui::Color32)> {
        let pid = &self.current_player;
        if let Some(current_position) = self.player_positions.get(pid).unwrap() {
            let color = self.get_color([pid.clone()].into());
            Some((*current_position, color))
        } else {
            None
        }
    }
}

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Deserialize, serde::Serialize,
)]
pub struct HexagonCode {
    pub code: u16,
    pub rotation: u8, //number inside 0..6
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Deserialize, serde::Serialize,
)]
pub struct BoardCoordinate {
    pub x: i32,
    pub y: i32,
}

#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub struct BoardField {
    pub position: BoardCoordinate,
    pub hexagon: Option<HexagonCode>,
}

#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub struct Board {
    pub fields: Vec<BoardField>,
    pub connections: BoardConnections,
    pub edge_count: usize,
}

fn compute_hexagon_corners(center: egui::Pos2, edge: f32) -> [egui::Pos2; 7] {
    let alpha = 30.0f32.to_radians();
    let mut start = center - edge * egui::vec2(alpha.sin(), alpha.cos());
    let mut points = Vec::new();
    points.push(start);
    for i in 0..6 {
        let alpha = (60.0 * (i as f32)).to_radians();
        let end = start + edge * egui::vec2(alpha.cos(), alpha.sin());
        start = end;
        points.push(start);
    }
    points.try_into().unwrap()
}

impl Board {
    pub fn new(edge_count: usize) -> Self {
        let board_center = egui::Pos2::ZERO;
        let board_edge = 1.;
        let outer_corners = compute_hexagon_corners(board_center, board_edge);
        let outer_edge_centers = outer_corners
            .iter()
            .zip(outer_corners.iter().skip(1))
            .map(|(&s, &e)| s + (e - s) / 2.)
            .collect_vec();
        let n = edge_count as i32;
        let mut fields = Vec::new();
        let inner_edge = board_edge / n as f32;
        let mut connectors = Vec::new();
        for x in -n..n {
            for y in -n..n {
                let coordinate = BoardCoordinate { x, y };
                let inner_center = {
                    let temp = board_center + inner_edge * egui::vec2(0., 3f32.sqrt()) * (x as f32);
                    let alpha = 30f32.to_radians();
                    temp + inner_edge
                        * 3f32.sqrt()
                        * egui::vec2(alpha.cos(), alpha.sin())
                        * (y as f32)
                };
                let distance = inner_center.distance(board_center) / board_edge;
                if (distance - 1.0) > 1e-6 {
                    continue;
                }
                let inner_corners = compute_hexagon_corners(inner_center, inner_edge);
                let inner_edge_centers = inner_corners
                    .iter()
                    .zip(inner_corners.iter().skip(1))
                    .map(|(&s, &e)| s + (e - s) / 2.)
                    .collect_vec();
                let factor = board_edge / 2.0 * 3f32.sqrt();
                let distances = inner_edge_centers
                    .iter()
                    .zip(outer_edge_centers.iter())
                    .map(|(&cell, &outer)| {
                        let outer2center = (outer - board_center) / factor;
                        let cell2center = (cell - board_center) / factor;
                        cell2center.dot(outer2center)
                    })
                    .collect_vec();
                let distance = distances
                    .into_iter()
                    .max_by(|x, y| x.partial_cmp(y).unwrap())
                    .unwrap();
                if distance - 1.0 > 1e-6 {
                    continue;
                }
                fields.push(BoardField {
                    position: coordinate,
                    hexagon: None,
                });
                inner_corners
                    .iter()
                    .zip(inner_corners.iter().skip(1))
                    .enumerate()
                    .for_each(|(k, (&s, &e))| {
                        for c in [0, 1] {
                            let factor = if c == 0 { 0.1 } else { 0.9 };
                            connectors.push((
                                ConnectorPosition {
                                    coordinate: coordinate.clone(),
                                    connector: ConnectorCoordinate((k as u8) * 2 + c),
                                },
                                s + (e - s) * factor,
                            ))
                        }
                    })
            }
        }
        let field_connectors = {
            let mut field_connectors = BiMap::new();
            loop {
                let mut workload = None;
                'loopi: for (i, (_, posi)) in connectors.iter().enumerate() {
                    for (j, (_, posj)) in connectors.iter().enumerate().skip(i + 1) {
                        if posj.distance(*posi) < 1e-6 {
                            workload = Some((i, j));
                            break 'loopi;
                        }
                    }
                }
                if let Some((i, j)) = workload {
                    let (cj, _) = connectors.remove(j);
                    let (ci, _) = connectors.remove(i);
                    field_connectors.insert_new(ci, cj);
                } else {
                    break;
                }
            }
            field_connectors
        };
        let outer_connectors = {
            let mut outer_connectors = Vec::new();
            let factor = 3f32.sqrt() / 2.0;
            for (index, (c, pos)) in connectors.iter().enumerate() {
                let outer = outer_edge_centers[c.connector.side() as usize];
                let outer2center = (outer - board_center) / board_edge / factor;
                let pos2center = (*pos - board_center) / board_edge / factor;
                debug_assert!((outer2center.length() - 1.0).abs() < 1e-6);
                let distance = outer2center.dot(pos2center);
                if (distance - 1.0).abs() < 1e-6 {
                    outer_connectors.push(index);
                }
            }
            let mut outer_connectors = outer_connectors
                .into_iter()
                .rev()
                .map(|i| connectors.remove(i).0)
                .collect_vec();
            outer_connectors.reverse();
            outer_connectors
        };
        let remaining_connectors = {
            // sort connecectors into clusters
            let mut clusters = Vec::new();
            while let Some((c_start, pos_start)) = connectors.pop() {
                let mut new_cluster = vec![(c_start, pos_start)];
                loop {
                    let mut to_add = vec![];
                    for (index, (c, pos)) in connectors.iter().enumerate() {
                        // check side faciness
                        /*
                        {
                            let s1 = c_start.connector.side();
                            let s2 = c.connector.side();
                            if s1 == 0 {
                                if ![1, 5].contains(&s2) {
                                    continue;
                                }
                            } else if s1 == 3 {
                                if ![2, 4].contains(&s2) {
                                    continue;
                                }
                            } else if s2 == 0 {
                                if ![1, 5].contains(&s1) {
                                    continue;
                                }
                            } else if s2 == 3 {
                                if ![2, 4].contains(&s1) {
                                    continue;
                                }
                            } else {
                                let max = s1.max(s2);
                                let min = s1.min(s2);
                                if max - min != 1 {
                                    continue;
                                }
                            }
                        }
                        */
                        for (c_before, pos_before) in &new_cluster {
                            if pos.distance(*pos_before) / inner_edge < 0.9 || c_before.same_edge(c)
                            {
                                to_add.push(index);
                                break;
                            }
                        }
                    }
                    if to_add.is_empty() {
                        break;
                    }
                    for index in to_add.into_iter().rev() {
                        new_cluster.push(connectors.remove(index));
                    }
                }
                if !(new_cluster.len() % 2 == 0) {
                    break;
                }
                clusters.push(new_cluster);
                //clusters.pop();
            }
            // for each cluster, connect pairs with maximal distance
            let mut remaining_connectors = crate::bimap::BiMap::new();
            for mut cluster in clusters {
                while !cluster.is_empty() {
                    let mut maximizer = (0, 0, -1.);
                    for (i, (_, pi)) in cluster.iter().enumerate() {
                        for (j, (_, pj)) in cluster.iter().enumerate().skip(i) {
                            let d = pi.distance(*pj);
                            if maximizer.2 < d {
                                maximizer = (i, j, d);
                            }
                        }
                    }
                    assert!(maximizer.2 > -1.);
                    let (i, j, _) = maximizer;
                    let (cj, _) = cluster.remove(j);
                    let (ci, _) = cluster.remove(i);
                    remaining_connectors.insert_new(ci, cj);
                }
            }
            remaining_connectors
        };
        assert!(connectors.is_empty());
        Self {
            edge_count,
            fields,
            connections: BoardConnections {
                outer_connectors,
                inner_connectors: BiMap::new(),
                field_connectors,
                remaining_connectors,
            },
        }
    }
}

#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, serde::Serialize, serde::Deserialize,
)]
pub struct ConnectorCoordinate(pub u8);
impl ConnectorCoordinate {
    pub(crate) fn side(&self) -> u8 {
        self.0 / 2
    }

    pub(crate) fn is_left(&self) -> bool {
        self.0 % 2 == 0
    }

    fn same_edge(&self, connector: ConnectorCoordinate) -> bool {
        let min = self.min(&connector).0;
        let max = self.max(&connector).0;
        max - min == 1
    }
} // value between 0 and 12

#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, serde::Deserialize, serde::Serialize,
)]
pub struct ConnectorPosition {
    pub coordinate: BoardCoordinate,
    pub connector: ConnectorCoordinate,
}
impl ConnectorPosition {
    fn same_edge(&self, other: &ConnectorPosition) -> bool {
        self.coordinate == other.coordinate && self.connector.same_edge(other.connector)
    }
}

#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub struct BoardConnections {
    pub outer_connectors: Vec<ConnectorPosition>,
    pub inner_connectors: crate::bimap::BiMap<ConnectorPosition>,
    pub remaining_connectors: crate::bimap::BiMap<ConnectorPosition>,
    pub field_connectors: BiMap<ConnectorPosition>,
}

#[derive(
    Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, serde::Deserialize, serde::Serialize,
)]
pub struct PlayerId(pub usize);

#[derive(Debug, Default, serde::Serialize, serde::Deserialize)]
pub struct BoardUsage {
    pub used_connectors: HashMap<PlayerId, HashSet<ConnectorPosition>>,
}
impl BoardUsage {
    pub(crate) fn get_players(&self, connector: &ConnectorPosition) -> Vec<PlayerId> {
        self.used_connectors
            .iter()
            .filter_map(|(pid, used)| used.contains(connector).then_some(pid.clone()))
            .collect_vec()
    }
}
#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct ColorMap {
    player_colors: HashMap<PlayerId, Color>,
    unused_color: Color,
}
impl ColorMap {
    fn new(players: &[PlayerId]) -> Self {
        fn color_cycle(i: usize) -> Color {
            match i {
                0 => unreachable!("The PlayerID starts with 1"),
                1 => Color::BLUE,
                2 => Color::GREEN,
                3 => Color::RED,
                4 => Color::DARK_BLUE,
                5 => Color::DARK_GREEN,
                6 => Color::DARK_RED,
                _ => todo!("Not yet implemented"),
            }
        }
        Self {
            player_colors: players
                .iter()
                .map(|id| (id.clone(), color_cycle(id.0)))
                .collect(),
            unused_color: Color::KHAKI,
        }
    }

    pub(crate) fn get_cell_boundary_color(&self, is_used: bool) -> Color {
        if is_used {
            Color::GOLD
        } else {
            Color::GRAY
        }
    }
}
