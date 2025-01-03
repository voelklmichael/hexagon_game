mod random_state;
use std::collections::{HashMap, HashSet};

use itertools::Itertools;

use crate::bimap::BiMap;
type Color = egui::Color32;

#[derive(Debug, Default, serde::Serialize, serde::Deserialize, Clone)]
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
#[derive(Debug, serde::Deserialize, serde::Serialize, Clone)]
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
    pub player_tiles: HashMap<PlayerId, Vec<HexagonCode>>,
    pub used_tiles: Vec<u16>,
    pub available_power_ups: Vec<(ConnectorPosition, PowerUp)>,
    pub player_pickups: HashMap<PlayerId, Vec<PowerUp>>,
}
#[derive(Debug, serde::Deserialize, serde::Serialize, Clone)]
pub enum PowerUp {
    Swords,
    Blitz,
    Hammer,
}
impl PowerUp {
    fn get_variants() -> Vec<Self> {
        use PowerUp::*;
        [Swords, Blitz, Hammer].into()
    }

    pub(crate) fn as_unicode_text(&self) -> String {
        match self {
            crate::game_state::PowerUp::Swords => "⚔",
            crate::game_state::PowerUp::Blitz => "⚡",
            crate::game_state::PowerUp::Hammer => "⚒",
        }
        .into()
    }

    fn get_killed_connectors(&self, connector: ConnectorCoordinate) -> Vec<ConnectorCoordinate> {
        let k = connector.0 as i8;
        match self {
            PowerUp::Swords => [k - 1, k + 1].to_vec(),
            PowerUp::Hammer => [k - 2, k + 2].to_vec(),
            PowerUp::Blitz => [k - 3, k + 3].to_vec(),
        }
        .into_iter()
        .map(|k| ConnectorCoordinate((k + 12) as u8 % 12))
        .collect_vec()
    }
}

#[derive(Debug, serde::Deserialize, serde::Serialize, Clone)]
pub struct GameConfiguration {
    pub player_count: usize,
    pub random_seed: u64,
    pub board_size: usize,
    pub tiles_per_player: usize,
    pub power_ups: bool,
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
            power_ups: true,
        }
    }
}

#[derive(Debug)]
pub enum Step {
    SameTile {
        start: ConnectorPosition,
        end: ConnectorPosition,
    },
    Remaining {
        towards: ConnectorPosition,
    },
    Tile2Tile {
        start: ConnectorPosition,
        end: ConnectorPosition,
    },
}

impl GameState {
    pub fn new(config: GameConfiguration) -> Result<Self, String> {
        let GameConfiguration {
            player_count,
            random_seed,
            board_size,
            tiles_per_player,
            power_ups,
        } = config;
        let power_ups = if player_count >= 2 { power_ups } else { false };
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
        let mut player_pickups = HashMap::new();
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
                tiles.push(HexagonCode::unrotated(tile));
            }
            player_tiles.insert(id.clone(), tiles);
            player_pickups.insert(id.clone(), Default::default());
        }

        // distribute power ups
        // - one for each player
        // - one per hex
        // - not on same hex as player starts
        //TODO: - not on neighbouring connectors
        let mut power_ups_start_locations = Vec::new();
        if power_ups {
            let mut possible_fields = board.fields.iter().map(|x| x.position).collect_vec();
            for (_, pos) in &player_positions {
                let pos = &pos.as_ref().unwrap().coordinate;
                if let Some(index) = possible_fields.iter().position(|x| x == pos) {
                    possible_fields.remove(index);
                };
            }
            for _ in 0..player_count {
                if possible_fields.is_empty() {
                    break;
                }
                let coordinate = random_state.select_rand_element(&mut possible_fields);
                let connector = random_state.select_rand_element(&mut (0..12).collect_vec());
                let pickup = random_state.select_rand_element(&mut PowerUp::get_variants());
                power_ups_start_locations.push((
                    ConnectorPosition {
                        coordinate,
                        connector: ConnectorCoordinate(connector),
                    },
                    pickup,
                ))
            }
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
            available_power_ups: power_ups_start_locations,
            player_pickups,
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
    pub(crate) fn play_by_code(
        &mut self,
        code: &HexagonCode,
    ) -> (bool, HashMap<PlayerId, Vec<Step>>, Vec<PlayerKilled>) {
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
            let permutation = code.to_permutation();
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
            let index = tiles.iter().position(|x| x.code == code.code).unwrap();
            tiles.remove(index);
            let tile = self.random_state.get_tile(&mut self.used_tiles);
            tiles.push(HexagonCode::unrotated(tile));
        }

        // move all players
        let mut steps = HashMap::new();
        let mut picked_up_powerups = Vec::new();
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
            let mut steps_taken = vec![Step::SameTile {
                start: current_position.clone(),
                end: next,
            }];
            loop {
                let iter = self
                    .available_power_ups
                    .iter()
                    .enumerate()
                    .filter(|(_, (pos, _))| pos == &next)
                    .map(|(index, _)| (pid.clone(), moved, index));
                picked_up_powerups.extend(iter);
                // pickup item
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
                    steps_taken.push(Step::Remaining { towards: next });
                } else if let Some(field) = self.board.connections.field_connectors.get(&next)
                    && !used.contains(field)
                {
                    steps_taken.push(Step::Tile2Tile {
                        start: next,
                        end: *field,
                    });
                    next = *field;
                } else if let Some(inner) = self.board.connections.inner_connectors.get(&next)
                    && !used.contains(inner)
                {
                    moved += 1;
                    steps_taken.push(Step::SameTile {
                        start: next,
                        end: *inner,
                    });
                    next = *inner;
                } else {
                    break;
                }
            }
            self.player_statistics.get_mut(pid).unwrap().moved(moved);
            if !moved_out {
                *self.player_positions.get_mut(pid).unwrap() = Some(next);
            }
            steps.insert(pid.clone(), steps_taken);
        }
        // find player who first picked up a power up
        let mut pickups_to_remove = Vec::new();
        while let Some((pid, mut moved, powerup)) = picked_up_powerups.pop() {
            pickups_to_remove.push(powerup);
            let mut pid = Some(pid);
            let mut i = 0;
            while i < picked_up_powerups.len() {
                if picked_up_powerups[i].2 == powerup {
                    let (pid2, moved2, _) = picked_up_powerups.remove(i);
                    match moved.cmp(&moved2) {
                        std::cmp::Ordering::Less => {}
                        std::cmp::Ordering::Equal => {
                            pid = None;
                        }
                        std::cmp::Ordering::Greater => {
                            pid = Some(pid2);
                            moved = moved2;
                        }
                    }
                } else {
                    i += 1;
                }
            }
            if let Some(pid) = pid {
                let pickup = self.available_power_ups[powerup].1.clone();
                self.player_pickups.get_mut(&pid).unwrap().push(pickup);
            }
        }
        // remove picked up power ups
        {
            pickups_to_remove.sort();
            pickups_to_remove.dedup();
            for index in pickups_to_remove.into_iter().rev() {
                self.available_power_ups.remove(index);
            }
        }

        // move a random power up
        if !self.available_power_ups.is_empty() {
            let (position, _) = self
                .random_state
                .select_rand_element(&mut self.available_power_ups.iter_mut().collect_vec());
            // move to neighbouring tile with 50% chance
            if self.random_state.next_with_limit(2) == 1 {
                if let Some(new_position) = self.board.connections.field_connectors.get(&position) {
                    if self
                        .player_positions
                        .iter()
                        .filter_map(|(_, pos)| pos.as_ref())
                        .all(|pos| {
                            pos != new_position
                                && self.board.connections.field_connectors.get(pos)
                                    != Some(new_position)
                                && self
                                    .board_usage
                                    .used_connectors
                                    .iter()
                                    .all(|(_, used)| !used.contains(new_position))
                        })
                    {
                        *position = *new_position;
                    }
                }
            };
            // move to different connector on same tile
            let new_connector = self.random_state.next_with_limit(5) as u8;
            let new_connector = if new_connector < position.connector.0 {
                new_connector
            } else {
                new_connector + 1
            };
            let new_position = ConnectorPosition {
                coordinate: position.coordinate,
                connector: ConnectorCoordinate(new_connector),
            };
            if self
                .player_positions
                .iter()
                .filter_map(|(_, pos)| pos.as_ref())
                .all(|pos| {
                    pos != &new_position
                        && self.board.connections.field_connectors.get(pos) != Some(&new_position)
                        && self
                            .board_usage
                            .used_connectors
                            .iter()
                            .all(|(_, used)| !used.contains(&new_position))
                })
            {
                *position = new_position;
            }
        }
        // check if player stands on power up
        {
            self.available_power_ups
                .iter()
                .map(|(pos, _)| pos)
                .for_each(|powerup_pos| {
                    if self
                        .player_positions
                        .iter()
                        .filter_map(|(_, pos)| pos.clone())
                        .any(|player_pos| &player_pos == powerup_pos)
                    {
                        panic!("Player position is power up position - this should never happen");
                    }
                });
        }
        // apply power ups
        let killed_players = {
            let mut killed_players = Vec::new();
            for (player_killing, player_killing_position) in self
                .player_positions
                .iter()
                .filter_map(|(pid, pos)| pos.as_ref().map(|pos| (pid, pos)))
            {
                for powerup in self.player_pickups.get(player_killing).unwrap() {
                    for killed in powerup.get_killed_connectors(player_killing_position.connector) {
                        assert_ne!(killed, player_killing_position.connector);
                        let killed = ConnectorPosition {
                            coordinate: player_killing_position.coordinate,
                            connector: killed,
                        };
                        let killed =
                            self.player_positions
                                .iter()
                                .filter_map(|(player_killed, pos)| {
                                    (pos.as_ref() == Some(&killed)).then_some(PlayerKilled {
                                        player_killed_id: player_killed.clone(),
                                        player_killing_id: player_killing.clone(),
                                        powerup: powerup.clone(),
                                        player_killed_pos: killed,
                                        player_killing_pos: *player_killing_position,
                                    })
                                });
                        killed_players.extend(killed);
                    }
                }
            }
            for killed in &killed_players {
                *self
                    .player_positions
                    .get_mut(&killed.player_killed_id)
                    .unwrap() = None;
            }
            killed_players
        };
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
            (false, steps, killed_players)
        } else {
            (true, steps, killed_players)
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

    pub(crate) fn testing() {
        fn take_turn(mut game: GameState) -> u32 {
            let pid = game.player_ids.first().unwrap().clone();
            let tiles = game.player_tiles.get(&pid).unwrap();
            let mut possible_tiles = vec![0, 1, 2usize];
            let tile = game.random_state.select_rand_element(&mut possible_tiles);
            let mut possible_rotations = vec![0, 1, 2, 3, 4, 5u8];
            let rotation = game
                .random_state
                .select_rand_element(&mut possible_rotations);
            let code = tiles[tile].code;
            if game.play_by_code(&HexagonCode { code, rotation }).0 {
                let max = game.player_statistics.get(&pid).unwrap().max_move();
                max
            } else {
                take_turn(game)
            }
        }
        let mut file_to_write = String::new();
        let n = 100_000;
        for board_size in 3..22 {
            let mut maxes = Vec::new();
            for random_seed in 0..n {
                let game = Self::new(GameConfiguration {
                    player_count: 1,
                    random_seed,
                    board_size,
                    tiles_per_player: 3,
                    power_ups: false,
                })
                .unwrap();
                let max = take_turn(game);
                maxes.push(max);
            }
            let max_max = maxes.iter().cloned().max().unwrap();
            let mut max_maxes = Vec::new();
            for max in 0..max_max {
                let count = maxes.iter().filter(|m| m == &&max).count();
                if count > 0 {
                    max_maxes.push((max, count));
                }
            }

            let mut header = String::new();
            let mut data = String::new();
            max_maxes.reverse();
            for (max, count) in max_maxes {
                header.push_str(&format!("{max: >5}|"));
                data.push_str(&format!("{count: >5}|"));
            }
            println!("{}", board_size);
            println!("{}", &header[..50]);
            println!("{}", &data[..50]);
            for s in [board_size.to_string(), header, data] {
                file_to_write.push_str(&s);
                file_to_write.push('\n');
            }
            std::fs::write("single_player.txt", &file_to_write).unwrap();
        }
    }
}

pub struct PlayerKilled {
    pub player_killed_id: PlayerId,
    pub player_killing_id: PlayerId,
    pub powerup: PowerUp,
    pub player_killed_pos: ConnectorPosition,
    pub player_killing_pos: ConnectorPosition,
}

#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Deserialize, serde::Serialize,
)]
pub struct HexagonCode {
    pub code: u16,
    pub rotation: u8, //number inside 0..6
}
impl HexagonCode {
    fn unrotated(code: u16) -> Self {
        Self { code, rotation: 0 }
    }

    pub(crate) fn to_permutation(&self) -> [u8; 12] {
        use crate::permutations::{int_to_array, rotate};
        let unrotated = int_to_array(self.code);
        rotate(unrotated, self.rotation)
    }
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Deserialize, serde::Serialize,
)]
pub struct BoardCoordinate {
    pub x: i32,
    pub y: i32,
}

#[derive(Debug, serde::Deserialize, serde::Serialize, Clone)]
pub struct BoardField {
    pub position: BoardCoordinate,
    pub hexagon: Option<HexagonCode>,
}

#[derive(Debug, serde::Deserialize, serde::Serialize, Clone)]
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

#[derive(Debug, serde::Deserialize, serde::Serialize, Clone)]
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

#[derive(Debug, Default, serde::Serialize, serde::Deserialize, Clone)]
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
#[derive(Debug, serde::Serialize, serde::Deserialize, Clone)]
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
