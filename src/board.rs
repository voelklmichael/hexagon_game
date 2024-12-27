use itertools::Itertools;

type BoardPoint = egui::Pos2;
#[derive(Debug)]
pub struct Board {
    pub ratio: u16,
    pub fields: Vec<(BoardCoordinate, bool)>,
    pub connected: std::collections::HashMap<Connector, Connector>,
    pub outer_connections: Vec<Connector>,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BoardCoordinate {
    pub x: i32,
    pub y: i32,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Connector {
    pub field: BoardCoordinate,
    pub side: u8,
    pub is_left: bool,
    pub connected_to_outside: bool,
}
impl Board {
    pub fn new(ratio: u16) -> Self {
        let total_size = 1.;
        let board_center = egui::pos2(total_size / 2., total_size / 2.);
        let bounding_hexagon_edge = total_size / 2.;
        let outer_corners = compute_hexagon_corners(board_center, bounding_hexagon_edge);
        let cell_edge = bounding_hexagon_edge / (ratio as f32);
        let outer_edge_centers = outer_corners
            .iter()
            .zip(outer_corners.iter().skip(1))
            .map(|(&s, &e)| s + (e - s) / 2.)
            .collect_vec();
        let mut fields = Vec::new();
        let mut centers = Vec::new();
        let mut connected = std::collections::HashMap::new();
        let mut outer_connections = Vec::new();
        let n = ratio as i32;
        for x in -n..n {
            for y in -n..n {
                let cell_center =
                    board_center + cell_edge * egui::vec2(0., 3f32.sqrt()) * (x as f32);
                let alpha = 30f32.to_radians();
                let cell_center = cell_center
                    + cell_edge * 3f32.sqrt() * egui::vec2(alpha.cos(), alpha.sin()) * (y as f32);
                let distance = cell_center.distance(board_center) / bounding_hexagon_edge;
                if (distance - 1.0) > 1e-6 {
                    continue;
                }
                let cell_corners = compute_hexagon_corners(cell_center, cell_edge);
                let cell_edge_centers = cell_corners
                    .iter()
                    .zip(cell_corners.iter().skip(1))
                    .map(|(&s, &e)| s + (e - s) / 2.)
                    .collect_vec();
                let factor = bounding_hexagon_edge / 2.0 * 3f32.sqrt();
                let distances = cell_edge_centers
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
                fields.push((BoardCoordinate { x, y }, false));
                for (k, pos) in cell_edge_centers.into_iter().enumerate() {
                    // check if edge is connected to outside
                    let outer = outer_edge_centers[k];
                    let outer2center = (outer - board_center) / factor;
                    let cell2center = (pos - board_center) / factor;
                    if (cell2center.dot(outer2center) - 1.0).abs() < 1e-6 {
                        for is_left in [true, false] {
                            outer_connections.push(Connector {
                                field: BoardCoordinate { x, y },
                                side: k as u8,
                                is_left,
                                connected_to_outside: true,
                            });
                        }
                        continue;
                    }

                    // check if
                    let mut is_connected = false;
                    for (coord, kk, ppos, iis_connected) in centers.iter_mut() {
                        if *iis_connected {
                            continue;
                        }
                        let kmax = k.max(*kk);
                        let kmin = k.min(*kk);
                        if kmax - kmin != 3 {
                            continue;
                        }
                        if pos.distance(*ppos) > 1e-6 {
                            continue;
                        }
                        for is_left in [true, false] {
                            connected.insert(
                                Connector {
                                    field: BoardCoordinate { x, y },
                                    side: k as u8,
                                    connected_to_outside: false,
                                    is_left,
                                },
                                Connector {
                                    field: *coord,
                                    side: *kk as u8,
                                    connected_to_outside: false,
                                    is_left: !is_left,
                                },
                            );
                            connected.insert(
                                Connector {
                                    field: *coord,
                                    side: *kk as u8,
                                    connected_to_outside: false,
                                    is_left,
                                },
                                Connector {
                                    field: BoardCoordinate { x, y },
                                    side: k as u8,
                                    connected_to_outside: false,
                                    is_left: !is_left,
                                },
                            );
                        }
                        is_connected = true;
                        *iis_connected = true;
                        break;
                    }
                    centers.push((BoardCoordinate { x, y }, k, pos, is_connected))
                }
            }
        }
        let mut not_connected = centers
            .into_iter()
            .filter_map(|(coord, side, pos, is_connected)| {
                (!is_connected).then_some((coord, side, pos))
            })
            .collect_vec();
        let mut clusters = Vec::new();
        while let Some(entry) = not_connected.pop() {
            let mut current_cluster = Vec::new();
            let mut to_add = vec![entry];
            while !to_add.is_empty() {
                current_cluster.extend(std::mem::take(&mut to_add));
                let mut to_add_indices = Vec::new();
                for (index, (_, _, ppos)) in not_connected.iter().enumerate() {
                    if current_cluster
                        .iter()
                        .map(|(_, _, pos)| pos)
                        .any(|pos| pos.distance(*ppos) / cell_edge < 1.)
                    {
                        to_add_indices.push(index)
                    }
                }
                for index in to_add_indices.into_iter().rev() {
                    to_add.push(not_connected.remove(index));
                }
            }
            clusters.push(current_cluster);
        }
        for mut cluster in clusters {
            while !cluster.is_empty() {
                let mut min = None;
                for (i, (_, _, pos)) in cluster.iter().enumerate() {
                    for (j, (_, _, ppos)) in cluster.iter().enumerate().skip(i + 1) {
                        let d = pos.distance(*ppos);
                        min = Some(if let Some((i0, j0, d0)) = min.take() {
                            if d0 < d {
                                (i0, j0, d0)
                            } else {
                                (i, j, d)
                            }
                        } else {
                            (i, j, d)
                        });
                    }
                }
                let (i, j, _) = min.unwrap();
                let (coord, side, _) = cluster.remove(j);
                let (ccoord, sside, _) = cluster.remove(i);
                for is_left in [true, false] {
                    let connector = Connector {
                        field: coord,
                        side: side as u8,
                        is_left,
                        connected_to_outside: true,
                    };
                    let cconnector = Connector {
                        field: ccoord,
                        side: sside as u8,
                        is_left: if (side + sside) % 2 == 0 {
                            is_left
                        } else {
                            !is_left
                        },
                        connected_to_outside: true,
                    };
                    connected.insert(connector, cconnector);
                    connected.insert(cconnector, connector);
                }
            }
        }
        Self {
            ratio,
            fields,
            connected,
            outer_connections,
        }
    }
}

#[test]
fn test_compute_board() {
    Board::new(3);
}

fn compute_hexagon_corners(center: BoardPoint, edge: f32) -> [BoardPoint; 7] {
    let alpha = 30.0f32.to_radians();
    let mut start = center - edge * egui::vec2(alpha.sin(), alpha.cos());
    let mut points = vec![start];
    for i in 0..6 {
        let alpha = (60.0 * (i as f32)).to_radians();
        let end = start + edge * egui::vec2(alpha.cos(), alpha.sin());
        start = end;
        points.push(start);
    }
    points.try_into().unwrap()
}
