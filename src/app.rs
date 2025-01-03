mod config_ui;

use std::ops::AddAssign;

use egui_extras::{Size, StripBuilder};
use itertools::Itertools;

use crate::game_state::{
    BoardCoordinate, BoardField, ConnectorCoordinate, ConnectorPosition, GameState, HexagonCode,
    Step,
};

const ANIMATION_SPEED: f32 = 0.01;
const ANIMATION_CIRCLE_SIZE_FACTOR: f32 = 1.5;
const EDGE_SHRINK_FACTOR: f32 = 0.95;
const PLAYER_KILLED_ANIMATION_COUNT: f32 = 100.;
#[derive(serde::Deserialize, serde::Serialize, Default)]
pub struct Hexagon {
    random_counter: u64,
    game_configuration: config_ui::GameConfigurationUI,
    selected_code_io: Option<HexagonCode>,
    game_state: Option<crate::game_state::GameState>,
    game_is_ongoing: bool,
    #[cfg(not(debug_assertions))]
    #[serde(skip)]
    sound_manager: Option<kira::manager::AudioManager<kira::manager::DefaultBackend>>,
    #[serde(skip)]
    animation:
        std::collections::HashMap<crate::game_state::PlayerId, Vec<(crate::game_state::Step, u64)>>,
    animation_counter: u64,
    #[serde(skip)]
    player_killed: (Option<u64>, Vec<crate::game_state::PlayerKilled>),
}
impl Hexagon {
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
        cc.egui_ctx.style_mut(|style| {
            style.text_styles.iter_mut().for_each(|x| {
                x.1.size *= 1.5;
            })
        });
        if let Some(storage) = cc.storage {
            return eframe::get_value(storage, eframe::APP_KEY).unwrap_or_default();
        }

        Default::default()
    }

    fn draw_selection(&mut self, ui: &mut egui::Ui) {
        if !self.game_is_ongoing {
            ui.label("Game is over");
            return;
        }

        let game = self.game_state.as_ref().unwrap();
        let Some((_, player_color)) = game.get_player_position_color() else {
            panic!("This should never happen");
        };
        let pid = game.current_player.0;

        ui.horizontal(|ui| {
            ui.colored_label(player_color, format!("Current Player: {pid}"));
        });

        let game = self.game_state.as_mut().unwrap();
        let Some((player_position, player_color)) = game.get_player_position_color() else {
            panic!("This should never happen");
        };
        let selectable_tiles = game.player_tiles.get_mut(&game.current_player).unwrap();

        let width = ui.available_width();
        let edge_size = width / 3.;
        let height = edge_size * 2. * selectable_tiles.len() as f32;
        let (response, painter) =
            ui.allocate_painter(egui::vec2(width, height), egui::Sense::click_and_drag());
        // background
        painter.rect_filled(response.rect, egui::Rounding::ZERO, egui::Color32::BLACK);

        let mut chosen_tile = None;

        for (offset_index, code) in selectable_tiles.iter_mut().enumerate() {
            let mut border_color = egui::Color32::GRAY;
            let mut is_selected = false;
            if let Some(selected) = &self.selected_code_io
                && selected.code == code.code
            {
                border_color = player_color;
                is_selected = true;
            }
            let mut center = response.rect.center_top();
            center.x -= edge_size * 0.2;
            let center = center + egui::vec2(0., edge_size * (2 * offset_index + 1) as f32); //TODO: this is not consistent with definition of edge_size
            draw_hexagon(
                &painter,
                center,
                edge_size * EDGE_SHRINK_FACTOR,
                None,
                border_color,
                edge_size / 30.,
            );
            let permutation = code.to_permutation();
            for i in 0..12u8 {
                let j = permutation[i as usize];
                if j < i {
                    continue;
                }
                assert_ne!(i, j);
                let (pi, ni) = compute_connector_position(
                    center,
                    edge_size,
                    &ConnectorPosition {
                        coordinate: BoardCoordinate { x: 0, y: 0 },
                        connector: ConnectorCoordinate(i),
                    },
                );
                let (pj, nj) = compute_connector_position(
                    center,
                    edge_size,
                    &ConnectorPosition {
                        coordinate: BoardCoordinate { x: 0, y: 0 },
                        connector: ConnectorCoordinate(j),
                    },
                );
                let factor = edge_size / 3.;
                let bezier = egui::epaint::CubicBezierShape::from_points_stroke(
                    [pi, pi - ni * factor, pj - nj * factor, pj],
                    false,
                    egui::Color32::TRANSPARENT,
                    egui::Stroke::new(edge_size / 30., egui::Color32::GOLD),
                );
                painter.add(bezier);
            }

            // draw player character (dummy for non-selected tiles)
            {
                let (p, _) = compute_connector_position(
                    center,
                    edge_size,
                    &ConnectorPosition {
                        coordinate: BoardCoordinate { x: 0, y: 0 },
                        connector: player_position.connector.clone(),
                    },
                );
                if is_selected {
                    painter.circle_filled(p, edge_size / 8., player_color);
                } else {
                    painter.circle_filled(p, edge_size / 12., player_color.gamma_multiply(0.7));
                }
            }

            // draw rotation buttons
            {
                for (k, is_left, angle) in [
                    (1, true, 55f32),
                    (2, false, 115f32),
                    (4, true, 235f32),
                    (5, false, -55f32),
                ] {
                    let (p, n) = {
                        let (p1, _) = compute_connector_position(
                            center,
                            edge_size,
                            &ConnectorPosition {
                                coordinate: BoardCoordinate { x: 0, y: 0 },
                                connector: ConnectorCoordinate(k * 2),
                            },
                        );
                        let (p2, n) = compute_connector_position(
                            center,
                            edge_size,
                            &ConnectorPosition {
                                coordinate: BoardCoordinate { x: 0, y: 0 },
                                connector: ConnectorCoordinate(k * 2 + 1),
                            },
                        );
                        (p1 + (p2 - p1) / 2., n)
                    };
                    let p = p + edge_size / 3. * n;
                    let (factor, is_above) = if let Some(pointer) = response.hover_pos()
                        && pointer.distance(p) / edge_size < 0.3
                    {
                        (2., true)
                    } else {
                        (1., false)
                    };
                    draw_text(
                        &painter,
                        if is_left { "↶".into() } else { "↷".into() },
                        angle,
                        p,
                        egui::Color32::GOLD,
                        edge_size / 2. * factor,
                    );
                    if response.clicked() && is_above {
                        let rotation = if is_left { 1 } else { 5 };
                        code.rotation = (code.rotation + rotation) % 6;
                        self.selected_code_io = Some(code.clone());
                    }
                }
            }
            // draw select/play button
            {
                let symbol = if is_selected { "⇒" } else { "✔" };
                let p = center + edge_size * EDGE_SHRINK_FACTOR * egui::vec2(1., 0.) * 1.4;
                let (factor, is_above) = if let Some(pointer) = response.hover_pos()
                    && pointer.distance(p) / edge_size < 0.3
                {
                    (2., true)
                } else {
                    (1., false)
                };
                let galley = painter.layout(
                    symbol.to_string(),
                    egui::FontId::monospace(edge_size / 2. * factor),
                    egui::Color32::GOLD,
                    0.,
                );
                painter.galley(p - galley.size() / 2., galley, egui::Color32::GOLD);

                if response.clicked() && is_above {
                    if is_selected {
                        chosen_tile = Some(code.clone());
                    } else {
                        self.selected_code_io = Some(code.clone());
                    }
                }
            }
        }

        if let Some(tile) = chosen_tile {
            let (game_is_over, animation, player_killed) = game.play_by_code(&tile);
            self.play_sound();
            self.animation = Default::default();
            for (pid, steps) in animation {
                let steps = steps.into_iter().map(|step| (step, 0)).collect_vec();
                self.animation.insert(pid, steps);
            }
            self.animation_counter = 0;
            self.player_killed = (Default::default(), player_killed);
            ui.ctx().request_repaint();
            if game_is_over {
                self.game_is_ongoing = false;
                return;
            }
        }
    }

    fn draw_board(&mut self, ui: &mut egui::Ui) {
        if !self.animation.is_empty() || !self.player_killed.1.is_empty() {
            ui.ctx().request_repaint();
            self.animation_counter += 1;
        }
        let game_state = self.game_state.as_ref().unwrap();
        let total_size = ui.max_rect().size();
        let total_size = total_size.x.min(total_size.y);

        let (_, painter) =
            ui.allocate_painter(egui::vec2(total_size, total_size), egui::Sense::click());

        // background
        let paint_area =
            egui::Rect::from_min_size(ui.max_rect().left_top(), egui::vec2(total_size, total_size));
        painter.rect_filled(paint_area, egui::Rounding::ZERO, egui::Color32::BLACK);
        let total_size = paint_area.width().min(paint_area.height());
        let board_center = paint_area.center();
        let bounding_hexagon_edge = total_size / 2.1;
        let cell_edge = bounding_hexagon_edge / (game_state.board.edge_count as f32);
        draw_hexagon(
            &painter,
            board_center,
            bounding_hexagon_edge,
            None,
            egui::Color32::GRAY,
            cell_edge / 10.,
        );
        for BoardField { position, hexagon } in &game_state.board.fields {
            let cell_center = compute_cell_center(board_center, cell_edge, position);
            draw_hexagon(
                &painter,
                cell_center,
                cell_edge * EDGE_SHRINK_FACTOR,
                None,
                game_state.colors.get_cell_boundary_color(hexagon.is_some()),
                cell_edge / 30.,
            );
        }
        for connector in &game_state.board.connections.outer_connectors {
            let (x, normal) = compute_connector_position(board_center, cell_edge, connector);
            let color = {
                let players = game_state.board_usage.get_players(connector);
                game_state.get_color(players)
            };
            painter.line_segment(
                [
                    x - normal * cell_edge / 10.,
                    x + normal * bounding_hexagon_edge / 50.,
                ],
                egui::Stroke::new(bounding_hexagon_edge / 100., color),
            );
        }
        let mut animations_done = Vec::new();
        for (conn1, conn2) in game_state.board.connections.field_connectors.iter() {
            let color = {
                let players = game_state.board_usage.get_players(conn1);
                game_state.get_color(players)
            };
            let (c1, n1) = compute_connector_position(board_center, cell_edge, conn1);
            let (c2, n2) = compute_connector_position(board_center, cell_edge, conn2);
            let factor = cell_edge / 30.;
            painter.line_segment(
                [c1 - n1 * factor, c2 - n2 * factor],
                egui::Stroke::new(bounding_hexagon_edge / 100., color),
            );
            for (pid, start_is_conn1, player_color, start_time) in
                self.animation.iter().filter_map(|(pid, steps)| {
                    let (step, start_time) = steps.first().unwrap();
                    if let Step::Tile2Tile { start, end } = step {
                        ((start == conn1 && end == conn2) || (start == conn2 && end == conn1)).then(
                            || {
                                (
                                    pid.clone(),
                                    start == conn1,
                                    self.game_state
                                        .as_ref()
                                        .unwrap()
                                        .get_color([pid.clone()].into()),
                                    *start_time,
                                )
                            },
                        )
                    } else {
                        None
                    }
                })
            {
                let points = [c1, c2];
                let points = if start_is_conn1 {
                    points
                } else {
                    [points[1], points[0]]
                };
                let distance = points[0].distance(points[1]);
                let moved = cell_edge
                    * EDGE_SHRINK_FACTOR
                    * (self.animation_counter - start_time) as f32
                    * ANIMATION_SPEED;
                if distance < moved {
                    animations_done.push(pid)
                } else {
                    let p = points[0] + (points[1] - points[0]).normalized() * moved;
                    painter.circle_filled(
                        p,
                        cell_edge / 10. * ANIMATION_CIRCLE_SIZE_FACTOR,
                        player_color,
                    );
                }
            }
        }
        for (conn1, conn2) in game_state.board.connections.remaining_connectors.iter() {
            let color = {
                let players = game_state.board_usage.get_players(conn1);
                game_state.get_color(players)
            };
            let (c1, n1) = compute_connector_position(board_center, cell_edge, conn1);
            let (c2, n2) = compute_connector_position(board_center, cell_edge, conn2);
            let factor = c1.distance(c2) / 3.;
            let bezier = egui::epaint::CubicBezierShape::from_points_stroke(
                [c1, c1 + n1 * factor, c2 + n2 * factor, c2],
                false,
                egui::Color32::TRANSPARENT,
                egui::Stroke::new(cell_edge / 30., color),
            );
            fn comparator(
                step: &Step,
                conn1: &ConnectorPosition,
                conn2: &ConnectorPosition,
            ) -> Option<bool> {
                if let Step::Remaining { towards } = step {
                    if towards == conn1 {
                        Some(false)
                    } else if towards == conn2 {
                        Some(true)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Self::animation_bezier(
                &painter,
                cell_edge,
                &mut animations_done,
                conn2,
                conn1,
                &bezier,
                &self.game_state,
                &self.animation,
                self.animation_counter,
                comparator,
            );
            painter.add(bezier);
        }
        for (conn1, conn2) in game_state.board.connections.inner_connectors.iter() {
            let color = {
                let players = game_state.board_usage.get_players(conn1);
                game_state.get_color(players)
            };
            let (c1, n1) = compute_connector_position(board_center, cell_edge, conn1);
            let (c2, n2) = compute_connector_position(board_center, cell_edge, conn2);
            let factor = cell_edge / 3.;
            let bezier = egui::epaint::CubicBezierShape::from_points_stroke(
                [c1, c1 - n1 * factor, c2 - n2 * factor, c2],
                false,
                egui::Color32::TRANSPARENT,
                egui::Stroke::new(cell_edge / 30., color),
            );
            fn comparator(
                step: &Step,
                conn1: &ConnectorPosition,
                conn2: &ConnectorPosition,
            ) -> Option<bool> {
                if let Step::SameTile { start, end } = step {
                    if start == conn1 && end == conn2 {
                        Some(false)
                    } else if start == conn2 && end == conn1 {
                        Some(true)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Self::animation_bezier(
                &painter,
                cell_edge,
                &mut animations_done,
                conn1,
                conn2,
                &bezier,
                &self.game_state,
                &self.animation,
                self.animation_counter,
                comparator,
            );
            painter.add(bezier);
        }
        // update animations (remove done steps, update following start_time)
        for pid in animations_done {
            let animations = self.animation.get_mut(&pid).unwrap();
            animations.remove(0);
            if let Some((_, start_time)) = animations.first_mut() {
                *start_time = self.animation_counter;
            } else {
                self.animation.remove(&pid);
            }
        }
        // update killed animations
        if self.animation.is_empty() && !self.player_killed.1.is_empty() {
            if self.player_killed.0.is_none() {
                self.player_killed.0 = Some(self.animation_counter);
            }
            let start_time = self.player_killed.0.unwrap();
            let elapsed =
                (self.animation_counter - start_time) as f32 / PLAYER_KILLED_ANIMATION_COUNT;
            let elapsed = elapsed.max(0.);
            if elapsed >= 1.0 {
                self.player_killed = Default::default();
            } else {
                for crate::game_state::PlayerKilled {
                    player_killed_id,
                    player_killing_id,
                    powerup,
                    player_killed_pos,
                    player_killing_pos,
                } in &self.player_killed.1
                {
                    let player_killed_pos =
                        compute_connector_position(board_center, cell_edge, player_killed_pos).0;
                    let player_killing_pos =
                        compute_connector_position(board_center, cell_edge, player_killing_pos).0;

                    painter.circle_filled(
                        player_killed_pos,
                        cell_edge / 10.,
                        game_state.get_color([player_killed_id.clone()].to_vec()),
                    );

                    let pos =
                        player_killing_pos + elapsed * (player_killed_pos - player_killing_pos);
                    draw_text(
                        &painter,
                        powerup.as_unicode_text(),
                        0.,
                        pos,
                        game_state.get_color([player_killing_id.clone()].to_vec()),
                        cell_edge / 4.,
                    );
                }
            }
        }

        // draw final player positions
        self.game_state
            .as_ref()
            .unwrap()
            .player_positions
            .iter()
            .filter_map(|(pid, pos)| pos.as_ref().map(|pos| (pid, pos)))
            .for_each(|(pid, position)| {
                let color = self
                    .game_state
                    .as_ref()
                    .unwrap()
                    .get_color([pid.clone()].into());
                let (position, _) = compute_connector_position(board_center, cell_edge, position);
                painter.circle_filled(position, cell_edge / 10., color);
            });

        // draw power ups
        self.game_state
            .as_ref()
            .unwrap()
            .available_power_ups
            .iter()
            .for_each(|(connector, pickup)| {
                let (pos, _) = compute_connector_position(board_center, cell_edge, connector);
                let center = pos;
                painter.circle_filled(center, cell_edge / 5., egui::Color32::DARK_GRAY);
                let text = pickup.as_unicode_text();
                draw_text(
                    &painter,
                    text,
                    0., //60f32 * (connector.connector.0 / 2) as f32,
                    center,
                    egui::Color32::ORANGE,
                    cell_edge / 4.,
                );
            });
    }

    fn animation_bezier(
        painter: &egui::Painter,
        cell_edge: f32,
        animations_done: &mut Vec<crate::game_state::PlayerId>,
        conn1: &ConnectorPosition,
        conn2: &ConnectorPosition,
        bezier: &egui::epaint::CubicBezierShape,
        game_state: &Option<crate::game_state::GameState>,
        animation: &std::collections::HashMap<crate::game_state::PlayerId, Vec<(Step, u64)>>,
        animation_counter: u64,
        comparator: fn(&Step, &ConnectorPosition, &ConnectorPosition) -> Option<bool>,
    ) {
        for (pid, start_is_conn1, player_color, start_time) in
            animation.iter().filter_map(|(pid, steps)| {
                let (step, start_time) = steps.first().unwrap();
                if let Some(to_reverse) = comparator(step, conn1, conn2) {
                    Some((
                        pid.clone(),
                        !to_reverse,
                        game_state.as_ref().unwrap().get_color([pid.clone()].into()),
                        *start_time,
                    ))
                } else {
                    None
                }
            })
        {
            let mut points = bezier.flatten(Some(cell_edge / 100.));
            if !start_is_conn1 {
                points.reverse();
            }
            let mut moved = cell_edge
                * EDGE_SHRINK_FACTOR
                * (animation_counter - start_time) as f32
                * ANIMATION_SPEED;
            while points.len() >= 2 {
                let d = points[1].distance(points[0]);
                if d < moved {
                    moved -= d;
                    points.remove(0);
                } else {
                    break;
                }
            }
            if points.len() < 2 {
                animations_done.push(pid)
            } else {
                let p = points[0] + (points[1] - points[0]).normalized() * moved;
                painter.circle_filled(
                    p,
                    cell_edge / 10. * ANIMATION_CIRCLE_SIZE_FACTOR,
                    player_color,
                );
            }
        }
    }

    fn show_statistics(&self, ui: &mut egui::Ui) {
        let game = self.game_state.as_ref().unwrap();
        for pid in game.player_ids.iter() {
            let statistics = game.player_statistics.get(pid).unwrap();
            let color = game.get_color([pid.clone()].into());
            let is_in_play = game.player_positions.get(pid).unwrap().is_some();
            let is_alive = if is_in_play { "•" } else { "x" };
            ui.colored_label(color, format!("Player #{pid} {is_alive}", pid = pid.0));
            ui.label(format!("Total move: {}", statistics.total_move()));
            ui.label(format!("Max move: {}", statistics.max_move()));
            if game.config.power_ups {
                let pickups = game.player_pickups.get(pid).unwrap();
                let pickups = if pickups.is_empty() {
                    "No Pickups".to_owned()
                } else {
                    pickups
                        .iter()
                        .map(crate::game_state::PowerUp::as_unicode_text)
                        .collect_vec()
                        .join("\t")
                };
                ui.label(format!("Pickups: {pickups}"));
            }
        }
        let max = game
            .player_statistics
            .iter()
            .map(|(_, x)| x.max_move())
            .max_by_key(|x| *x)
            .unwrap();
        if max > 0 && game.player_ids.len() >= 2 {
            let mut leaders = game
                .player_statistics
                .iter()
                .filter_map(|(id, s)| (s.max_move() == max).then_some(id.0))
                .collect_vec();
            let msg = if leaders.len() == 1 {
                let pid = leaders.pop().unwrap();
                if self.game_is_ongoing {
                    format!("Player {pid} is leading: {max}")
                } else {
                    format!("Player {pid} won with: {max}")
                }
            } else {
                let pids = leaders
                    .into_iter()
                    .map(|pid| pid.to_string())
                    .collect_vec()
                    .join(", ");
                if self.game_is_ongoing {
                    format!("Players {pids} are leading: {max}")
                } else {
                    format!("Players {pids} won with: {max}")
                }
            };
            ui.heading(msg);
        }
    }

    fn play_sound(&mut self) {
        #[cfg(not(debug_assertions))]
        if self.sound_manager.is_none() {
            log::debug!("Starting audio");
            use kira::{
                manager::{backend::DefaultBackend, AudioManager, AudioManagerSettings},
                sound::static_sound::StaticSoundData,
            };

            // Create an audio manager. This plays sounds and manages resources.
            let mut manager =
                AudioManager::<DefaultBackend>::new(AudioManagerSettings::default()).unwrap();
            let file = include_bytes!("../rendezvous.ogg");
            let cursor = std::io::Cursor::new(file);
            let sound_data = StaticSoundData::from_cursor(cursor).unwrap();
            manager.play(sound_data.loop_region(0.0..)).unwrap();
            self.sound_manager = Some(manager);
            log::debug!("Audio started");
        }
    }
}

fn draw_text(
    painter: &egui::Painter,
    text: String,
    angle_in_degrees: f32,
    pos: egui::Pos2,
    color: egui::Color32,
    font_size: f32,
) {
    let galley = painter.layout(text, egui::FontId::monospace(font_size), color, 0.);
    let angle = angle_in_degrees.to_radians();
    let offset = galley.size() / 2.;
    let offset = egui::vec2(
        offset.x * angle.cos() - offset.y * angle.sin(),
        offset.x * angle.sin() + offset.y * angle.cos(),
    );
    let p = pos - offset;
    painter.add(egui::Shape::Text(egui::epaint::TextShape {
        pos: p,
        galley,
        underline: egui::Stroke::NONE,
        override_text_color: None,
        angle,
        fallback_color: egui::Color32::GOLD,
        opacity_factor: 1.,
    }));
}

fn compute_connector_position(
    board_center: egui::Pos2,
    cell_edge: f32,
    connector: &ConnectorPosition,
) -> (egui::Pos2, egui::Vec2) {
    let ConnectorPosition {
        coordinate,
        connector,
    } = connector;
    let cell_center = compute_cell_center(board_center, cell_edge, coordinate);
    let edge_index = connector.side();
    let alpha = (60f32 * edge_index as f32).to_radians();
    let edge_center = cell_center
        + cell_edge * egui::vec2(alpha.cos(), alpha.sin()).rot90() * 3f32.sqrt() / 2.
            * EDGE_SHRINK_FACTOR;
    let factor = if connector.is_left() { -1 } else { 1 } as f32 * 4. / 7.;
    let edge_corner = edge_center + factor * cell_edge / 2. * egui::vec2(alpha.cos(), alpha.sin());
    let normal = egui::vec2(alpha.cos(), alpha.sin()).rot90();
    (edge_corner, normal)
}

fn compute_cell_center(
    board_center: egui::Pos2,
    cell_edge: f32,
    field: &BoardCoordinate,
) -> egui::Pos2 {
    let cell_center = board_center + cell_edge * egui::vec2(0., 3f32.sqrt()) * (field.x as f32);
    let alpha = 30f32.to_radians();
    let cell_center = cell_center
        + cell_edge * 3f32.sqrt() * egui::vec2(alpha.cos(), alpha.sin()) * (field.y as f32);
    cell_center
}

impl eframe::App for Hexagon {
    fn save(&mut self, storage: &mut dyn eframe::Storage) {
        eframe::set_value(storage, eframe::APP_KEY, self);
    }

    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        #[cfg(not(target_arch = "wasm32"))]
        self.play_sound();
        self.random_counter.add_assign(1);
        ctx.set_visuals(egui::Visuals::dark());
        egui::CentralPanel::default().show(ctx, |ui| {
            let stripbuilder = StripBuilder::new(ui);
            let is_first_frame = self.game_state.is_none();
            let stripbuilder = if is_first_frame {
                stripbuilder
            } else {
                stripbuilder
                    .size(Size::relative(0.1))
                    .size(Size::relative(0.2))
            };
            stripbuilder
                .size(Size::remainder())
                .horizontal(|mut strip| {
                    strip.cell(|ui| {
                        egui::ScrollArea::both().show(ui, |ui| {
                            fn show_config(input: &mut Hexagon, ui: &mut egui::Ui) {
                                if let Some(config) =
                                    input.game_configuration.show(ui, input.random_counter)
                                {
                                    input.game_is_ongoing = true;
                                    input.selected_code_io = None;
                                    input.game_state = Some(GameState::new(config).unwrap());
                                    input.play_sound();
                                    ui.ctx().request_repaint();
                                }
                            }
                            if is_first_frame {
                                show_config(self, ui);
                            } else {
                                ui.collapsing("New game", |ui| {
                                    show_config(self, ui);
                                    ui.separator();
                                });
                            }
                            if !is_first_frame {
                                if ui.button("Restart").clicked() {
                                    self.play_sound();
                                    self.game_is_ongoing = true;
                                    self.game_state = Some(
                                        GameState::new(
                                            self.game_state.as_ref().unwrap().config.clone(),
                                        )
                                        .unwrap(),
                                    );
                                    self.selected_code_io = None;
                                    return;
                                }
                                ui.separator();
                                self.show_statistics(ui);
                            }
                        });
                    });

                    if !is_first_frame {
                        strip.cell(|ui| {
                            egui::ScrollArea::both().show(ui, |ui| self.draw_selection(ui));
                        });
                        strip.cell(|ui| self.draw_board(ui));
                    }
                })
        });
    }
}

fn draw_hexagon(
    painter: &egui::Painter,
    center: egui::Pos2,
    cell_edge: f32,
    label: Option<&str>,
    border_color: egui::Color32,
    thickness: f32,
) {
    let alpha = 30.0f32.to_radians();
    let mut start = center - cell_edge * egui::vec2(alpha.sin(), alpha.cos());
    let mut points = Vec::new();
    points.push(start);
    for i in 0..6 {
        let alpha = (60.0 * (i as f32)).to_radians();
        let end = start + cell_edge * egui::vec2(alpha.cos(), alpha.sin());
        start = end;
        points.push(start);
    }
    points.push(points[1]); // ensures that boundary is nicely closed
    painter.line(points.clone(), egui::Stroke::new(thickness, border_color));
    if let Some(label) = label {
        painter.text(
            center,
            egui::Align2::CENTER_CENTER,
            label,
            Default::default(),
            egui::Color32::WHITE,
        );
    }
}
