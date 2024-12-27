mod config_ui;

use std::ops::AddAssign;

use egui_extras::{Size, StripBuilder};
use itertools::Itertools;

use crate::{
    game_state::{
        BoardCoordinate, BoardField, ConnectorCoordinate, ConnectorPosition, GameState, HexagonCode,
    },
    permutations,
};

const EDGE_SHRINK_FACTOR: f32 = 0.95;
#[derive(serde::Deserialize, serde::Serialize, Default)]
pub struct Hexagon {
    random_counter: u64,
    game_configuration: config_ui::GameConfigurationUI,
    selected_code_io: Option<HexagonCode>,
    game_state: Option<crate::game_state::GameState>,
    game_is_ongoing: bool,
    #[serde(skip)]
    sound_manager: Option<kira::manager::AudioManager<kira::manager::DefaultBackend>>,
}
impl Hexagon {
    pub fn new(cc: &eframe::CreationContext<'_>) -> Self {
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
        if let Some(code) = &self.selected_code_io {
            if ui.button("Choose").clicked() {
                if self.game_state.as_mut().unwrap().play_by_code(code) {
                    self.game_is_ongoing = false;
                    return;
                }
                self.play_sound();
                self.selected_code_io = None;
            }
        } else {
            ui.label("Please select edge");
        }
        let (response, painter) =
            ui.allocate_painter(ui.available_size(), egui::Sense::click_and_drag());
        // background
        painter.rect_filled(response.rect, egui::Rounding::ZERO, egui::Color32::BLACK);

        let game = self.game_state.as_ref().unwrap();
        let selectable_tiles = game.player_tiles.get(&game.current_player).unwrap();
        let Some((player_position, player_color)) = game.get_player_position_color() else {
            panic!("This should never happen");
        };

        let total_size = response.rect.height().min(response.rect.width());
        let edge_size = total_size / (selectable_tiles.len() as f32 + 0.5);

        for (offset_index, &code) in selectable_tiles.iter().enumerate() {
            let mut border_color = egui::Color32::GRAY;
            let mut selected_io = None;

            if let Some(HexagonCode { code: c, rotation }) = self.selected_code_io {
                if code == c {
                    selected_io = Some(rotation);
                    border_color = player_color;
                }
            }
            let center = response.rect.center_top();
            let center = center + egui::vec2(0., edge_size * (2 * offset_index + 1) as f32);
            draw_hexagon(
                &painter,
                center,
                edge_size,
                None,
                border_color,
                edge_size / 30.,
            );
            let permutation = permutations::int_to_array(code);
            for i in 0..12u8 {
                let j = permutation[i as usize];
                if j < i {
                    continue;
                }
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
                if let Some(mouse) = response.interact_pointer_pos()
                    && response.clicked()
                {
                    for (k, p) in [(i, pi), (j, pj)] {
                        if mouse.distance(p) / edge_size < 1. / 5. {
                            let selected = HexagonCode {
                                code,
                                rotation: k / 2,
                            };
                            if self.selected_code_io.as_ref() == Some(&selected) {
                                self.selected_code_io = None;
                            } else {
                                self.selected_code_io = Some(selected);
                            }
                            break;
                        }
                    }
                }
                let factor = edge_size / 3.;
                let bezier = egui::epaint::CubicBezierShape::from_points_stroke(
                    [pi, pi - ni * factor, pj - nj * factor, pj],
                    false,
                    egui::Color32::TRANSPARENT,
                    egui::Stroke::new(edge_size / 30., egui::Color32::GOLD),
                );
                painter.add(bezier);
            }

            if let Some(rotation) = selected_io {
                let c = (player_position.connector.0 % 2 + 2 * rotation) % 12;
                let (p, _) = compute_connector_position(
                    center,
                    edge_size,
                    &ConnectorPosition {
                        coordinate: BoardCoordinate { x: 0, y: 0 },
                        connector: ConnectorCoordinate(c),
                    },
                );
                painter.circle_filled(p, edge_size / 6., player_color);
            }
        }
    }

    fn draw_board(&mut self, ui: &mut egui::Ui) {
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
        for (c1, c2) in game_state.board.connections.field_connectors.iter() {
            let color = {
                let players = game_state.board_usage.get_players(c1);
                game_state.get_color(players)
            };
            let (c1, n1) = compute_connector_position(board_center, cell_edge, c1);
            let (c2, n2) = compute_connector_position(board_center, cell_edge, c2);
            let factor = cell_edge / 30.;
            painter.line_segment(
                [c1 - n1 * factor, c2 - n2 * factor],
                egui::Stroke::new(bounding_hexagon_edge / 100., color),
            );
        }
        for (c1, c2) in game_state.board.connections.remaining_connectors.iter() {
            let color = {
                let players = game_state.board_usage.get_players(c1);
                game_state.get_color(players)
            };
            let (c1, n1) = compute_connector_position(board_center, cell_edge, c1);
            let (c2, n2) = compute_connector_position(board_center, cell_edge, c2);
            let factor = c1.distance(c2) / 3.;
            let bezier = egui::epaint::CubicBezierShape::from_points_stroke(
                [c1, c1 + n1 * factor, c2 + n2 * factor, c2],
                false,
                egui::Color32::TRANSPARENT,
                egui::Stroke::new(cell_edge / 30., color),
            );
            painter.add(bezier);
        }
        for (c1, c2) in game_state.board.connections.inner_connectors.iter() {
            let color = {
                let players = game_state.board_usage.get_players(c1);
                game_state.get_color(players)
            };
            let (c1, n1) = compute_connector_position(board_center, cell_edge, c1);
            let (c2, n2) = compute_connector_position(board_center, cell_edge, c2);
            let factor = cell_edge / 3.;
            let bezier = egui::epaint::CubicBezierShape::from_points_stroke(
                [c1, c1 - n1 * factor, c2 - n2 * factor, c2],
                false,
                egui::Color32::TRANSPARENT,
                egui::Stroke::new(cell_edge / 30., color),
            );
            painter.add(bezier);
        }
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
        }
        let max = game
            .player_statistics
            .iter()
            .map(|(_, x)| x.max_move())
            .max_by_key(|x| *x)
            .unwrap();
        if max > 0 {
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
        //#[cfg(not(debug_assertions))]
        //#[cfg(not(target_arch = "wasm32"))]
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
    /// Called each time the UI needs repainting, which may be many times per second.
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        #[cfg(not(target_arch = "wasm32"))]
        self.play_sound();
        self.random_counter.add_assign(1);
        ctx.set_visuals(egui::Visuals::dark());
        egui::CentralPanel::default().show(ctx, |ui| {
            StripBuilder::new(ui)
                .size(Size::relative(0.2))
                .size(Size::remainder())
                .horizontal(|mut strip| {
                    strip.cell(|ui| {
                        if !self.game_is_ongoing {
                            if let Some(config) =
                                self.game_configuration.show(ui, self.random_counter)
                            {
                                self.game_is_ongoing = true;
                                self.game_state = Some(GameState::new(config).unwrap());
                                self.play_sound();
                            }
                            ui.separator();
                        }
                        if self.game_state.is_some() {
                            if ui.button("Restart").clicked() {
                                self.play_sound();
                                self.game_is_ongoing = false;
                                return;
                            }
                            ui.separator();
                            self.show_statistics(ui);
                            ui.separator();
                            self.draw_selection(ui);
                        }
                    });
                    if self.game_state.is_some() {
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
