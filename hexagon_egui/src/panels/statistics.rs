use hexagon_engine::{PlayerId, Statistics};
use hexagon_types::DBHighscorePeak;

use crate::app::RenderingData;
use crate::panels::color_to_egui;

fn highlight_label(ui: &mut egui::Ui, value: u32, max: u32) {
    let text = egui::RichText::new(value.to_string());
    let text = if value == max {
        text.strong().color(egui::Color32::GOLD)
    } else {
        text
    };
    ui.label(text);
}

fn show_peak_section(
    ui: &mut egui::Ui,
    heading: &str,
    peak: &DBHighscorePeak,
    rendering_data: &RenderingData,
) {
    ui.add_space(6.0);
    ui.label(egui::RichText::new(heading).strong());

    if peak.players.is_empty() {
        ui.label("No data yet.");
        return;
    }

    let mut players: Vec<u8> = peak.players.keys().copied().collect();
    players.sort_unstable();

    let max_velocity = peak
        .players
        .values()
        .map(|s| s.max_velocity)
        .max()
        .unwrap_or(0);
    let max_distance = peak
        .players
        .values()
        .map(|s| s.total_distance)
        .max()
        .unwrap_or(0);

    egui::Grid::new(heading)
        .striped(true)
        .min_col_width(80.0)
        .show(ui, |ui| {
            ui.label("Player");
            ui.label("Distance");
            ui.label("Max Velocity");
            ui.end_row();

            for &idx in &players {
                let Some(stats) = peak.players.get(&idx) else { continue };
                let player_id = PlayerId(idx as u32);
                if let Some(&color) = rendering_data.player_colors.get(&player_id) {
                    let (rect, _) =
                        ui.allocate_exact_size(egui::vec2(14.0, 14.0), egui::Sense::hover());
                    ui.painter().rect_filled(rect, 2.0, color_to_egui(color));
                } else {
                    ui.label(format!("P{idx}"));
                }
                let mv = stats.max_velocity as u32;
                let td = stats.total_distance as u32;
                let text_td = egui::RichText::new(td.to_string());
                let text_td = if stats.total_distance == max_distance {
                    text_td.strong().color(egui::Color32::GOLD)
                } else {
                    text_td
                };
                ui.label(text_td);
                let text_mv = egui::RichText::new(mv.to_string());
                let text_mv = if stats.max_velocity == max_velocity {
                    text_mv.strong().color(egui::Color32::GOLD)
                } else {
                    text_mv
                };
                ui.label(text_mv);
                ui.end_row();
            }
        });
}

pub fn show(
    ui: &mut egui::Ui,
    rendering_data: &RenderingData,
    statistics: Option<&Statistics>,
    user_best: Option<&DBHighscorePeak>,
    overall_best: Option<&DBHighscorePeak>,
) {
    ui.heading("Statistics");

    let Some(statistics) = statistics else {
        ui.label("No game in progress.");
        return;
    };

    let mut players: Vec<PlayerId> = statistics.total_path_segments.keys().copied().collect();
    players.sort_unstable_by_key(|id| id.0);

    let max_segments = players
        .iter()
        .filter_map(|id| statistics.total_path_segments.get(id))
        .copied()
        .max()
        .unwrap_or(0);
    let max_weight = players
        .iter()
        .filter_map(|id| statistics.total_path_weight.get(id))
        .copied()
        .max()
        .unwrap_or(0);
    let max_velocity = players
        .iter()
        .filter_map(|id| statistics.max_velocity.get(id))
        .copied()
        .max()
        .unwrap_or(0);

    egui::Grid::new("statistics_grid")
        .striped(true)
        .min_col_width(80.0)
        .show(ui, |ui| {
            ui.label("Player");
            ui.label("Segments");
            ui.label("Total Weight");
            ui.label("Max Velocity");
            ui.end_row();

            for player_id in &players {
                if let Some(&color) = rendering_data.player_colors.get(player_id) {
                    let (rect, _) =
                        ui.allocate_exact_size(egui::vec2(14.0, 14.0), egui::Sense::hover());
                    ui.painter().rect_filled(rect, 2.0, color_to_egui(color));
                }
                let segments = statistics
                    .total_path_segments
                    .get(player_id)
                    .copied()
                    .unwrap_or(0);
                let weight = statistics
                    .total_path_weight
                    .get(player_id)
                    .copied()
                    .unwrap_or(0);
                let velocity = statistics.max_velocity.get(player_id).copied().unwrap_or(0);
                highlight_label(ui, segments, max_segments);
                highlight_label(ui, weight, max_weight);
                highlight_label(ui, velocity, max_velocity);
                ui.end_row();
            }
        });

    if let Some(peak) = user_best {
        show_peak_section(ui, "Your Previous Best", peak, rendering_data);
    }
    if let Some(peak) = overall_best {
        show_peak_section(ui, "Mission Record", peak, rendering_data);
    }
}
