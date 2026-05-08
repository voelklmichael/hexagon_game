use hexagon_engine::{PlayerId, Statistics};

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

pub fn show(ui: &mut egui::Ui, rendering_data: &RenderingData, statistics: Option<&Statistics>) {
    ui.heading("Statistics");

    let Some(statistics) = statistics else {
        ui.label("No game in progress.");
        return;
    };

    let players: Vec<PlayerId> = rendering_data.player_colors.keys().copied().collect();

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
}
