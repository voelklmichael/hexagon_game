use hexagon_engine::{GameOptions, PlayerId, Statistics};
use hexagon_types::DBHighscorePeak;

use crate::app::RenderingData;
use crate::panels::color_to_egui;

fn show_header(ui: &mut egui::Ui) {
    ui.label("");
    ui.label("↔").on_hover_text("Total traveled distance");
    ui.label("⚡").on_hover_text("Maximum velocity");
    ui.end_row();
}

fn show_color_swatch(ui: &mut egui::Ui, player_id: PlayerId, rendering_data: &RenderingData) {
    if let Some(&color) = rendering_data.player_colors.get(&player_id) {
        let (rect, _) = ui.allocate_exact_size(egui::vec2(14.0, 14.0), egui::Sense::hover());
        ui.painter().rect_filled(rect, 2.0, color_to_egui(color));
    } else {
        ui.label(format!("P{}", player_id.0));
    }
}

fn highlight_label(ui: &mut egui::Ui, value: i64, max: i64, highlight: bool) {
    let text = egui::RichText::new(format!("{:.1}", value as f32 / 1000.0));
    let text = if highlight && value == max {
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
    highlight: bool,
) {
    ui.add_space(6.0);
    ui.label(egui::RichText::new(heading).strong());

    if peak.players.is_empty() {
        ui.label("No data yet.");
        return;
    }

    let mut players: Vec<u8> = peak.players.keys().copied().collect();
    players.sort_unstable();

    let max_distance = peak
        .players
        .values()
        .map(|s| s.total_distance)
        .max()
        .unwrap_or(0);
    let max_velocity = peak
        .players
        .values()
        .map(|s| s.max_velocity)
        .max()
        .unwrap_or(0);

    egui::Grid::new(heading).striped(true).show(ui, |ui| {
        show_header(ui);

        for &idx in &players {
            let Some(stats) = peak.players.get(&idx) else {
                continue;
            };
            show_color_swatch(ui, PlayerId(idx as u32), rendering_data);
            highlight_label(ui, stats.total_distance, max_distance, highlight);
            highlight_label(ui, stats.max_velocity, max_velocity, highlight);
            ui.end_row();
        }
    });
}

pub fn show(
    ui: &mut egui::Ui,
    rendering_data: &RenderingData,
    statistics: Option<&Statistics>,
    game_options: Option<&GameOptions>,
    user_best: Option<&DBHighscorePeak>,
    overall_best: Option<&DBHighscorePeak>,
) {
    let Some(statistics) = statistics else {
        ui.label("No game in progress.");
        return;
    };

    let highlight = !matches!(game_options, Some(GameOptions::Highscore(_)));

    let mut players: Vec<PlayerId> = statistics.total_path_segments.keys().copied().collect();
    players.sort_unstable_by_key(|id| id.0);

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
        .show(ui, |ui| {
            show_header(ui);

            for player_id in &players {
                show_color_swatch(ui, *player_id, rendering_data);
                let weight = statistics
                    .total_path_weight
                    .get(player_id)
                    .copied()
                    .unwrap_or(0);
                let velocity = statistics.max_velocity.get(player_id).copied().unwrap_or(0);
                highlight_label(ui, weight as i64, max_weight as i64, highlight);
                highlight_label(ui, velocity as i64, max_velocity as i64, highlight);
                ui.end_row();
            }
        });

    if let Some(peak) = user_best {
        show_peak_section(ui, "Your Previous Best", peak, rendering_data, highlight);
    }
    if let Some(peak) = overall_best {
        show_peak_section(ui, "Mission Record", peak, rendering_data, highlight);
    }
}
