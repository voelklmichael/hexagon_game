use hexagon_engine::Color;
use strum::VariantArray;

use crate::app::RenderingData;
use crate::panels::color_to_egui;

fn color_selector(ui: &mut egui::Ui, label: &str, color: &mut Color) {
    ui.horizontal(|ui| {
        let (rect, _) = ui.allocate_exact_size(egui::vec2(20.0, 16.0), egui::Sense::hover());
        ui.painter().rect_filled(rect, 3.0, color_to_egui(*color));
        egui::ComboBox::from_label(label)
            .selected_text(format!("{color:?}"))
            .show_ui(ui, |ui| {
                for &variant in Color::VARIANTS {
                    ui.selectable_value(color, variant, format!("{variant:?}"));
                }
            });
    });
}

pub fn show(ui: &mut egui::Ui, data: &mut RenderingData) {
    ui.label("Board Colors");
    color_selector(ui, "Background", &mut data.background);
    color_selector(ui, "Hex Fill", &mut data.hex_fill);
    color_selector(ui, "Hex Stroke", &mut data.hex_stroke);
    color_selector(ui, "Highlighted Hex Fill", &mut data.highlighted_hex_fill);
    color_selector(
        ui,
        "Highlighted Hex Stroke",
        &mut data.highlighted_hex_stroke,
    );
    color_selector(ui, "Dead End", &mut data.dead_end_color);
    color_selector(ui, "Closed Loop", &mut data.closed_loop_color);
    color_selector(ui, "Unused", &mut data.unused_color);

    ui.separator();
    ui.label("Player Colors");
    for (player_id, color) in &mut data.player_colors {
        color_selector(ui, &format!("Player {}", player_id.0), color);
    }
}
