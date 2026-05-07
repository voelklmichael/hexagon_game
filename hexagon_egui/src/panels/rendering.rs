use hexagon_engine::Color;
use strum::VariantArray;

use crate::app::RenderingData;

fn color_to_egui(color: Color) -> egui::Color32 {
    let hex = color.to_svg_string();
    let r = u8::from_str_radix(&hex[1..3], 16).unwrap_or(0);
    let g = u8::from_str_radix(&hex[3..5], 16).unwrap_or(0);
    let b = u8::from_str_radix(&hex[5..7], 16).unwrap_or(0);
    egui::Color32::from_rgb(r, g, b)
}

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
    ui.heading("Rendering");

    ui.separator();
    ui.label("Board Colors");
    color_selector(ui, "Hex Fill", &mut data.hex_fill);
    color_selector(ui, "Hex Stroke", &mut data.hex_stroke);
    color_selector(ui, "Highlighted Hex Fill", &mut data.highlighted_hex_fill);
    color_selector(ui, "Highlighted Hex Stroke", &mut data.highlighted_hex_stroke);
    color_selector(ui, "Dead End", &mut data.dead_end_color);
    color_selector(ui, "Closed Loop", &mut data.closed_loop_color);
    color_selector(ui, "Unused", &mut data.unused_color);

    ui.separator();
    ui.label("Player Colors");
    for (player_id, color) in &mut data.player_colors {
        color_selector(ui, &format!("Player {}", player_id.0), color);
    }
}
