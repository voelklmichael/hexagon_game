pub mod controls;
pub mod game_board;
pub mod game_state_json;
pub mod hand;
pub mod music;
pub mod options;
pub mod rendering;
pub mod statistics;

pub(crate) fn color_to_egui(color: hexagon_engine::Color) -> egui::Color32 {
    let hex = color.to_svg_string();
    let r = u8::from_str_radix(&hex[1..3], 16).unwrap_or(0);
    let g = u8::from_str_radix(&hex[3..5], 16).unwrap_or(0);
    let b = u8::from_str_radix(&hex[5..7], 16).unwrap_or(0);
    egui::Color32::from_rgb(r, g, b)
}
