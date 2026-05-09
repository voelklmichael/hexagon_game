use hexagon_engine::GameState;

pub fn show(ui: &mut egui::Ui, game: Option<&GameState>) {
    ui.heading("Game State JSON");

    let json = match game {
        Some(g) => serde_json::to_string_pretty(g).unwrap_or_else(|e| e.to_string()),
        None => String::from("No game in progress."),
    };

    ui.horizontal(|ui| {
        if ui.button("Copy").clicked() {
            ui.ctx().copy_text(json.clone());
        }
    });

    egui::ScrollArea::vertical().show(ui, |ui| {
        ui.add(
            egui::TextEdit::multiline(&mut json.as_str())
                .font(egui::TextStyle::Monospace)
                .desired_width(f32::INFINITY),
        );
    });
}
