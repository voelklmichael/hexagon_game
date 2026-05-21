pub fn show(ui: &mut egui::Ui, missions_available: bool) -> bool {
    let mut start_clicked = false;
    ui.vertical_centered(|ui| {
        ui.add_space(ui.available_height() / 3.0);
        ui.label(egui::RichText::new("Introduction").size(48.0));
        ui.add_space(24.0);
        if ui
            .add_enabled(
                missions_available,
                egui::Button::new(egui::RichText::new("Start game").size(20.0))
                    .min_size(egui::vec2(160.0, 40.0)),
            )
            .clicked()
        {
            start_clicked = true;
        }
    });
    start_clicked
}
