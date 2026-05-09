pub fn show(ui: &mut egui::Ui) {
    ui.heading("Help & Tutorial");

    ui.collapsing("How to play", |ui| {
        ui.add(egui::Label::new("Select a tile from your hand, rotate it if needed, then press \"Play selected tile\".").wrap());
        ui.add(egui::Label::new("Players move automatically along the connected path after each tile is placed.").wrap());
        ui.add(egui::Label::new("The game ends when all active players have left the board.").wrap());
    });

    ui.collapsing("Connector types", |ui| {
        egui::Grid::new("connector_types")
            .num_columns(2)
            .spacing([12.0, 4.0])
            .show(ui, |ui| {
                ui.strong("OnHex");
                ui.add(egui::Label::new("A curve within a single hexagon, connecting two of its edge points.").wrap());
                ui.end_row();
                ui.strong("HexToHex");
                ui.add(egui::Label::new("A passage between two adjacent hexagons.").wrap());
                ui.end_row();
                ui.strong("Outside");
                ui.add(egui::Label::new("A passage connecting two non-adjacent outer edge points.").wrap());
                ui.end_row();
                ui.strong("DeadEnd");
                ui.add(egui::Label::new("An outer edge point with no continuation — players stop or die here.").wrap());
                ui.end_row();
            });
    });

    ui.collapsing("Winning conditions", |ui| {
        egui::Grid::new("winning_conditions")
            .num_columns(2)
            .spacing([12.0, 4.0])
            .show(ui, |ui| {
                ui.strong("Last Man Standing");
                ui.add(egui::Label::new("The last active player wins.").wrap());
                ui.end_row();
                ui.strong("Longest Way");
                ui.add(egui::Label::new("The player who travelled the greatest total path weight wins.").wrap());
                ui.end_row();
                ui.strong("Highest Velocity");
                ui.add(egui::Label::new("The player with the greatest single-turn speed wins.").wrap());
                ui.end_row();
            });
    });

    ui.collapsing("Controls reference", |ui| {
        egui::Grid::new("controls_ref")
            .num_columns(2)
            .spacing([12.0, 4.0])
            .show(ui, |ui| {
                ui.strong("Click tile");
                ui.add(egui::Label::new("Select / deselect a tile from your hand.").wrap());
                ui.end_row();
                ui.strong("↺ / ↻");
                ui.add(egui::Label::new("Rotate the tile counter-clockwise or clockwise.").wrap());
                ui.end_row();
                ui.strong("➡");
                ui.add(egui::Label::new("Play the selected tile and advance all players.").wrap());
                ui.end_row();
                ui.strong("Undo / Redo");
                ui.add(egui::Label::new("Step back or forward through tile placements.").wrap());
                ui.end_row();
                ui.strong("Restart");
                ui.add(egui::Label::new("Replay the current game from the start with the same seed.").wrap());
                ui.end_row();
                ui.strong("New game");
                ui.add(egui::Label::new("Start a new game with the same options but a new random seed.").wrap());
                ui.end_row();
            });
    });
}
