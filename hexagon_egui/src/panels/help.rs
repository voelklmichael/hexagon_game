pub fn show(ui: &mut egui::Ui) {
    ui.heading("Help & Tutorial");

    ui.collapsing("How to play", |ui| {
        ui.label(
            "Select a tile from your hand, rotate it if needed, then press \"Play selected tile\".",
        );
        ui.label("Players move automatically along the connected path after each tile is placed.");
        ui.label("The game ends when all active players have left the board.");
    });

    ui.collapsing("Connector types", |ui| {
        egui::Grid::new("connector_types")
            .num_columns(2)
            .spacing([12.0, 4.0])
            .show(ui, |ui| {
                ui.strong("OnHex");
                ui.label("A curve within a single hexagon, connecting two of its edge points.");
                ui.end_row();
                ui.strong("HexToHex");
                ui.label("A passage between two adjacent hexagons.");
                ui.end_row();
                ui.strong("Outside");
                ui.label("A passage connecting two non-adjacent outer edge points.");
                ui.end_row();
                ui.strong("DeadEnd");
                ui.label("An outer edge point with no continuation — players stop or die here.");
                ui.end_row();
            });
    });

    ui.collapsing("Winning conditions", |ui| {
        egui::Grid::new("winning_conditions")
            .num_columns(2)
            .spacing([12.0, 4.0])
            .show(ui, |ui| {
                ui.strong("Last Man Standing");
                ui.label("The last active player wins.");
                ui.end_row();
                ui.strong("Longest Way");
                ui.label("The player who travelled the greatest total path weight wins.");
                ui.end_row();
                ui.strong("Highest Velocity");
                ui.label("The player with the greatest single-turn speed wins.");
                ui.end_row();
            });
    });

    ui.collapsing("Controls reference", |ui| {
        egui::Grid::new("controls_ref")
            .num_columns(2)
            .spacing([12.0, 4.0])
            .show(ui, |ui| {
                ui.strong("Click tile");
                ui.label("Select / deselect a tile from your hand.");
                ui.end_row();
                ui.strong("↺ / ↻");
                ui.label("Rotate the tile counter-clockwise or clockwise.");
                ui.end_row();
                ui.strong("Play selected tile");
                ui.label("Place the selected tile and advance all players.");
                ui.end_row();
                ui.strong("Undo / Redo");
                ui.label("Step back or forward through tile placements.");
                ui.end_row();
                ui.strong("Restart");
                ui.label("Replay the current game from the start with the same seed.");
                ui.end_row();
                ui.strong("New game");
                ui.label("Start a new game with the same options but a new random seed.");
                ui.end_row();
            });
    });
}
