use hexagon_engine::GameState;

use crate::app::GameHistory;

pub fn show(ui: &mut egui::Ui, game: &mut Option<GameState>, history: &mut GameHistory) -> bool {
    let has_game = game.is_some();
    let can_undo = !history.undo_stack.is_empty();
    let can_redo = !history.redo_stack.is_empty();
    let mut started = false;
    let btn_font_size = egui::TextStyle::Body.resolve(ui.style()).size * 1.4;

    ui.horizontal(|ui| {
        if ui
            .add_enabled(has_game, egui::Button::new(egui::RichText::new("Restart this game").size(btn_font_size)))
            .clicked()
            && let Some(current) = game
        {
            let opts = current.options.clone();
            match opts.start_game() {
                Ok(new_game) => {
                    history.undo_stack.clear();
                    history.redo_stack.clear();
                    *game = Some(new_game);
                    started = true;
                }
                Err(e) => eprintln!("Restart failed: {e}"),
            }
        }

        if ui
            .add_enabled(has_game, egui::Button::new(egui::RichText::new("New game").size(btn_font_size)))
            .clicked()
            && let Some(current) = game
        {
            let mut opts = current.options.clone();
            opts.randomize_seed();
            match opts.start_game() {
                Ok(new_game) => {
                    history.undo_stack.clear();
                    history.redo_stack.clear();
                    *game = Some(new_game);
                    started = true;
                }
                Err(e) => eprintln!("New game failed: {e}"),
            }
        }

        if ui
            .add_enabled(can_undo, egui::Button::new(egui::RichText::new("Undo").size(btn_font_size)))
            .clicked()
            && let Some(prev) = history.undo_stack.pop()
        {
            if let Some(current) = game.take() {
                history.redo_stack.push(current);
            }
            *game = Some(prev);
        }

        if ui
            .add_enabled(can_redo, egui::Button::new(egui::RichText::new("Redo").size(btn_font_size)))
            .clicked()
            && let Some(next) = history.redo_stack.pop()
        {
            if let Some(current) = game.take() {
                history.undo_stack.push(current);
            }
            *game = Some(next);
        }
    });
    started
}
