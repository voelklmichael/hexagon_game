use hexagon_engine::{CollisionMode, GameOptionsDiscriminants, OuterConnectors, WinningCondition};
use hexagon_types::WinningConditionHighscoreV1;

use crate::app::OptionsState;

pub fn show(ui: &mut egui::Ui, options: &mut OptionsState) -> bool {
    ui.horizontal(|ui| {
        ui.selectable_value(
            &mut options.selected,
            GameOptionsDiscriminants::Standard,
            "Standard",
        );
        ui.selectable_value(
            &mut options.selected,
            GameOptionsDiscriminants::Delivery,
            "Delivery",
        );
    });

    ui.separator();

    match options.selected {
        GameOptionsDiscriminants::Standard => show_standard(ui, options),
        GameOptionsDiscriminants::Delivery => show_delivery(ui, options),
        GameOptionsDiscriminants::Highscore | GameOptionsDiscriminants::HighscoreV2 => {}
    }

    ui.separator();
    ui.button("Start New Game").clicked()
}

fn int_buttons(ui: &mut egui::Ui, value: &mut usize, min: usize) {
    ui.horizontal(|ui| {
        for i in min..=7usize {
            if ui.selectable_label(*value == i, i.to_string()).clicked() {
                *value = i;
            }
        }
    });
}

fn outer_connectors_label(v: &OuterConnectors) -> &'static str {
    match v {
        OuterConnectors::OnlyDeathEnds => "Only death ends",
        OuterConnectors::ReducedDeathEnds => "Reduced death ends",
    }
}

fn outer_connectors_combo(ui: &mut egui::Ui, id: &str, value: &mut OuterConnectors) {
    egui::ComboBox::from_id_salt(id)
        .selected_text(outer_connectors_label(value))
        .show_ui(ui, |ui| {
            ui.selectable_value(value, OuterConnectors::OnlyDeathEnds, "Only death ends");
            ui.selectable_value(
                value,
                OuterConnectors::ReducedDeathEnds,
                "Reduced death ends",
            );
        });
}

fn show_standard(ui: &mut egui::Ui, options: &mut OptionsState) {
    let s = &mut options.standard;
    egui::Grid::new("options_standard")
        .num_columns(2)
        .spacing([20.0, 4.0])
        .show(ui, |ui| {
            ui.label("Board radius");
            int_buttons(ui, &mut s.board_radius, 1);
            ui.end_row();

            ui.label("Outer connectors");
            outer_connectors_combo(ui, "outer_connectors_standard", &mut s.outer_connectors);
            ui.end_row();

            ui.label("Random seed");
            ui.add(egui::DragValue::new(&mut s.random_seed));
            ui.end_row();

            ui.label("Player count");
            int_buttons(ui, &mut s.player_count, 2);
            ui.end_row();

            ui.label("Collision");
            egui::ComboBox::from_id_salt("collision_mode")
                .selected_text(match s.collision_mode {
                    CollisionMode::PassThrough => "Pass through",
                    CollisionMode::BothDie => "Both die",
                })
                .show_ui(ui, |ui| {
                    ui.selectable_value(
                        &mut s.collision_mode,
                        CollisionMode::PassThrough,
                        "Pass through",
                    );
                    ui.selectable_value(&mut s.collision_mode, CollisionMode::BothDie, "Both die");
                });
            ui.end_row();

            ui.label("Winning condition");
            egui::ComboBox::from_id_salt("winning_condition")
                .selected_text(match s.winning_condition {
                    WinningCondition::LastManStanding => "Last man standing",
                    WinningCondition::LongestWay => "Longest way",
                    WinningCondition::HighestVelocity => "Highest velocity",
                    WinningCondition::Highscore(WinningConditionHighscoreV1 { .. }) => "Highscore",
                })
                .show_ui(ui, |ui| {
                    ui.selectable_value(
                        &mut s.winning_condition,
                        WinningCondition::LastManStanding,
                        "Last man standing",
                    );
                    ui.selectable_value(
                        &mut s.winning_condition,
                        WinningCondition::LongestWay,
                        "Longest way",
                    );
                    ui.selectable_value(
                        &mut s.winning_condition,
                        WinningCondition::HighestVelocity,
                        "Highest velocity",
                    );
                });
            ui.end_row();

            ui.label("Hand size");
            int_buttons(ui, &mut s.hand_size, 1);
            ui.end_row();
        });
}

fn show_delivery(ui: &mut egui::Ui, options: &mut OptionsState) {
    let d = &mut options.delivery;
    egui::Grid::new("options_delivery")
        .num_columns(2)
        .spacing([20.0, 4.0])
        .show(ui, |ui| {
            ui.label("Board radius");
            int_buttons(ui, &mut d.board_radius, 1);
            ui.end_row();

            ui.label("Outer connectors");
            outer_connectors_combo(ui, "outer_connectors_delivery", &mut d.outer_connectors);
            ui.end_row();

            ui.label("Random seed");
            ui.add(egui::DragValue::new(&mut d.random_seed));
            ui.end_row();

            ui.label("NPC count");
            int_buttons(ui, &mut d.npc_count, 0);
            ui.end_row();

            ui.label("Player has target");
            ui.checkbox(&mut d.player_has_target, "");
            ui.end_row();

            ui.label("Hand size");
            int_buttons(ui, &mut d.hand_size, 1);
            ui.end_row();
        });
}
