use hexagon_engine::{
    CollisionMode, GameOptionsDelivery, GameOptionsStandard, GameState, OuterConnectors,
    WinningConditionStandard,
};

struct Preset {
    name: &'static str,
    description: &'static str,
    options: PresetOptions,
}

enum PresetOptions {
    Standard(GameOptionsStandard),
    Delivery(GameOptionsDelivery),
}

impl PresetOptions {
    fn start_game(self) -> Result<GameState, String> {
        match self {
            PresetOptions::Standard(o) => o.start_game(),
            PresetOptions::Delivery(o) => o.start_game(),
        }
    }
}

fn presets() -> Vec<Preset> {
    vec![
        Preset {
            name: "2-player classic",
            description: "Standard, radius 2, Highest Velocity",
            options: PresetOptions::Standard(GameOptionsStandard {
                board_radius: 2,
                outer_connectors: OuterConnectors::ReducedDeathEnds,
                random_seed: 0,
                player_count: 2,
                collision_mode: CollisionMode::PassThrough,
                winning_condition: WinningConditionStandard::HighestVelocity,
                hand_size: 3,
            }),
        },
        Preset {
            name: "Delivery solo",
            description: "Delivery, radius 2, 2 NPCs",
            options: PresetOptions::Delivery(GameOptionsDelivery {
                board_radius: 2,
                outer_connectors: OuterConnectors::ReducedDeathEnds,
                random_seed: 0,
                npc_count: 2,
                player_has_target: true,
                hand_size: 3,
            }),
        },
        Preset {
            name: "4-player chaos",
            description: "Standard, radius 3, Both Die, Last Man Standing",
            options: PresetOptions::Standard(GameOptionsStandard {
                board_radius: 3,
                outer_connectors: OuterConnectors::ReducedDeathEnds,
                random_seed: 0,
                player_count: 4,
                collision_mode: CollisionMode::BothDie,
                winning_condition: WinningConditionStandard::LastManStanding,
                hand_size: 3,
            }),
        },
    ]
}

pub fn show(ui: &mut egui::Ui, game: &mut Option<GameState>) {
    ui.heading("Predefined Games");

    egui::Grid::new("predefined_games")
        .num_columns(3)
        .spacing([12.0, 6.0])
        .show(ui, |ui| {
            for preset in presets() {
                ui.strong(preset.name);
                ui.label(preset.description);
                if ui.button("Load").clicked() {
                    match preset.options.start_game() {
                        Ok(new_game) => *game = Some(new_game),
                        Err(e) => eprintln!("Failed to load preset: {e}"),
                    }
                }
                ui.end_row();
            }
        });
}
