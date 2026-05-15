use hexagon_engine::{
    CollisionMode, GameOptionsDelivery, GameOptionsStandard, GameState, OuterConnectors,
    RandomNumberGenerator, WinningCondition,
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
    fn start_game(self, seed: u32) -> Result<GameState, String> {
        match self {
            PresetOptions::Standard(mut o) => {
                o.random_seed = seed;
                o.start_game()
            }
            PresetOptions::Delivery(mut o) => {
                o.random_seed = seed;
                o.start_game()
            }
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
                winning_condition: WinningCondition::HighestVelocity,
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
                winning_condition: WinningCondition::LastManStanding,
                hand_size: 3,
            }),
        },
    ]
}

pub fn show(
    ui: &mut egui::Ui,
    game: &mut Option<GameState>,
    rng: &mut RandomNumberGenerator,
) -> bool {
    ui.heading("Predefined Games");
    let mut start_new_game = false;

    egui::Grid::new("predefined_games")
        .num_columns(2)
        .spacing([12.0, 6.0])
        .show(ui, |ui| {
            for preset in presets() {
                if ui.button(preset.name).clicked() {
                    match preset
                        .options
                        .start_game((rng.next_f64().abs() * (u32::MAX as f64).round()) as u32)
                    {
                        Ok(new_game) => {
                            *game = Some(new_game);
                            start_new_game = true;
                        }
                        Err(e) => eprintln!("Failed to load preset: {e}"),
                    }
                }
                ui.add(egui::Label::new(preset.description).wrap());
                ui.end_row();
            }
        });
    start_new_game
}
