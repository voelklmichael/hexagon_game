use crate::game_state::GameConfiguration;

#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub struct GameConfigurationUI {
    player_count: u32,
    board_size: u32,
    tiles_per_player: u32,
    random_seed: String,
    auto_random: bool,
    error: Option<String>,
    power_ups: bool,
}

impl GameConfigurationUI {
    #[must_use]
    pub(crate) fn show(
        &mut self,
        ui: &mut egui::Ui,
        random_counter: u64,
    ) -> Option<GameConfiguration> {
        for (label, input, min, max) in [
            ("Player Count", &mut self.player_count, 1, 6),
            ("Board Size", &mut self.board_size, 3, 12),
            ("Tiles/Player", &mut self.tiles_per_player, 1, 5),
        ] {
            ui.heading(label);
            ui.with_layout(
                egui::Layout::left_to_right(egui::Align::LEFT).with_main_wrap(true),
                |ui| {
                    for i in min..=max {
                        let button = ui.button(i.to_string());
                        let button = if i == *input {
                            button.highlight()
                        } else {
                            button
                        };
                        if button.clicked() {
                            *input = i;
                        }
                    }
                },
            );
        }
        if self.player_count >= 2 {
            ui.heading("Power Ups?");
            ui.checkbox(&mut self.power_ups, "");
        }
        {
            ui.heading("Random?");
            ui.checkbox(&mut self.auto_random, "");
        }
        if !self.auto_random {
            ui.heading("Seed");
            ui.text_edit_singleline(&mut self.random_seed);
        }

        if let Some(error) = &self.error {
            ui.colored_label(egui::Color32::RED, error);
        }
        if ui.button("Start").clicked() {
            match self.parse(random_counter) {
                Ok(config) => match config.validate() {
                    Ok(()) => Some(config),
                    Err(e) => {
                        self.error = Some(e);
                        None
                    }
                },
                Err(e) => {
                    self.error = Some(e);
                    None
                }
            }
        } else {
            None
        }
    }

    fn parse(&mut self, random_seed: u64) -> Result<GameConfiguration, String> {
        let random_seed = if self.auto_random {
            random_seed
        } else {
            match self.random_seed.parse() {
                Ok(x) => x,
                Err(_) => return Err("Random seed invalid".into()),
            }
        };
        Ok(GameConfiguration {
            player_count: self.player_count as _,
            random_seed,
            board_size: self.board_size as _,
            tiles_per_player: self.tiles_per_player as _,
            power_ups: self.power_ups,
        })
    }
}

impl Default for GameConfigurationUI {
    fn default() -> Self {
        let GameConfiguration {
            player_count,
            random_seed,
            board_size,
            tiles_per_player,
            power_ups,
        } = GameConfiguration::default();
        Self {
            player_count: player_count as _,
            board_size: board_size as _,
            tiles_per_player: tiles_per_player as _,
            random_seed: random_seed.to_string(),
            auto_random: true,
            error: None,
            power_ups,
        }
    }
}
