use crate::game_state::GameConfiguration;

#[derive(Debug, serde::Deserialize, serde::Serialize)]
pub struct GameConfigurationUI {
    pub player_count: String,
    pub board_size: String,
    pub tiles_per_player: String,
    pub random_seed: String,
    pub auto_random: bool,
    pub error: Option<String>,
}

impl GameConfigurationUI {
    #[must_use]
    pub(crate) fn show(
        &mut self,
        ui: &mut egui::Ui,
        random_counter: u64,
    ) -> Option<GameConfiguration> {
        egui::Grid::new("ConfigurationGrid")
            .num_columns(2)
            .show(ui, |ui| {
                for (label, input) in [
                    ("Players", &mut self.player_count),
                    ("Board Size", &mut self.board_size),
                    ("Tiles/Player", &mut self.tiles_per_player),
                ] {
                    ui.label(label);
                    ui.text_edit_singleline(input);
                    ui.end_row();
                }
                {
                    ui.label("Random?");
                    ui.checkbox(&mut self.auto_random, "");
                    ui.end_row();
                }
                if !self.auto_random {
                    ui.label("Seed");
                    ui.text_edit_singleline(&mut self.random_seed);
                    ui.end_row();
                }
            });
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
        let player_count: usize = match self.player_count.parse() {
            Ok(x) => x,
            Err(_) => return Err("Player count invalid".into()),
        };
        let board_size: usize = match self.board_size.parse() {
            Ok(x) => x,
            Err(_) => return Err("Board size invalid".into()),
        };
        let tiles_per_player: usize = match self.tiles_per_player.parse() {
            Ok(x) => x,
            Err(_) => return Err("Tiles/player invalid".into()),
        };
        let random_seed = if self.auto_random {
            random_seed
        } else {
            match self.random_seed.parse() {
                Ok(x) => x,
                Err(_) => return Err("Random seed invalid".into()),
            }
        };
        Ok(GameConfiguration {
            player_count,
            random_seed,
            board_size,
            tiles_per_player,
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
        } = GameConfiguration::default();
        Self {
            player_count: player_count.to_string(),
            board_size: board_size.to_string(),
            tiles_per_player: tiles_per_player.to_string(),
            random_seed: random_seed.to_string(),
            auto_random: true,
            error: None,
        }
    }
}
