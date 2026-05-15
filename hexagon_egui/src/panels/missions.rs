use hexagon_engine::{GameOptionsDelivery, GameState, start_highscore_game};
use hexagon_types::{Mission, MissionDelivery, MissionEntry, MissionKind};
use strum::VariantArray as _;
use std::collections::HashSet;
use uuid::Uuid;

pub(crate) fn mission_id(missions: &[MissionEntry], index: usize) -> Option<Uuid> {
    missions.get(index).map(|m| m.id)
}

pub(crate) fn mission_count(missions: &[MissionEntry]) -> usize {
    missions.len()
}

pub(crate) fn start_mission(missions: &[MissionEntry], index: usize, game: &mut Option<GameState>) {
    let Some(entry) = missions.get(index) else {
        return;
    };
    match &entry.json {
        Mission::Delivery(MissionDelivery::V1(v1)) => {
            let opts = GameOptionsDelivery {
                board_radius: v1.board_radius as usize,
                outer_connectors: v1.outer_connectors.clone(),
                random_seed: v1.random_seed,
                npc_count: v1.npc_count as usize,
                player_has_target: v1.player_has_target,
                hand_size: v1.hand_size as usize,
            };
            match opts.start_game() {
                Ok(new_game) => *game = Some(new_game),
                Err(e) => tracing::error!("Failed to start mission '{}': {e}", entry.name),
            }
        }
        Mission::HighScore(hexagon_types::MissionHighscore::V1(v1)) => {
            match start_highscore_game(v1.clone()) {
                Ok(new_game) => *game = Some(new_game),
                Err(e) => tracing::error!("Failed to start mission '{}': {e}", entry.name),
            }
        }
    }
}

/// Returns the index into `missions` of the mission that was started, if any.
pub fn show(
    ui: &mut egui::Ui,
    missions: &[MissionEntry],
    missions_loaded: bool,
    game: &mut Option<GameState>,
    missions_won: &HashSet<Uuid>,
) -> Option<usize> {
    ui.heading("Missions");

    if !missions_loaded {
        ui.add_space(8.0);
        ui.horizontal(|ui| {
            ui.spinner();
            ui.label("Loading…");
        });
        return None;
    }

    if missions.is_empty() {
        ui.label("No missions available.");
        return None;
    }

    let mut started = None;

    for &kind in MissionKind::VARIANTS {
        let section: Vec<(usize, &MissionEntry)> = missions
            .iter()
            .enumerate()
            .filter(|(_, m)| m.kind == kind)
            .collect();

        if section.is_empty() {
            continue;
        }

        let header = match kind {
            MissionKind::Delivery => "Delivery",
            MissionKind::HighScore => "High Score",
        };

        egui::CollapsingHeader::new(header)
            .default_open(true)
            .show(ui, |ui| {
                egui::Grid::new(header)
                    .num_columns(2)
                    .spacing([12.0, 6.0])
                    .show(ui, |ui| {
                        for (i, mission) in &section {
                            let won = missions_won.contains(&mission.id);
                            let label = if won {
                                format!("✔ {}", mission.name)
                            } else {
                                mission.name.clone()
                            };
                            let can_start = matches!(&mission.json, Mission::Delivery(_));
                            if ui
                                .add_enabled(can_start, egui::Button::new(label))
                                .clicked()
                            {
                                start_mission(missions, *i, game);
                                started = Some(*i);
                            }
                            ui.add(egui::Label::new(&mission.description).wrap());
                            ui.end_row();
                        }
                    });
            });
    }

    started
}
