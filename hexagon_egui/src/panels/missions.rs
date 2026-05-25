use hexagon_engine::{GameState, start_highscore_game_v1, start_highscore_game_v2};
use hexagon_types::{Mission, MissionEntry, MissionTag};
use std::collections::HashSet;
use strum::VariantArray as _;
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
        Mission::HighScore(hs) => match hs.as_ref() {
            hexagon_types::MissionHighscore::V1(v1) => match start_highscore_game_v1(v1.clone()) {
                Ok(new_game) => *game = Some(new_game),
                Err(e) => tracing::error!("Failed to start mission '{}': {e}", entry.name),
            },
            hexagon_types::MissionHighscore::V2(v2) => match start_highscore_game_v2(v2.clone()) {
                Ok(new_game) => *game = Some(new_game),
                Err(e) => tracing::error!("Failed to start mission '{}': {e}", entry.name),
            },
        },
    }
}

/// Returns the index into `missions` of the mission that was started, if any.
pub fn show(
    ui: &mut egui::Ui,
    missions: &[MissionEntry],
    game: &mut Option<GameState>,
    missions_won: &HashSet<Uuid>,
) -> Option<usize> {
    if missions.is_empty() {
        ui.label("No missions available.");
        return None;
    }

    let mut started = None;

    for &tag in MissionTag::VARIANTS {
        let section: Vec<(usize, &MissionEntry)> = missions
            .iter()
            .enumerate()
            .filter(|(_, m)| m.tag == tag)
            .collect();

        if section.is_empty() {
            continue;
        }

        let header = match tag {
            MissionTag::Tutorial => "Tutorial",
            MissionTag::Deliviery => "Delivery",
        };

        egui::CollapsingHeader::new(header)
            .default_open(true)
            .show(ui, |ui| {
                egui::Grid::new(tag as usize)
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
                            if ui.button(label).clicked() {
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
