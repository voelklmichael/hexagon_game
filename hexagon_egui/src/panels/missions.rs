use hexagon_engine::GameState;
use std::collections::HashSet;
use std::sync::OnceLock;
use uuid::Uuid;

#[derive(serde::Deserialize)]
struct Mission {
    mission_id: Uuid,
    name: String,
    description: String,
    #[serde(flatten)]
    options: hexagon_engine::GameOptionsDelivery,
}

const MISSION_SOURCES: &[&str] = &[
    include_str!("../../missions/delivery/01_first_steps.json"),
    include_str!("../../missions/delivery/02_one_courier.json"),
    include_str!("../../missions/delivery/03_one_courier_with_target.json"),
    include_str!("../../missions/delivery/04_big_delivery.json"),
];

static MISSIONS: OnceLock<Vec<Mission>> = OnceLock::new();

fn missions() -> &'static [Mission] {
    MISSIONS.get_or_init(|| {
        MISSION_SOURCES
            .iter()
            .enumerate()
            .filter_map(|(i, src)| {
                serde_json::from_str(src)
                    .map_err(|e| eprintln!("Failed to parse mission {i}: {e}"))
                    .ok()
            })
            .collect()
    })
}

pub(crate) fn mission_count() -> usize {
    missions().len()
}

pub(crate) fn mission_id(index: usize) -> Option<Uuid> {
    missions().get(index).map(|m| m.mission_id)
}

pub(crate) fn start_mission(index: usize, game: &mut Option<GameState>) {
    let m = &missions()[index];
    match m.options.clone().start_game() {
        Ok(new_game) => *game = Some(new_game),
        Err(e) => eprintln!("Failed to start mission '{}': {e}", m.name),
    }
}

/// Returns the index of the mission that was started, if any.
pub fn show(
    ui: &mut egui::Ui,
    game: &mut Option<GameState>,
    missions_won: &HashSet<Uuid>,
) -> Option<usize> {
    ui.heading("Missions");
    ui.label("Select a delivery mission to play.");
    ui.add_space(4.0);

    let mut started = None;

    egui::Grid::new("missions_list")
        .num_columns(2)
        .spacing([12.0, 6.0])
        .show(ui, |ui| {
            for (i, mission) in missions().iter().enumerate() {
                let won = missions_won.contains(&mission.mission_id);
                let label = if won {
                    format!("✔ {}", mission.name)
                } else {
                    mission.name.clone()
                };
                if ui.button(label).clicked() {
                    start_mission(i, game);
                    started = Some(i);
                }
                ui.add(egui::Label::new(&mission.description).wrap());
                ui.end_row();
            }
        });

    started
}
