use crate::app::MusicState;
use crate::music::TRACKS;

pub fn show(ui: &mut egui::Ui, state: &mut MusicState, player: Option<&crate::music::MusicPlayer>) {
    let mut new_track = state.current_track;

    ui.horizontal(|ui| {
        if ui.button("⏮").clicked() {
            new_track = if state.current_track == 0 {
                TRACKS.len() - 1
            } else {
                state.current_track - 1
            };
        }

        let pause_label = if state.paused { "▶" } else { "⏸" };
        if ui.button(pause_label).clicked() {
            state.paused = !state.paused;
            state.started = true;
            if let Some(p) = player {
                p.set_paused(state.paused);
            }
        }

        if ui.button("⏭").clicked() {
            new_track = (state.current_track + 1) % TRACKS.len();
        }

        egui::ComboBox::from_id_salt("music_track")
            .selected_text(TRACKS[state.current_track].1)
            .show_ui(ui, |ui| {
                for (i, (_, name)) in TRACKS.iter().enumerate() {
                    ui.selectable_value(&mut new_track, i, *name);
                }
            });
    });

    if new_track != state.current_track {
        state.current_track = new_track;
        state.paused = false;
        state.started = true;
        if let Some(p) = player {
            p.play_track(state.current_track);
        }
    }

    ui.horizontal(|ui| {
        ui.label("Volume");
        let resp = ui.add(egui::Slider::new(&mut state.volume, 0.0..=1.0));
        if resp.changed()
            && let Some(p) = player
        {
            p.set_volume(state.volume);
        }
    });
}
