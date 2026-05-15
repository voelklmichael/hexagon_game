use wasm_bindgen::prelude::*;

use crate::music::TRACKS;

#[wasm_bindgen]
extern "C" {
    type Audio;

    #[wasm_bindgen(constructor)]
    fn new() -> Audio;

    #[wasm_bindgen(method, setter, js_name = src)]
    fn set_src(this: &Audio, src: &str);

    #[wasm_bindgen(method, getter)]
    fn ended(this: &Audio) -> bool;

    #[wasm_bindgen(method)]
    fn pause(this: &Audio);

    #[wasm_bindgen(method, setter, js_name = volume)]
    fn set_volume(this: &Audio, volume: f64);
}

#[wasm_bindgen(inline_js = "
export function audio_play(audio) { audio.play(); }
")]
extern "C" {
    fn audio_play(audio: &JsValue);
}

pub struct MusicPlayer {
    audio: Audio,
}

impl MusicPlayer {
    pub fn new(volume: f32) -> Option<Self> {
        let audio = Audio::new();
        audio.set_volume(volume as f64);
        Some(Self { audio })
    }

    pub fn play_track(&self, track: usize) {
        self.audio
            .set_src(&format!("private_assets/{}", TRACKS[track].0));
        audio_play(self.audio.as_ref());
    }

    /// Returns true once the current track has finished playing.
    pub fn check_and_reset_finished(&self) -> bool {
        self.audio.ended()
    }

    pub fn set_volume(&self, volume: f32) {
        self.audio.set_volume(volume as f64);
    }

    pub fn set_paused(&self, paused: bool) {
        if paused {
            self.audio.pause();
        } else {
            audio_play(self.audio.as_ref());
        }
    }
}
