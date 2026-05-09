pub struct MusicPlayer {}

impl MusicPlayer {
    pub fn new(_volume: f32) -> Option<Self> {
        Some(Self {})
    }

    pub fn play_track(&self, _track: usize) {}

    /// Returns true once when the current track has finished playing.
    pub fn check_and_reset_finished(&self) -> bool {
        true
    }

    pub fn set_volume(&self, _volume: f32) {}

    pub fn set_paused(&self, _paused: bool) {}
}
