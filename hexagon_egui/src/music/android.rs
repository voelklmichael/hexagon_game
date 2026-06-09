pub struct MusicPlayer;

impl MusicPlayer {
    pub fn new(_volume: f32) -> Option<Self> {
        None
    }

    pub fn play_track(&self, _track: usize) {}

    pub fn check_and_reset_finished(&self) -> bool {
        false
    }

    pub fn set_volume(&self, _volume: f32) {}

    pub fn set_paused(&self, _paused: bool) {}
}
