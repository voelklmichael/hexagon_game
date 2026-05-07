use std::path::Path;

pub struct MusicPlayer {
    _sink: rodio::MixerDeviceSink,
    player: rodio::Player,
}

impl MusicPlayer {
    pub fn new(volume: f32) -> Option<Self> {
        let sink = rodio::DeviceSinkBuilder::open_default_sink().ok()?;
        let player = rodio::Player::connect_new(sink.mixer());
        player.set_volume(volume);
        Some(Self {
            _sink: sink,
            player,
        })
    }

    pub fn play_file(&self, path: &Path) {
        let Ok(file) = std::fs::File::open(path) else {
            return;
        };
        let Ok(source) = rodio::Decoder::try_from(file) else {
            return;
        };
        self.player.append(source);
    }

    pub fn set_volume(&self, volume: f32) {
        self.player.set_volume(volume);
    }

    pub fn set_paused(&self, paused: bool) {
        if paused {
            self.player.pause();
        } else {
            self.player.play();
        }
    }
}
