use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

const ASSETS_DIR: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/../private_assets/");

pub const TRACKS: &[(&str, &str)] = &[
    (
        "alexzavesa-dance-playful-night-510786.mp3",
        "Dance Playful Night",
    ),
    (
        "freemusiclab-charming-phonk-i-free-background-music-i-free-music-lab-release-513626.mp3",
        "Charming Phonk",
    ),
    (
        "kornevmusic-upbeat-happy-corporate-487426.mp3",
        "Upbeat Happy Corporate",
    ),
    (
        "lightbeatsmusic-joyful-rhythm-walk-funk-513936.mp3",
        "Joyful Rhythm Walk Funk",
    ),
    (
        "magpiemusic-action-race-rock-music-513682.mp3",
        "Action Race Rock Music",
    ),
    (
        "miromaxmusic-music-promotion-no-copyright-513944.mp3",
        "Music Promotion",
    ),
    (
        "starostin-comedy-cartoon-funny-background-music-492540.mp3",
        "Comedy Cartoon",
    ),
];

pub fn track_path(index: usize) -> PathBuf {
    Path::new(ASSETS_DIR).join(TRACKS[index].0)
}

pub struct MusicPlayer {
    _sink: rodio::MixerDeviceSink,
    player: rodio::Player,
    track_finished: Arc<AtomicBool>,
    generation: Arc<AtomicUsize>,
}

impl MusicPlayer {
    pub fn new(volume: f32) -> Option<Self> {
        let sink = rodio::DeviceSinkBuilder::open_default_sink().ok()?;
        let player = rodio::Player::connect_new(sink.mixer());
        player.set_volume(volume);
        Some(Self {
            _sink: sink,
            player,
            track_finished: Arc::new(AtomicBool::new(false)),
            generation: Arc::new(AtomicUsize::new(0)),
        })
    }

    pub fn play_track(&self, path: &Path) {
        let Ok(file) = std::fs::File::open(path) else {
            return;
        };
        let Ok(source) = rodio::Decoder::try_from(file) else {
            return;
        };

        self.player.clear();

        let current_gen = self.generation.fetch_add(1, Ordering::SeqCst) + 1;
        let track_finished = self.track_finished.clone();
        let generation = self.generation.clone();

        self.player.append(source);
        self.player
            .append(rodio::source::EmptyCallback::new(Box::new(move || {
                if generation.load(Ordering::SeqCst) == current_gen {
                    track_finished.store(true, Ordering::Relaxed);
                }
            })));
        self.player.play();
    }

    /// Returns true once when the current track has finished playing.
    pub fn check_and_reset_finished(&self) -> bool {
        self.track_finished.swap(false, Ordering::Relaxed)
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
