use std::path::{Path, PathBuf};

use std::sync::Arc;

use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};

use crate::music::TRACKS;

pub(crate) const ASSETS_DIR: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/../private_assets/");

pub fn track_path(index: usize) -> PathBuf {
    Path::new(ASSETS_DIR).join(TRACKS[index].0)
}

pub struct MusicPlayer {
    pub(crate) _sink: rodio::MixerDeviceSink,
    pub(crate) player: rodio::Player,
    pub(crate) track_finished: Arc<AtomicBool>,
    pub(crate) generation: Arc<AtomicUsize>,
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

    pub fn play_track(&self, track: usize) {
        let path = &track_path(track);
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
