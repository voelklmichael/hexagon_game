#[cfg(all(not(target_arch = "wasm32"), not(target_os = "android")))]
mod desktop;
#[cfg(all(not(target_arch = "wasm32"), not(target_os = "android")))]
pub use desktop::*;

#[cfg(target_arch = "wasm32")]
mod wasm;
#[cfg(target_arch = "wasm32")]
pub use wasm::*;

#[cfg(target_os = "android")]
mod android;
#[cfg(target_os = "android")]
pub use android::*;

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
