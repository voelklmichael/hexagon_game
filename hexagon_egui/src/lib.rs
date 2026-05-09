#![warn(clippy::all, rust_2018_idioms)]

mod app;
#[cfg(not(target_arch = "wasm32"))]
mod music;
mod panels;
pub use app::HexApp;
