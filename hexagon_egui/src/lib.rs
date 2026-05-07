#![warn(clippy::all, rust_2018_idioms)]

mod app;
mod panels;
#[cfg(not(target_arch = "wasm32"))]
mod music;
pub use app::HexApp;
