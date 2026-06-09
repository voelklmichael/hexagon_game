#![warn(clippy::all, rust_2018_idioms)]

mod app;
mod confetti;
mod music;
mod panels;
pub use app::HexApp;

#[cfg(target_os = "android")]
#[no_mangle]
fn android_main(app: android_activity::AndroidApp) {
    android_logger::init_once(
        android_logger::Config::default().with_max_level(log::LevelFilter::Info),
    );

    let options = eframe::NativeOptions {
        android_app: Some(app),
        ..Default::default()
    };

    eframe::run_native(
        "hexagon",
        options,
        Box::new(|cc| Ok(Box::new(HexApp::new(cc)))),
    )
    .expect("failed to run app");
}
