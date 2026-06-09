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
        Box::new(|cc| Ok(Box::new(hexagon_egui::HexApp::new(cc)))),
    )
    .expect("failed to run app");
}
