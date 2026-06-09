#[unsafe(no_mangle)]
fn android_main(app: android_activity::AndroidApp) {
    android_logger::init_once(
        android_logger::Config::default().with_max_level(log::LevelFilter::Info),
    );

    let app_for_insets = app.clone();
    let options = eframe::NativeOptions {
        android_app: Some(app),
        ..Default::default()
    };

    eframe::run_native(
        "hexagon",
        options,
        Box::new(move |cc| Ok(Box::new(hexagon_egui::HexApp::new_android(cc, app_for_insets)))),
    )
    .expect("failed to run app");
}
