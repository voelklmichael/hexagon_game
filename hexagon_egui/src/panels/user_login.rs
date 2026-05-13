#[derive(serde::Serialize, serde::Deserialize)]
#[serde(default, rename_all = "camelCase")]
#[derive(Default)]
pub struct UserLogin {
    user_name_textbox: String,
    #[serde(skip)]
    password_textbox: String,
    #[serde(skip)]
    logged_in_as: Option<String>,
}
pub fn show(ui: &mut egui::Ui, user: &mut UserLogin) {
    ui.heading("User Login");
    {
        if let Some(user) = &user.logged_in_as {
            ui.label(format!("Logged in as: {user}"));
        } else {
            ui.label("Not logged in");
        }
    }
    ui.add_space(8.0);

    ui.label("Username");
    ui.text_edit_singleline(&mut user.user_name_textbox);
    ui.add_space(4.0);

    ui.label("Password");
    ui.add(egui::TextEdit::singleline(&mut user.password_textbox).password(true));
    ui.add_space(8.0);

    if ui.button("Log in").clicked() {
        // TODO: authenticate against backend
    }
    if ui.button("Create User").clicked() {
        // TODO: authenticate against backend
    }
}
