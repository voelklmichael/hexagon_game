use crate::app::BackendReqwest;

#[derive(serde::Serialize, serde::Deserialize)]
#[serde(default, rename_all = "camelCase")]
#[derive(Default)]
pub struct UserLogin {
    email_textbox: String,
    name_textbox: String,
    #[serde(skip)]
    password_textbox: String,
    #[serde(skip)]
    pub logged_in_as: Option<(uuid::Uuid, String)>,
    #[serde(skip)]
    login_error: Option<String>,
    #[serde(skip)]
    create_error: Option<String>,
}

pub fn show(ui: &mut egui::Ui, user: &mut UserLogin, client: &mut BackendReqwest) {
    if let Some(result) = client.login_user_task.take() {
        match result {
            Ok((id, name)) => {
                user.logged_in_as = Some((id, name));
                user.login_error = None;
            }
            Err(e) => user.login_error = Some(e.to_string()),
        }
    }
    if let Some(result) = client.create_user_task.take() {
        match result {
            Ok(()) => {
                user.create_error = None;
                client.log_in(user.email_textbox.clone(), user.password_textbox.clone());
            }
            Err(e) => user.create_error = Some(e.to_string()),
        }
    }

    let logging_in = client.login_user_task.is_pending();
    let creating = client.create_user_task.is_pending();
    let busy = logging_in || creating;

    // --- Status ---
    if let Some((_id, name)) = &user.logged_in_as {
        ui.label(format!("Logged in as: {name}"));
    } else {
        ui.label("Not logged in");
    }
    ui.add_space(8.0);

    // --- Login ---
    ui.heading("Login");
    ui.add_space(4.0);
    ui.label("Email");
    ui.text_edit_singleline(&mut user.email_textbox);
    ui.add_space(4.0);
    ui.label("Password");
    ui.add(egui::TextEdit::singleline(&mut user.password_textbox).password(true));
    ui.add_space(8.0);
    ui.add_enabled_ui(!busy, |ui| {
        let label = if logging_in { "Logging in…" } else { "Log in" };
        if ui.button(label).clicked() {
            user.login_error = None;
            client.log_in(user.email_textbox.clone(), user.password_textbox.clone());
        }
    });
    if let Some(err) = &user.login_error {
        ui.colored_label(egui::Color32::RED, err);
    }

    ui.add_space(8.0);
    ui.separator();
    ui.add_space(8.0);

    // --- Create User ---
    ui.heading("Create User");
    ui.add_space(4.0);
    ui.label("Name");
    ui.text_edit_singleline(&mut user.name_textbox);
    ui.add_space(4.0);
    ui.label("Email");
    ui.text_edit_singleline(&mut user.email_textbox);
    ui.add_space(4.0);
    ui.label("Password");
    ui.add(egui::TextEdit::singleline(&mut user.password_textbox).password(true));
    ui.add_space(8.0);
    ui.add_enabled_ui(!busy, |ui| {
        let label = if creating { "Creating…" } else { "Create User" };
        if ui.button(label).clicked() {
            user.create_error = None;
            client.create_user(
                user.name_textbox.clone(),
                user.email_textbox.clone(),
                user.password_textbox.clone(),
            );
        }
    });
    if let Some(err) = &user.create_error {
        ui.colored_label(egui::Color32::RED, err);
    }
}
