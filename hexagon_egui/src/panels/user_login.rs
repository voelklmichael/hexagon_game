use crate::app::BackendReqwest;

#[derive(serde::Serialize, serde::Deserialize)]
#[serde(default, rename_all = "camelCase")]
#[derive(Default)]
pub struct UserLogin {
    user_name_textbox: String,
    #[serde(skip)]
    password_textbox: String,
    #[serde(skip)]
    pub logged_in_as: Option<String>,
    pub user_id: Option<uuid::Uuid>,
}

pub fn show(ui: &mut egui::Ui, user: &mut UserLogin, client: &mut BackendReqwest) {
    if let Some(result) = client.login_user_task.take() {
        match result {
            Ok(name) => {
                user.user_name_textbox = name.clone();
                user.logged_in_as = Some(name);
                client.fetch_me();
            }
            Err(e) => tracing::info!("Failed to log-in user: {e}"),
        }
    }
    if let Some(result) = client.fetch_me_task.take() {
        match result {
            Ok(id) => user.user_id = Some(id),
            Err(e) => tracing::info!("Failed to fetch user id: {e}"),
        }
    }
    if let Some(result) = client.create_user_task.take() {
        match result {
            Ok(id) => {
                tracing::info!("User successfully created");
                user.user_id = Some(id);
            }
            Err(e) => tracing::info!("Failed to create user: {e}"),
        }
    }

    ui.heading("User Login");
    if let Some(name) = &user.logged_in_as {
        ui.label(format!("Logged in as: {name}"));
    } else {
        ui.label("Not logged in");
    }
    ui.add_space(8.0);

    ui.label("Username");
    ui.text_edit_singleline(&mut user.user_name_textbox);
    ui.add_space(4.0);

    ui.label("Password");
    ui.add(egui::TextEdit::singleline(&mut user.password_textbox).password(true));
    ui.add_space(8.0);

    let creating_or_logging_in =
        client.create_user_task.is_pending() || client.login_user_task.is_pending();

    ui.add_enabled_ui(!creating_or_logging_in, |ui| {
        let label = if creating_or_logging_in {
            "Loggnig in…"
        } else {
            "Log in"
        };
        if ui.button(label).clicked() {
            client.log_in(
                user.user_name_textbox.clone(),
                user.password_textbox.clone(),
            );
        }
    });
    ui.add_enabled_ui(!creating_or_logging_in, |ui| {
        let label = if creating_or_logging_in {
            "Creating…"
        } else {
            "Create User"
        };
        if ui.button(label).clicked() {
            client.create_user(
                user.user_name_textbox.clone(),
                user.password_textbox.clone(),
            );
        }
    });
}
