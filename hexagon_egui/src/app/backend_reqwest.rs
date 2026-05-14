use std::sync::Arc;

use egui_async::Bind;

use hexagon_frontend_reqwest::{ApiClient, ApiError};

#[cfg(debug_assertions)]
static BASE_URL: &str = "http://localhost:3000";
#[cfg(not(debug_assertions))]
static BASE_URL: &str = "https://hexagon-game-a0w8.onrender.com";

#[derive(Default)]
pub struct BackendReqwest {
    client: Option<Arc<ApiClient>>,
    pub create_user_task: Bind<(), ApiError>,
    pub login_user_task: Bind<(uuid::Uuid, String), ApiError>,
}

impl BackendReqwest {
    pub(crate) fn create_user(&mut self, name: String, email: String, password: String) {
        let Some(client) = self.get_client() else {
            return;
        };
        self.create_user_task.request(async move {
            client
                .create_user(&password, &name, &email)
                .await
                .map(|_| ())
        });
    }

    pub(crate) fn log_in(&mut self, email: String, password: String) {
        let Some(client) = self.get_client() else {
            return;
        };
        self.login_user_task.request(async move {
            let resp = client.login(&email, &password, None).await?;
            Ok((resp.user_id, resp.name))
        });
    }

    fn get_client(&mut self) -> Option<Arc<ApiClient>> {
        if self.client.is_none() {
            tracing::info!("Connecting to {BASE_URL}");
            match ApiClient::new(BASE_URL) {
                Ok(client) => {
                    tracing::info!("Connected to {BASE_URL}");
                    self.client = Some(Arc::new(client))
                }
                Err(e) => {
                    tracing::info!("Failed to connect to {BASE_URL}: {e}");
                    return None;
                }
            }
        }
        self.client.clone()
    }
}
