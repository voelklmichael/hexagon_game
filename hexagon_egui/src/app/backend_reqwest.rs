use std::sync::Arc;

use egui_async::Bind;
use uuid::Uuid;

use hexagon_frontend_reqwest::{ApiClient, ApiError, DBHighscore};

#[cfg(debug_assertions)]
static BASE_URL: &str = "http://localhost:3000";
#[cfg(not(debug_assertions))]
static BASE_URL: &str = "https://hexagon-game-a0w8.onrender.com";

#[derive(Default)]
pub struct BackendReqwest {
    client: Option<Arc<ApiClient>>,
    pub create_user_task: Bind<Uuid, ApiError>,
    pub login_user_task: Bind<String, ApiError>,
    pub fetch_me_task: Bind<Uuid, ApiError>,
    upsert_highscore_task: Bind<(), ApiError>,
}

impl BackendReqwest {
    pub(crate) fn create_user(&mut self, user_name: String, password: String) {
        let Some(client) = self.get_client() else {
            return;
        };
        self.create_user_task.request(async move {
            client.create_user(&user_name, &password).await
        });
    }

    pub(crate) fn log_in(&mut self, user_name: String, password: String) {
        let Some(client) = self.get_client() else {
            return;
        };
        self.login_user_task.request(async move {
            client
                .login(&user_name, &password, None)
                .await
                .map(|_| user_name)
        });
    }

    pub(crate) fn fetch_me(&mut self) {
        let Some(client) = self.get_client() else {
            return;
        };
        self.fetch_me_task.request(async move { client.fetch_me().await });
    }

    pub(crate) fn upsert_highscore(
        &mut self,
        user_id: Uuid,
        mission_id: Uuid,
        max_velocity: i64,
        total_distance: i64,
    ) {
        let Some(client) = self.get_client() else {
            return;
        };
        let score = DBHighscore {
            user_id,
            mission_id,
            max_velocity,
            total_distance,
        };
        self.upsert_highscore_task
            .request(async move { client.upsert_highscore(&score).await });
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
