use std::sync::Arc;

use egui_async::Bind;
use hexagon_engine::Statistics;
use hexagon_frontend_reqwest::{ApiClient, ApiError};
use hexagon_types::{DBHighscore, PlayerStats};

#[cfg(debug_assertions)]
static BASE_URL: &str = "http://localhost:3000";
#[cfg(not(debug_assertions))]
static BASE_URL: &str = "https://hexagon-game-a0w8.onrender.com";

#[derive(Default)]
pub struct BackendReqwest {
    client: Option<Arc<ApiClient>>,
    pub create_user_task: Bind<(), ApiError>,
    pub login_user_task: Bind<(uuid::Uuid, String), ApiError>,
    pub mission_done_task: Bind<(), ApiError>,
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

    pub(crate) fn report_mission_done(
        &mut self,
        user_id: uuid::Uuid,
        mission_id: uuid::Uuid,
        statistics: &Statistics,
    ) {
        let Some(client) = self.get_client() else {
            return;
        };
        let players = statistics
            .max_velocity
            .keys()
            .map(|&pid| {
                let max_velocity = statistics
                    .max_velocity
                    .get(&pid)
                    .copied()
                    .unwrap_or(0) as i64;
                let total_distance = statistics
                    .total_path_weight
                    .get(&pid)
                    .copied()
                    .unwrap_or(0) as i64;
                (pid.0 as u8, PlayerStats { max_velocity, total_distance })
            })
            .collect();
        let score = DBHighscore { user_id, mission_id, players };
        self.mission_done_task
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
