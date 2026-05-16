use std::sync::Arc;

use egui_async::Bind;
use hexagon_engine::Statistics;
use hexagon_frontend_reqwest::{ApiClient, ApiError};
use hexagon_types::{DBHighscore, DBHighscorePeak, MeResponse, MissionEntry, PlayerStats};

#[cfg(debug_assertions)]
static BASE_URL: &str = "http://localhost:3000";
#[cfg(not(debug_assertions))]
static BASE_URL: &str = "https://hexagon-game-a0w8.onrender.com";

#[derive(Default)]
pub struct BackendReqwest {
    client: Option<Arc<ApiClient>>,
    pub create_user_task: Bind<(), ApiError>,
    pub login_user_task: Bind<(uuid::Uuid, String), ApiError>,
    me_task: Bind<Option<MeResponse>, ApiError>,
    pub session_user: Option<(uuid::Uuid, String)>,
    pub mission_done_task: Bind<(), ApiError>,
    pub fetch_won_missions_task: Bind<Vec<uuid::Uuid>, ApiError>,
    pub fetch_mission_user_best_task: Bind<(uuid::Uuid, Option<DBHighscorePeak>), ApiError>,
    pub fetch_mission_overall_best_task: Bind<(uuid::Uuid, DBHighscorePeak), ApiError>,
    pub fetch_all_missions_task: Bind<Vec<MissionEntry>, ApiError>,
    pub save_previous_game_task: Bind<i64, ApiError>,
    session_checked: bool,
    missions_fetched: bool,
}

impl BackendReqwest {
    pub(crate) fn check_session(&mut self) {
        if self.session_checked {
            return;
        }
        self.session_checked = true;
        let Some(client) = self.get_client() else {
            return;
        };
        self.me_task.request(async move { client.me().await });
    }

    pub(crate) fn fetch_all_missions(&mut self) {
        if self.missions_fetched {
            return;
        }
        self.missions_fetched = true;
        let Some(client) = self.get_client() else {
            return;
        };
        self.fetch_all_missions_task
            .request(async move { client.fetch_all_missions().await });
    }

    pub(crate) fn is_session_pending(&mut self) -> bool {
        self.me_task.is_pending()
    }

    pub(crate) fn poll_me_task(&mut self) {
        self.check_session();
        self.fetch_all_missions();

        if let Some(result) = self.me_task.take() {
            match result {
                Ok(Some(resp)) => {
                    self.fetch_won_missions(resp.user_id);
                    self.session_user = Some((resp.user_id, resp.name));
                }
                Ok(None) => {}
                Err(e) => tracing::warn!("Session check failed: {e}"),
            }
        }
    }

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

    pub(crate) fn fetch_won_missions(&mut self, user_id: uuid::Uuid) {
        let Some(client) = self.get_client() else {
            return;
        };
        self.fetch_won_missions_task
            .request(async move { client.fetch_won_missions(user_id).await });
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
                let max_velocity = statistics.max_velocity.get(&pid).copied().unwrap_or(0) as i64;
                let total_distance =
                    statistics.total_path_weight.get(&pid).copied().unwrap_or(0) as i64;
                (
                    pid.0 as u8,
                    PlayerStats {
                        max_velocity,
                        total_distance,
                    },
                )
            })
            .collect();
        let score = DBHighscore {
            user_id,
            mission_id,
            players,
        };
        self.mission_done_task
            .request(async move { client.upsert_highscore(&score).await });
    }

    pub(crate) fn fetch_mission_user_best(&mut self, user_id: uuid::Uuid, mission_id: uuid::Uuid) {
        let Some(client) = self.get_client() else {
            return;
        };
        self.fetch_mission_user_best_task.request(async move {
            match client.fetch_highscore_user(user_id, mission_id).await {
                Ok(peak) => Ok((mission_id, Some(peak))),
                Err(ApiError::Status { status: 404, .. }) => Ok((mission_id, None)),
                Err(e) => Err(e),
            }
        });
    }

    pub(crate) fn save_previous_game(
        &mut self,
        user_id: uuid::Uuid,
        mission_id: uuid::Uuid,
        game_state: serde_json::Value,
    ) {
        let Some(client) = self.get_client() else {
            return;
        };
        self.save_previous_game_task.request(async move {
            client.save_previous_game(user_id, mission_id, &game_state).await
        });
    }

    pub(crate) fn fetch_mission_overall_best(&mut self, mission_id: uuid::Uuid) {
        let Some(client) = self.get_client() else {
            return;
        };
        self.fetch_mission_overall_best_task.request(async move {
            client
                .fetch_highscore_overall(mission_id)
                .await
                .map(|peak| (mission_id, peak))
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
