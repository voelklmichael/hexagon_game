use std::collections::HashMap;
use std::sync::Arc;

use egui_async::Bind;
use hexagon_engine::Statistics;
use hexagon_frontend_reqwest::{ApiClient, ApiError};
use hexagon_types::{DBHighscore, DBHighscorePeak, MeResponse, MissionEntry, PlayerStats};

#[cfg(debug_assertions)]
static BASE_URL: &str = "http://localhost:3000";
#[cfg(not(debug_assertions))]
static BASE_URL: &str = "https://hexagon-game-a0w8.onrender.com";

const MAX_RETRIES: u32 = 10;

// ---------------------------------------------------------------------------
// Retry state
// ---------------------------------------------------------------------------

#[derive(Default)]
struct RetryState {
    retries: u32,
    /// Egui time (seconds since app start) of the next eligible retry.
    /// `None` means no retry is currently scheduled.
    next_retry_at: Option<f64>,
}

impl RetryState {
    fn on_failure(&mut self, label: &str, current_time: f64) {
        self.retries += 1;
        if self.retries <= MAX_RETRIES {
            let delay = (1u64 << self.retries) as f64;
            self.next_retry_at = Some(current_time + delay);
            tracing::warn!("{label} failed, retry {} in {delay}s", self.retries);
        } else {
            self.next_retry_at = None;
            tracing::warn!("{label} giving up after {MAX_RETRIES} retries");
        }
    }

    fn on_success(&mut self) {
        self.retries = 0;
        self.next_retry_at = None;
    }

    /// Returns `true` and clears the scheduled time if a retry is due.
    fn take_eligible(&mut self, current_time: f64) -> bool {
        if self.next_retry_at.is_some_and(|t| t <= current_time) {
            self.next_retry_at = None;
            true
        } else {
            false
        }
    }
}

// ---------------------------------------------------------------------------
// Pending write queue
// ---------------------------------------------------------------------------

struct PendingMissionResult {
    mission_id: uuid::Uuid,
    players: HashMap<u8, PlayerStats>,
    game_state: serde_json::Value,
    retries: u32,
    next_retry_at: f64,
}

// ---------------------------------------------------------------------------
// BackendReqwest
// ---------------------------------------------------------------------------

#[derive(Default)]
pub struct BackendReqwest {
    client: Option<Arc<ApiClient>>,
    pub create_user_task: Bind<(), ApiError>,
    pub login_user_task: Bind<(uuid::Uuid, String), ApiError>,
    me_task: Bind<Option<MeResponse>, ApiError>,
    pub session_user: Option<(uuid::Uuid, String)>,

    // --- fetch tasks ---
    pub fetch_won_missions_task: Bind<Vec<uuid::Uuid>, ApiError>,
    pub fetch_mission_user_best_task: Bind<(uuid::Uuid, Option<DBHighscorePeak>), ApiError>,
    pub fetch_mission_overall_best_task: Bind<(uuid::Uuid, Option<DBHighscorePeak>), ApiError>,
    pub fetch_all_missions_task: Bind<Vec<MissionEntry>, ApiError>,

    // Retry state + stored params for each background fetch
    fetch_all_missions_retry: RetryState,
    fetch_won_missions_retry: RetryState,
    fetch_user_best_retry: RetryState,
    fetch_user_best_mid: Option<uuid::Uuid>,
    fetch_overall_best_retry: RetryState,
    fetch_overall_best_mid: Option<uuid::Uuid>,

    // --- write queue ---
    mission_write_task: Bind<(), ApiError>,
    pending_mission_results: Vec<PendingMissionResult>,
    /// Index into `pending_mission_results` of the currently in-flight write.
    in_flight_write: Option<usize>,
    session_checked: bool,
    missions_fetched: bool,
}

impl BackendReqwest {
    pub(crate) fn check_session(&mut self) {
        if self.session_checked {
            return;
        }
        let Some(client) = self.get_client() else {
            return;
        };
        self.session_checked = true;
        self.me_task.request(async move { client.me().await });
    }

    pub(crate) fn fetch_all_missions(&mut self) {
        if self.missions_fetched {
            return;
        }
        let Some(client) = self.get_client() else {
            return;
        };
        self.missions_fetched = true;
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
        self.fetch_won_missions_retry.on_success();
        self.fire_fetch_won_missions(user_id);
    }

    fn fire_fetch_won_missions(&mut self, user_id: uuid::Uuid) {
        let Some(client) = self.get_client() else {
            return;
        };
        self.fetch_won_missions_task
            .request(async move { client.fetch_won_missions(user_id).await });
    }

    pub(crate) fn fetch_mission_user_best(&mut self, user_id: uuid::Uuid, mission_id: uuid::Uuid) {
        self.fetch_user_best_mid = Some(mission_id);
        self.fetch_user_best_retry.on_success();
        self.fire_fetch_user_best(user_id, mission_id);
    }

    fn fire_fetch_user_best(&mut self, user_id: uuid::Uuid, mission_id: uuid::Uuid) {
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

    pub(crate) fn fetch_mission_overall_best(&mut self, mission_id: uuid::Uuid) {
        self.fetch_overall_best_mid = Some(mission_id);
        self.fetch_overall_best_retry.on_success();
        self.fire_fetch_overall_best(mission_id);
    }

    fn fire_fetch_overall_best(&mut self, mission_id: uuid::Uuid) {
        let Some(client) = self.get_client() else {
            return;
        };
        self.fetch_mission_overall_best_task.request(async move {
            match client.fetch_highscore_overall(mission_id).await {
                Ok(peak) => Ok((mission_id, Some(peak))),
                Err(ApiError::Status { status: 404, .. }) => Ok((mission_id, None)),
                Err(e) => Err(e),
            }
        });
    }

    // --- retry hooks (called from backend_responses) ---

    pub(crate) fn on_fetch_all_missions_ok(&mut self) {
        self.fetch_all_missions_retry.on_success();
    }

    pub(crate) fn on_fetch_all_missions_err(&mut self, current_time: f64) {
        self.fetch_all_missions_retry
            .on_failure("fetch_all_missions", current_time);
    }

    pub(crate) fn on_fetch_won_missions_ok(&mut self) {
        self.fetch_won_missions_retry.on_success();
    }

    pub(crate) fn on_fetch_won_missions_err(&mut self, current_time: f64) {
        self.fetch_won_missions_retry
            .on_failure("fetch_won_missions", current_time);
    }

    pub(crate) fn on_fetch_user_best_ok(&mut self) {
        self.fetch_user_best_retry.on_success();
    }

    pub(crate) fn on_fetch_user_best_err(&mut self, current_time: f64) {
        self.fetch_user_best_retry
            .on_failure("fetch_mission_user_best", current_time);
    }

    pub(crate) fn on_fetch_overall_best_ok(&mut self) {
        self.fetch_overall_best_retry.on_success();
    }

    pub(crate) fn on_fetch_overall_best_err(&mut self, current_time: f64) {
        self.fetch_overall_best_retry
            .on_failure("fetch_mission_overall_best", current_time);
    }

    /// Re-fires any fetch that failed and whose back-off delay has elapsed.
    pub(crate) fn poll_fetch_retries(&mut self, user_id: Option<uuid::Uuid>, current_time: f64) {
        let Some(client) = self.get_client() else {
            return;
        };

        if !self.fetch_all_missions_task.is_pending()
            && self.fetch_all_missions_retry.take_eligible(current_time)
        {
            let c = client.clone();
            self.fetch_all_missions_task
                .request(async move { c.fetch_all_missions().await });
        }

        if !self.fetch_won_missions_task.is_pending()
            && let Some(uid) = user_id
            && self.fetch_won_missions_retry.take_eligible(current_time)
        {
            let c = client.clone();
            self.fetch_won_missions_task
                .request(async move { c.fetch_won_missions(uid).await });
        }

        if !self.fetch_mission_user_best_task.is_pending()
            && let Some(uid) = user_id
            && let Some(mid) = self.fetch_user_best_mid
            && self.fetch_user_best_retry.take_eligible(current_time)
        {
            let c = client.clone();
            self.fetch_mission_user_best_task.request(async move {
                match c.fetch_highscore_user(uid, mid).await {
                    Ok(peak) => Ok((mid, Some(peak))),
                    Err(ApiError::Status { status: 404, .. }) => Ok((mid, None)),
                    Err(e) => Err(e),
                }
            });
        }

        if !self.fetch_mission_overall_best_task.is_pending()
            && let Some(mid) = self.fetch_overall_best_mid
            && self.fetch_overall_best_retry.take_eligible(current_time)
        {
            let c = client.clone();
            self.fetch_mission_overall_best_task.request(async move {
                match c.fetch_highscore_overall(mid).await {
                    Ok(peak) => Ok((mid, Some(peak))),
                    Err(ApiError::Status { status: 404, .. }) => Ok((mid, None)),
                    Err(e) => Err(e),
                }
            });
        }
    }

    // --- write queue ---

    pub(crate) fn queue_mission_result(
        &mut self,
        mission_id: uuid::Uuid,
        statistics: &Statistics,
        game_state: serde_json::Value,
    ) {
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

        self.pending_mission_results.push(PendingMissionResult {
            mission_id,
            players,
            game_state,
            retries: 0,
            next_retry_at: 0.0,
        });
    }

    pub(crate) fn poll_mission_writes(&mut self, user_id: Option<uuid::Uuid>, current_time: f64) {
        if self.in_flight_write.is_some() {
            if let Some(result) = self.mission_write_task.take() {
                let idx = self.in_flight_write.take().unwrap();
                match result {
                    Ok(()) => {
                        self.pending_mission_results.remove(idx);
                    }
                    Err(e) => {
                        let entry = &mut self.pending_mission_results[idx];
                        entry.retries += 1;
                        if entry.retries <= MAX_RETRIES {
                            let delay = 1u64 << entry.retries;
                            entry.next_retry_at = current_time + delay as f64;
                            tracing::warn!(
                                "Mission write failed (attempt {}), retrying in {delay}s: {e}",
                                entry.retries
                            );
                        } else {
                            tracing::warn!(
                                "Mission write failed after {MAX_RETRIES} retries, giving up: {e}"
                            );
                            self.pending_mission_results.remove(idx);
                        }
                    }
                }
            }
            return;
        }

        let Some(user_id) = user_id else { return };
        let Some(client) = self.get_client() else {
            return;
        };

        let Some(idx) = self
            .pending_mission_results
            .iter()
            .position(|p| p.retries <= MAX_RETRIES && p.next_retry_at <= current_time)
        else {
            return;
        };

        let entry = &self.pending_mission_results[idx];
        let score = DBHighscore {
            user_id,
            mission_id: entry.mission_id,
            players: entry.players.clone(),
        };
        let client2 = client.clone();
        let game_state = entry.game_state.clone();
        let mission_id = entry.mission_id;

        self.mission_write_task.request(async move {
            client.upsert_highscore(&score).await?;
            client2
                .save_previous_game(user_id, mission_id, &game_state)
                .await?;
            Ok(())
        });
        self.in_flight_write = Some(idx);
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
