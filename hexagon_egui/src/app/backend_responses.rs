use crate::HexApp;

impl HexApp {
    pub(super) fn process_backend_responses(&mut self) {
        if let Some(result) = self.backend_reqwest.fetch_won_missions_task.take() {
            match result {
                Ok(ids) => {
                    self.missions_won.extend(ids);
                }
                Err(e) => tracing::warn!("fetch_won_missions failed: {e}"),
            }
        }

        let active_mission_uuid = self
            .current_mission
            .and_then(crate::panels::missions::mission_id);
        if let Some(result) = self.backend_reqwest.fetch_mission_user_best_task.take() {
            match result {
                Ok((id, best)) if Some(id) == active_mission_uuid => {
                    self.mission_user_best = best;
                }
                Ok(_) => {}
                Err(e) => tracing::warn!("fetch_mission_user_best failed: {e}"),
            }
        }
        if let Some(result) = self.backend_reqwest.fetch_mission_overall_best_task.take() {
            match result {
                Ok((id, best)) if Some(id) == active_mission_uuid => {
                    self.mission_overall_best = Some(best);
                }
                Ok(_) => {}
                Err(e) => tracing::warn!("fetch_mission_overall_best failed: {e}"),
            }
        }
    }
}
