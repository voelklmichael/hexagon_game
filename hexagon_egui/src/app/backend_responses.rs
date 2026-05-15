use crate::HexApp;

impl HexApp {
    pub(super) fn process_backend_responses(&mut self) {
        if let Some((id, name)) = self.backend_reqwest.session_user.take() {
            self.user_login.logged_in_as = Some((id, name));
            if !self
                .backend_reqwest
                .fetch_mission_user_best_task
                .is_pending()
                && let (Some((user_id, _)), Some(uuid)) = (
                    &self.user_login.logged_in_as,
                    self.current_mission
                        .and_then(|idx| crate::panels::missions::mission_id(&self.missions, idx)),
                )
            {
                self.backend_reqwest.fetch_mission_user_best(*user_id, uuid);
            }
        }

        if let Some(result) = self.backend_reqwest.fetch_all_missions_task.take() {
            self.missions_loaded = true;
            match result {
                Ok(missions) => {
                    tracing::info!("Fetched missions: {}", missions.len());
                    self.missions = missions
                }
                Err(e) => tracing::warn!("fetch_all_missions failed: {e}"),
            }
        }

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
            .and_then(|idx| crate::panels::missions::mission_id(&self.missions, idx));
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
