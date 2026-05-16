use crate::HexApp;

impl HexApp {
    pub(super) fn process_backend_responses(&mut self, current_time: f64) {
        if let Some(result) = self.backend_reqwest.create_user_task.take() {
            match result {
                Ok(()) => {
                    tracing::info!("User created, logging in");
                    self.user_login.create_error = None;
                    let email = self.user_login.email_textbox.clone();
                    let password = self.user_login.password_textbox.clone();
                    self.backend_reqwest.log_in(email, password);
                }
                Err(e) => self.user_login.create_error = Some(e.to_string()),
            }
        }

        if let Some(result) = self.backend_reqwest.login_user_task.take() {
            match result {
                Ok((id, name)) => {
                    tracing::info!("Logged in as {name} ({id})");
                    self.backend_reqwest.fetch_won_missions(id);
                    self.user_login.logged_in_as = Some((id, name));
                    self.user_login.login_error = None;
                }
                Err(e) => self.user_login.login_error = Some(e.to_string()),
            }
        }

        if let Some((id, name)) = self.backend_reqwest.session_user.take() {
            tracing::info!("Session restored as {name} ({id})");
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
            match result {
                Ok(missions) => {
                    tracing::info!("Fetched {} missions", missions.len());
                    self.backend_reqwest.on_fetch_all_missions_ok();
                    self.missions = missions;
                }
                Err(e) => {
                    tracing::warn!("fetch_all_missions failed: {e}");
                    self.backend_reqwest.on_fetch_all_missions_err(current_time);
                }
            }
        }

        if let Some(result) = self.backend_reqwest.fetch_won_missions_task.take() {
            match result {
                Ok(ids) => {
                    tracing::info!("Fetched {} won missions", ids.len());
                    self.backend_reqwest.on_fetch_won_missions_ok();
                    self.missions_won.extend(ids);
                }
                Err(e) => {
                    tracing::warn!("fetch_won_missions failed: {e}");
                    self.backend_reqwest.on_fetch_won_missions_err(current_time);
                }
            }
        }

        if let Some(result) = self.backend_reqwest.fetch_mission_user_best_task.take() {
            match result {
                Ok((id, Some(best))) => {
                    tracing::info!("Fetched user best for mission {id}");
                    self.backend_reqwest.on_fetch_user_best_ok();
                    self.cached_user_bests.insert(id, best);
                }
                Ok((id, None)) => {
                    tracing::info!("No user best yet for mission {id}");
                    self.backend_reqwest.on_fetch_user_best_ok();
                }
                Err(e) => {
                    tracing::warn!("fetch_mission_user_best failed: {e}");
                    self.backend_reqwest.on_fetch_user_best_err(current_time);
                }
            }
        }

        if let Some(result) = self.backend_reqwest.fetch_mission_overall_best_task.take() {
            match result {
                Ok((id, Some(best))) => {
                    tracing::info!("Fetched overall best for mission {id}");
                    self.backend_reqwest.on_fetch_overall_best_ok();
                    self.cached_overall_bests.insert(id, best);
                }
                Ok((id, None)) => {
                    tracing::info!("No overall best yet for mission {id}");
                    self.backend_reqwest.on_fetch_overall_best_ok();
                }
                Err(e) => {
                    tracing::warn!("fetch_mission_overall_best failed: {e}");
                    self.backend_reqwest.on_fetch_overall_best_err(current_time);
                }
            }
        }
    }
}
