use std::collections::HashMap;
use std::sync::Arc;
use std::time::{Duration, Instant};

use hexagon_db::DB;
use resend_rs::types::CreateEmailBaseOptions;
use uuid::Uuid;

const MAX_ATTEMPTS: u32 = 10;
/// How often to re-query the DB when there is nothing ready to send.
const POLL_INTERVAL: Duration = Duration::from_secs(5);

/// Spawns a background task that polls the database for unsent password-reset
/// tokens and delivers them via Resend, with per-token exponential backoff.
/// The task stops cleanly when `cancellation_token` is cancelled.
pub fn spawn_email_worker(
    db: Arc<DB>,
    resend: Option<Arc<resend_rs::Resend>>,
    from: Option<String>,
    cancellation_token: tokio_util::sync::CancellationToken,
) {
    tokio::spawn(async move { worker(db, resend, from, cancellation_token).await });
}

async fn worker(
    db: Arc<DB>,
    resend: Option<Arc<resend_rs::Resend>>,
    from: Option<String>,
    cancellation_token: tokio_util::sync::CancellationToken,
) {
    // Per-token retry state: token → (attempt_count, retry_not_before).
    // Resets on server restart, which is fine — tokens will just be retried sooner.
    let mut retry: HashMap<Uuid, (u32, Instant)> = HashMap::new();

    'outer: loop {
        let tokens = match db.fetch_unsent_reset_tokens().await {
            Ok(t) => t,
            Err(e) => {
                tracing::warn!("Failed to fetch unsent reset tokens: {e}");
                tokio::time::sleep(POLL_INTERVAL).await;
                continue;
            }
        };

        tracing::info!("Email worker: {} unsent token(s) pending", tokens.len());

        // Remove stale retry entries for tokens that expired or were sent by another instance.
        let live: std::collections::HashSet<Uuid> = tokens.iter().map(|t| t.token).collect();
        retry.retain(|id, _| live.contains(id));

        let now = Instant::now();
        let mut earliest_next: Option<Instant> = None;

        for token in &tokens {
            if cancellation_token.is_cancelled() {
                break 'outer;
            }

            match retry.get(&token.token) {
                // Still in backoff — note when it becomes due.
                Some((_, retry_at)) if *retry_at > now => {
                    earliest_next = Some(match earliest_next {
                        Some(e) => e.min(*retry_at),
                        None => *retry_at,
                    });
                    continue;
                }
                // Gave up on this token (attempt >= MAX_ATTEMPTS).
                Some((attempts, _)) if *attempts >= MAX_ATTEMPTS => continue,
                _ => {}
            }

            let attempt = retry.get(&token.token).map(|(a, _)| *a).unwrap_or(0);

            tracing::info!(
                "Sending password-reset email to {} (attempt {})",
                token.email,
                attempt + 1
            );
            if try_send(&resend, &from, &token.email, token.token).await {
                retry.remove(&token.token);
                if let Err(e) = db.mark_reset_token_sent(token.token).await {
                    tracing::warn!("mark_reset_token_sent failed for {}: {e}", token.token);
                }
            } else {
                let next_attempt = attempt + 1;
                if next_attempt < MAX_ATTEMPTS {
                    let delay = Duration::from_secs(2u64.pow(next_attempt.min(10)));
                    let retry_at = Instant::now() + delay;
                    tracing::warn!(
                        "Reset email to {} failed (attempt {next_attempt}), retry in {delay:?}",
                        token.email
                    );
                    earliest_next = Some(match earliest_next {
                        Some(e) => e.min(retry_at),
                        None => retry_at,
                    });
                    retry.insert(token.token, (next_attempt, retry_at));
                } else {
                    tracing::warn!(
                        "Giving up on reset email to {} after {MAX_ATTEMPTS} attempts",
                        token.email
                    );
                    retry.insert(
                        token.token,
                        (next_attempt, Instant::now() + Duration::MAX / 2),
                    );
                }
            }
        }

        // Sleep until the next retry is due, but re-poll the DB at least every POLL_INTERVAL
        // to pick up newly created tokens.
        let sleep = earliest_next
            .map(|t| t.saturating_duration_since(Instant::now()))
            .unwrap_or(POLL_INTERVAL)
            .min(POLL_INTERVAL);

        tokio::select! {
            _ = cancellation_token.cancelled() => {
                tracing::info!("Email worker shut down");
                break;
            }
            _ = tokio::time::sleep(sleep) => {}
        }
    }
}

async fn try_send(
    resend: &Option<Arc<resend_rs::Resend>>,
    from: &Option<String>,
    to: &str,
    token: Uuid,
) -> bool {
    let (Some(resend), Some(from)) = (resend, from) else {
        tracing::info!("No Resend client — reset token for {to}: {token}");
        return true;
    };
    let body = format!("Your password reset token: {token}\n\nThis token expires in 1 hour.");
    let email = CreateEmailBaseOptions::new(from.as_str(), [to], "Password Reset").with_text(&body);
    match resend.emails.send(email).await {
        Ok(_) => {
            tracing::info!("Password-reset email sent to {to}");
            true
        }
        Err(e) => {
            tracing::warn!("Sending password-reset email to {to} failed: {e}");
            false
        }
    }
}
