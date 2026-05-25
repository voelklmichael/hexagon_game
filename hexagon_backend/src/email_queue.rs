use std::collections::HashMap;
use std::sync::Arc;
use std::time::{Duration, Instant};

use hexagon_db::DB;
use lettre::message::header::ContentType;
use lettre::transport::smtp::authentication::Credentials;
use lettre::{AsyncSmtpTransport, AsyncTransport, Message, Tokio1Executor};
use secrecy::ExposeSecret;
use uuid::Uuid;

use crate::config::SmtpConfig;

const MAX_ATTEMPTS: u32 = 10;
const POLL_INTERVAL: Duration = Duration::from_secs(5);

pub fn spawn_email_worker(
    db: Arc<DB>,
    smtp: SmtpConfig,
    cancellation_token: tokio_util::sync::CancellationToken,
) {
    tokio::spawn(async move { worker(db, smtp, cancellation_token).await });
}

async fn worker(
    db: Arc<DB>,
    smtp: SmtpConfig,
    cancellation_token: tokio_util::sync::CancellationToken,
) {
    let creds = Credentials::new(
        smtp.smtp_username.clone(),
        smtp.smtp_password.expose_secret().to_owned(),
    );
    let mailer = AsyncSmtpTransport::<Tokio1Executor>::relay(&smtp.smtp_host)
        .unwrap()
        .port(smtp.smtp_port)
        .credentials(creds)
        .build();

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

        let live: std::collections::HashSet<Uuid> = tokens.iter().map(|t| t.token).collect();
        retry.retain(|id, _| live.contains(id));

        let now = Instant::now();
        let mut earliest_next: Option<Instant> = None;

        for token in &tokens {
            if cancellation_token.is_cancelled() {
                break 'outer;
            }

            match retry.get(&token.token) {
                Some((_, retry_at)) if *retry_at > now => {
                    earliest_next = Some(match earliest_next {
                        Some(e) => e.min(*retry_at),
                        None => *retry_at,
                    });
                    continue;
                }
                Some((attempts, _)) if *attempts >= MAX_ATTEMPTS => continue,
                _ => {}
            }

            let attempt = retry.get(&token.token).map(|(a, _)| *a).unwrap_or(0);

            tracing::info!(
                "Sending password-reset email to {} (attempt {})",
                token.email,
                attempt + 1
            );

            if try_send(&mailer, &smtp.email_from, &token.email, token.token).await {
                retry.remove(&token.token);
                if let Err(e) = db.mark_reset_token_sent(token.token).await {
                    tracing::warn!("mark_reset_token_sent failed for {}: {e}", token.token);
                } else {
                    tracing::info!(
                        "Password-reset email delivered and marked sent for {}",
                        token.email
                    );
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
    mailer: &AsyncSmtpTransport<Tokio1Executor>,
    from: &str,
    to: &str,
    token: Uuid,
) -> bool {
    let body = format!("Your password reset token: {token}\n\nThis token expires in 1 hour.");
    let email = match Message::builder()
        .from(from.parse().unwrap())
        .to(to.parse().unwrap())
        .subject("Password Reset")
        .header(ContentType::TEXT_PLAIN)
        .body(body)
    {
        Ok(m) => m,
        Err(e) => {
            tracing::warn!("Failed to build email for {to}: {e}");
            return false;
        }
    };
    match mailer.send(email).await {
        Ok(_) => true,
        Err(e) => {
            tracing::warn!("Sending password-reset email to {to} failed: {e}");
            false
        }
    }
}
