use hexagon_types::{DBHighscore, DBHighscorePeak, LoginResponse, MeResponse};
use uuid::Uuid;

#[derive(Debug, thiserror::Error)]
pub enum ApiError {
    #[error("HTTP request failed: {0}")]
    Reqwest(#[from] reqwest::Error),
    #[error("Unexpected status {status}: {body}")]
    Status { status: u16, body: String },
}

#[derive(Debug, serde::Serialize)]
struct LoginRequest<'a> {
    email: &'a str,
    password: &'a str,
    next: Option<&'a str>,
}

#[derive(Debug, serde::Serialize)]
struct UserCreation<'a> {
    password: &'a str,
    name: &'a str,
    email: &'a str,
}

pub struct ApiClient {
    client: reqwest::Client,
    base_url: String,
}

impl ApiClient {
    /// Creates a new client. `base_url` should be e.g. `"https://example.com"`.
    pub fn new(base_url: impl Into<String>) -> Result<Self, ApiError> {
        #[cfg(not(target_arch = "wasm32"))]
        let client = reqwest::Client::builder()
            .https_only(!cfg!(debug_assertions))
            .cookie_store(true)
            .build()?;
        #[cfg(target_arch = "wasm32")]
        let client = reqwest::Client::new();
        Ok(Self {
            client,
            base_url: base_url.into(),
        })
    }

    fn url(&self, path: &str) -> String {
        format!("{}{}", self.base_url, path)
    }

    async fn expect_no_content(res: reqwest::Response) -> Result<(), ApiError> {
        let status = res.status();
        if status.is_success() {
            return Ok(());
        }
        let code = status.as_u16();
        let body = res.text().await.unwrap_or_default();
        Err(ApiError::Status { status: code, body })
    }

    async fn expect_json<T: serde::de::DeserializeOwned>(
        res: reqwest::Response,
    ) -> Result<T, ApiError> {
        if res.status().is_success() {
            return Ok(res.json().await?);
        }
        let code = res.status().as_u16();
        let body = res.text().await.unwrap_or_default();
        Err(ApiError::Status { status: code, body })
    }

    // --- health ---
    pub async fn healthz(&self) -> Result<(), ApiError> {
        let res = self.client.get(self.url("/healthz")).send().await?;
        Self::expect_no_content(res).await
    }

    // --- highscore ---
    pub async fn upsert_highscore(&self, score: &DBHighscore) -> Result<(), ApiError> {
        let res = self
            .client
            .post(self.url("/highscore"))
            .json(score)
            .send()
            .await?;
        Self::expect_no_content(res).await
    }

    pub async fn fetch_highscore_user(
        &self,
        user_id: Uuid,
        mission_id: Uuid,
    ) -> Result<DBHighscorePeak, ApiError> {
        let res = self
            .client
            .get(self.url(&format!("/highscore/user/{user_id}/{mission_id}")))
            .send()
            .await?;
        Self::expect_json(res).await
    }

    pub async fn fetch_highscore_overall(
        &self,
        mission_id: Uuid,
    ) -> Result<DBHighscorePeak, ApiError> {
        let res = self
            .client
            .get(self.url(&format!("/highscore/overall/{mission_id}")))
            .send()
            .await?;
        Self::expect_json(res).await
    }

    pub async fn fetch_won_missions(&self, user_id: Uuid) -> Result<Vec<Uuid>, ApiError> {
        let res = self
            .client
            .get(self.url(&format!("/highscore/user/{user_id}/missions")))
            .send()
            .await?;
        Self::expect_json(res).await
    }

    // --- user login ---
    pub async fn login(
        &self,
        email: &str,
        password: &str,
        next: Option<&str>,
    ) -> Result<LoginResponse, ApiError> {
        let res = self
            .client
            .post(self.url("/user_login/login"))
            .json(&LoginRequest {
                email,
                password,
                next,
            })
            .send()
            .await?;
        Self::expect_json(res).await
    }

    pub async fn me(&self) -> Result<Option<MeResponse>, ApiError> {
        let res = self
            .client
            .get(self.url("/user_login/me"))
            .send()
            .await
            .inspect_err(|e| tracing::warn!("Failed to call /user_login/me: {e}"))?;
        if res.status() == reqwest::StatusCode::UNAUTHORIZED {
            tracing::info!("Not logged in: UNAUTHORIZED");
            return Ok(None);
        }

        match Self::expect_json::<MeResponse>(res).await {
            Ok(me) => {
                tracing::info!("Logged in as {}", me.name);
                Ok(Some(me))
            }
            Err(e) => {
                tracing::warn!("Failed to call /user_login/me: {e}");
                Err(e.into())
            }
        }
    }

    pub async fn logout(&self) -> Result<(), ApiError> {
        let res = self
            .client
            .get(self.url("/user_login/logout"))
            .send()
            .await?;
        Self::expect_no_content(res).await
    }

    /// Creates a new user account and returns the new user's UUID.
    pub async fn create_user(
        &self,
        password: &str,
        name: &str,
        email: &str,
    ) -> Result<Uuid, ApiError> {
        let res = self
            .client
            .post(self.url("/user_login/create"))
            .json(&UserCreation {
                password,
                name,
                email,
            })
            .send()
            .await?;
        Self::expect_json(res).await
    }
}
