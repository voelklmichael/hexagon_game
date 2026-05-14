pub use hexagon_types::DBHighscore;
use hexagon_types::DBHighscorePeak;
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
    username: &'a str,
    password: &'a str,
    next: Option<&'a str>,
}

#[derive(Debug, serde::Deserialize)]
pub struct NextUrl {
    pub next: Option<String>,
}

#[derive(Debug, serde::Serialize)]
struct UserCreation<'a> {
    username: &'a str,
    password: &'a str,
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
        if res.status().is_success() {
            return Ok(res.json().await?);
        }
        let status = res.status().as_u16();
        let body = res.text().await.unwrap_or_default();
        Err(ApiError::Status { status, body })
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
        if res.status().is_success() {
            return Ok(res.json().await?);
        }
        let status = res.status().as_u16();
        let body = res.text().await.unwrap_or_default();
        Err(ApiError::Status { status, body })
    }

    // --- user login ---

    pub async fn fetch_me(&self) -> Result<Uuid, ApiError> {
        let res = self
            .client
            .get(self.url("/user_login/me"))
            .send()
            .await?;
        if res.status().is_success() {
            return Ok(res.json().await?);
        }
        let status = res.status().as_u16();
        let body = res.text().await.unwrap_or_default();
        Err(ApiError::Status { status, body })
    }

    pub async fn login(
        &self,
        username: &str,
        password: &str,
        next: Option<&str>,
    ) -> Result<NextUrl, ApiError> {
        let res = self
            .client
            .post(self.url("/user_login/login"))
            .json(&LoginRequest {
                username,
                password,
                next,
            })
            .send()
            .await?;
        if res.status().is_success() {
            return Ok(res.json().await?);
        }
        let status = res.status().as_u16();
        let body = res.text().await.unwrap_or_default();
        Err(ApiError::Status { status, body })
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
    pub async fn create_user(&self, username: &str, password: &str) -> Result<Uuid, ApiError> {
        let res = self
            .client
            .post(self.url("/user_login/create"))
            .json(&UserCreation { username, password })
            .send()
            .await?;
        if res.status().is_success() {
            return Ok(res.json().await?);
        }
        let status = res.status().as_u16();
        let body = res.text().await.unwrap_or_default();
        Err(ApiError::Status { status, body })
    }
}
