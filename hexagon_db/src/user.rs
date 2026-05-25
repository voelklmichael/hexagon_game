use uuid::Uuid;

#[derive(sqlx::FromRow)]
pub struct DBUser {
    pub id: Uuid,
    pub password_hash: String,
    pub name: String,
    pub email: String,
}

#[derive(sqlx::FromRow)]
pub struct UnsentResetToken {
    pub token: Uuid,
    pub email: String,
}

impl crate::DB {
    pub async fn fetch_user_by_id(&self, user_id: Uuid) -> Result<Option<DBUser>, sqlx::Error> {
        sqlx::query_as::<_, DBUser>(
            "SELECT id, password_hash, name, email FROM users WHERE id = $1",
        )
        .bind(user_id)
        .fetch_optional(&self.0)
        .await
    }

    pub async fn fetch_user_by_email(&self, email: &str) -> Result<Option<DBUser>, sqlx::Error> {
        sqlx::query_as::<_, DBUser>(
            "SELECT id, password_hash, name, email FROM users WHERE email = $1",
        )
        .bind(email)
        .fetch_optional(&self.0)
        .await
    }

    pub async fn create_user(
        &self,
        password_hash: &str,
        name: &str,
        email: &str,
    ) -> Result<Uuid, sqlx::Error> {
        let row: (Uuid,) = sqlx::query_as(
            "INSERT INTO users (password_hash, name, email)
             VALUES ($1, $2, $3)
             RETURNING id",
        )
        .bind(password_hash)
        .bind(name)
        .bind(email)
        .fetch_one(&self.0)
        .await?;
        Ok(row.0)
    }

    /// Creates a password-reset token for the user with the given email.
    /// Deletes any existing token for that user first.
    /// Returns `None` if no user with that email exists.
    pub async fn create_password_reset_token(
        &self,
        email: &str,
    ) -> Result<Option<Uuid>, sqlx::Error> {
        let Some(user) = self.fetch_user_by_email(email).await? else {
            return Ok(None);
        };
        sqlx::query("DELETE FROM password_reset_tokens WHERE user_id = $1")
            .bind(user.id)
            .execute(&self.0)
            .await?;
        let (token,): (Uuid,) = sqlx::query_as(
            "INSERT INTO password_reset_tokens (user_id) VALUES ($1) RETURNING token",
        )
        .bind(user.id)
        .fetch_one(&self.0)
        .await?;
        Ok(Some(token))
    }

    /// Returns all tokens whose email has not been sent yet and that have not expired.
    pub async fn fetch_unsent_reset_tokens(&self) -> Result<Vec<UnsentResetToken>, sqlx::Error> {
        sqlx::query_as(
            "SELECT prt.token, u.email
             FROM password_reset_tokens prt
             JOIN users u ON u.id = prt.user_id
             WHERE prt.sent_at IS NULL AND prt.expires_at > NOW()",
        )
        .fetch_all(&self.0)
        .await
    }

    pub async fn mark_reset_token_sent(&self, token: Uuid) -> Result<(), sqlx::Error> {
        sqlx::query("UPDATE password_reset_tokens SET sent_at = NOW() WHERE token = $1")
            .bind(token)
            .execute(&self.0)
            .await?;
        Ok(())
    }

    /// Validates the token (must exist and not be expired), updates the user's password hash,
    /// and deletes the token. Returns `false` if the token is invalid or expired.
    pub async fn reset_password(
        &self,
        token: Uuid,
        new_password_hash: &str,
    ) -> Result<bool, sqlx::Error> {
        let result = sqlx::query(
            "UPDATE users SET password_hash = $1
             WHERE id = (
                 SELECT user_id FROM password_reset_tokens
                 WHERE token = $2 AND expires_at > NOW()
             )",
        )
        .bind(new_password_hash)
        .bind(token)
        .execute(&self.0)
        .await?;

        if result.rows_affected() == 0 {
            return Ok(false);
        }

        sqlx::query("DELETE FROM password_reset_tokens WHERE token = $1")
            .bind(token)
            .execute(&self.0)
            .await?;

        Ok(true)
    }
}
