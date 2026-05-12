use uuid::Uuid;

#[derive(sqlx::FromRow)]
pub struct DBUserPassword {
    pub id: Uuid,
    pub user_name: String,
    pub password_hash: String,
}

impl crate::DB {
    pub async fn fetch_user_password_by_id(
        &self,
        user_id: Uuid,
    ) -> Result<Option<DBUserPassword>, sqlx::Error> {
        sqlx::query_as::<_, DBUserPassword>(
            "SELECT id, user_name, password_hash FROM users_password WHERE id = $1",
        )
        .bind(user_id)
        .fetch_optional(&self.0)
        .await
    }

    pub async fn fetch_user_password_by_name(
        &self,
        user_name: &str,
    ) -> Result<Option<DBUserPassword>, sqlx::Error> {
        sqlx::query_as::<_, DBUserPassword>(
            "SELECT id, user_name, password_hash FROM users_password WHERE user_name = $1",
        )
        .bind(user_name)
        .fetch_optional(&self.0)
        .await
    }

    pub async fn create_user(
        &self,
        user_name: &str,
        password_hash: &str,
    ) -> Result<Uuid, sqlx::Error> {
        let id = Uuid::new_v4();
        sqlx::query(
            "INSERT INTO users_password (id, user_name, password_hash) VALUES ($1, $2, $3)",
        )
        .bind(id)
        .bind(user_name)
        .bind(password_hash)
        .execute(&self.0)
        .await?;
        Ok(id)
    }
}
