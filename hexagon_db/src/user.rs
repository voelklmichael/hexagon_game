use uuid::Uuid;

#[derive(sqlx::FromRow)]
pub struct DBUser {
    pub id: Uuid,
    pub user_name: String,
    pub password_hash: String,
    pub name: String,
    pub email: String,
}

impl crate::DB {
    pub async fn fetch_user_by_id(&self, user_id: Uuid) -> Result<Option<DBUser>, sqlx::Error> {
        sqlx::query_as::<_, DBUser>(
            "SELECT id, user_name, password_hash, name, email FROM users WHERE id = $1",
        )
        .bind(user_id)
        .fetch_optional(&self.0)
        .await
    }

    pub async fn fetch_user_by_name(&self, user_name: &str) -> Result<Option<DBUser>, sqlx::Error> {
        sqlx::query_as::<_, DBUser>(
            "SELECT id, user_name, password_hash, name, email FROM users WHERE user_name = $1",
        )
        .bind(user_name)
        .fetch_optional(&self.0)
        .await
    }

    /// Creates a new user. If the given id is already taken, a new one is generated automatically and returned.
    /// Never use the input id again, always use the returned one! (They might be different!)
    pub async fn create_user(
        &self,
        id: Uuid,
        user_name: &str,
        password_hash: &str,
        name: &str,
        email: &str,
    ) -> Result<Uuid, sqlx::Error> {
        let row: (Uuid,) = sqlx::query_as(
            "INSERT INTO users (id, user_name, password_hash, name, email)
             VALUES ($1, $2, $3, $4, $5)
             ON CONFLICT (id) DO UPDATE
                 SET id            = gen_random_uuid(),
                     user_name     = EXCLUDED.user_name,
                     password_hash = EXCLUDED.password_hash,
                     name          = EXCLUDED.name,
                     email         = EXCLUDED.email
             RETURNING id",
        )
        .bind(id)
        .bind(user_name)
        .bind(password_hash)
        .bind(name)
        .bind(email)
        .fetch_one(&self.0)
        .await?;
        Ok(row.0)
    }
}
