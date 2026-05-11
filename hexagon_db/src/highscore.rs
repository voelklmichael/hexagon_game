use serde_json::Value;

use uuid::Uuid;

#[derive(Debug)]
pub struct Highscore {
    pub user_id: Uuid,
    pub mission_id: Uuid,
    /// Stored as BIGINT; represents a u32 value.
    pub max_velocity: i64,
    /// Stored as BIGINT; represents a u32 value.
    pub total_distance: i64,
    pub history: Value,
}

#[derive(Debug, sqlx::FromRow)]
pub struct HighscorePeak {
    pub max_velocity: i64,
    pub total_distance: i64,
}
impl crate::DB {
    /// Inserts a highscore row or, on conflict, keeps the greater value for each metric.
    pub async fn upsert_highscore(&self, score: &Highscore) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
        INSERT INTO highscore (user_id, mission_id, max_velocity, total_distance, history)
        VALUES ($1, $2, $3, $4, $5)
        ON CONFLICT (user_id, mission_id) DO UPDATE SET
            max_velocity   = GREATEST(excluded.max_velocity,   highscore.max_velocity),
            total_distance = GREATEST(excluded.total_distance, highscore.total_distance),
            history        = excluded.history
        "#,
        )
        .bind(score.user_id)
        .bind(score.mission_id)
        .bind(score.max_velocity)
        .bind(score.total_distance)
        .bind(&score.history)
        .execute(&self.0)
        .await?;

        Ok(())
    }

    /// Returns the stored peak `max_velocity` and `total_distance` for the given pair, or `None` if no
    /// row exists yet.
    pub async fn fetch_highscore(
        &self,
        user_id: Uuid,
        mission_id: Uuid,
    ) -> Result<Option<HighscorePeak>, sqlx::Error> {
        sqlx::query_as::<_, HighscorePeak>(
        "SELECT max_velocity, total_distance FROM highscore WHERE user_id = $1 AND mission_id = $2",
    )
    .bind(user_id)
    .bind(mission_id)
    .fetch_optional(&self.0)
    .await
    }
}
