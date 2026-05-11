use uuid::Uuid;

#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct DBHighscore {
    pub user_id: Uuid,
    pub mission_id: Uuid,
    pub max_velocity: i64,
    pub total_distance: i64,
}

#[derive(Debug, serde::Serialize, sqlx::FromRow)]
pub struct DBHighscorePeak {
    pub max_velocity: i64,
    pub total_distance: i64,
}

impl crate::DB {
    /// Inserts a highscore row or, on conflict, keeps the greater value for each metric.
    pub async fn upsert_highscore(&self, score: &DBHighscore) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
            INSERT INTO highscore (user_id, mission_id, max_velocity, total_distance)
            VALUES ($1, $2, $3, $4)
            ON CONFLICT (user_id, mission_id) DO UPDATE SET
                max_velocity   = GREATEST(excluded.max_velocity,   highscore.max_velocity),
                total_distance = GREATEST(excluded.total_distance, highscore.total_distance)
            "#,
        )
        .bind(score.user_id)
        .bind(score.mission_id)
        .bind(score.max_velocity)
        .bind(score.total_distance)
        .execute(&self.0)
        .await?;

        Ok(())
    }

    /// Returns the peak values for a specific (user_id, mission_id) pair.
    pub async fn fetch_highscore_user(
        &self,
        user_id: Uuid,
        mission_id: Uuid,
    ) -> Result<Option<DBHighscorePeak>, sqlx::Error> {
        sqlx::query_as::<_, DBHighscorePeak>(
            "SELECT max_velocity, total_distance FROM highscore \
             WHERE user_id = $1 AND mission_id = $2",
        )
        .bind(user_id)
        .bind(mission_id)
        .fetch_optional(&self.0)
        .await
    }

    /// Returns the maximum max_velocity and total_distance across all users for a single mission.
    pub async fn fetch_highscore_overall(
        &self,
        mission_id: Uuid,
    ) -> Result<DBHighscorePeak, sqlx::Error> {
        sqlx::query_as::<_, DBHighscorePeak>(
            "SELECT COALESCE(MAX(max_velocity), 0)   AS max_velocity, \
                    COALESCE(MAX(total_distance), 0) AS total_distance \
             FROM highscore WHERE mission_id = $1",
        )
        .bind(mission_id)
        .fetch_one(&self.0)
        .await
    }
}
