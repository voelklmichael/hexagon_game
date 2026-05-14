use hexagon_types::{DBHighscore, DBHighscorePeak};
use uuid::Uuid;

#[derive(sqlx::FromRow)]
struct HighscorePeakRow {
    max_velocity: i64,
    total_distance: i64,
}

impl From<HighscorePeakRow> for DBHighscorePeak {
    fn from(r: HighscorePeakRow) -> Self {
        Self {
            max_velocity: r.max_velocity,
            total_distance: r.total_distance,
        }
    }
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
        Ok(sqlx::query_as::<_, HighscorePeakRow>(
            "SELECT max_velocity, total_distance FROM highscore \
             WHERE user_id = $1 AND mission_id = $2",
        )
        .bind(user_id)
        .bind(mission_id)
        .fetch_optional(&self.0)
        .await?
        .map(Into::into))
    }

    /// Returns the maximum max_velocity and total_distance across all users for a single mission.
    pub async fn fetch_highscore_overall(
        &self,
        mission_id: Uuid,
    ) -> Result<DBHighscorePeak, sqlx::Error> {
        Ok(sqlx::query_as::<_, HighscorePeakRow>(
            "SELECT COALESCE(MAX(max_velocity), 0)   AS max_velocity, \
                    COALESCE(MAX(total_distance), 0) AS total_distance \
             FROM highscore WHERE mission_id = $1",
        )
        .bind(mission_id)
        .fetch_one(&self.0)
        .await?
        .into())
    }
}
