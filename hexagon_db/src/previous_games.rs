use hexagon_types::PreviousGame;
use uuid::Uuid;

#[derive(sqlx::FromRow)]
struct PreviousGameRow {
    id: i64,
    user_id: Uuid,
    mission_id: Uuid,
    game_state: sqlx::types::Json<serde_json::Value>,
}

impl From<PreviousGameRow> for PreviousGame {
    fn from(r: PreviousGameRow) -> Self {
        Self {
            id: r.id,
            user_id: r.user_id,
            mission_id: r.mission_id,
            game_state: r.game_state.0,
        }
    }
}

impl crate::DB {
    pub async fn insert_previous_game(
        &self,
        user_id: Uuid,
        mission_id: Uuid,
        game_state: &serde_json::Value,
    ) -> Result<i64, sqlx::Error> {
        let row: (i64,) = sqlx::query_as(
            "INSERT INTO previous_games (user_id, mission_id, game_state) VALUES ($1, $2, $3) RETURNING id",
        )
        .bind(user_id)
        .bind(mission_id)
        .bind(sqlx::types::Json(game_state))
        .fetch_one(&self.0)
        .await?;
        Ok(row.0)
    }

    pub async fn fetch_previous_games(&self) -> Result<Vec<PreviousGame>, sqlx::Error> {
        let rows: Vec<PreviousGameRow> = sqlx::query_as(
            "SELECT id, user_id, mission_id, game_state FROM previous_games ORDER BY id DESC",
        )
        .fetch_all(&self.0)
        .await?;
        Ok(rows.into_iter().map(Into::into).collect())
    }
}
