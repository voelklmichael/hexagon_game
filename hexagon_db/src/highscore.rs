use hexagon_types::{DBHighscore, DBHighscorePeak, PlayerStats};
use uuid::Uuid;

#[derive(sqlx::FromRow)]
struct HighscoreRow {
    player_0_max_velocity: i64,
    player_0_total_distance: i64,
    player_1_max_velocity: i64,
    player_1_total_distance: i64,
    player_2_max_velocity: i64,
    player_2_total_distance: i64,
    player_3_max_velocity: i64,
    player_3_total_distance: i64,
    player_4_max_velocity: i64,
    player_4_total_distance: i64,
    player_5_max_velocity: i64,
    player_5_total_distance: i64,
    player_6_max_velocity: i64,
    player_6_total_distance: i64,
    player_7_max_velocity: i64,
    player_7_total_distance: i64,
    player_8_max_velocity: i64,
    player_8_total_distance: i64,
    player_9_max_velocity: i64,
    player_9_total_distance: i64,
}

impl From<HighscoreRow> for DBHighscorePeak {
    fn from(r: HighscoreRow) -> Self {
        let raw = [
            (r.player_0_max_velocity, r.player_0_total_distance),
            (r.player_1_max_velocity, r.player_1_total_distance),
            (r.player_2_max_velocity, r.player_2_total_distance),
            (r.player_3_max_velocity, r.player_3_total_distance),
            (r.player_4_max_velocity, r.player_4_total_distance),
            (r.player_5_max_velocity, r.player_5_total_distance),
            (r.player_6_max_velocity, r.player_6_total_distance),
            (r.player_7_max_velocity, r.player_7_total_distance),
            (r.player_8_max_velocity, r.player_8_total_distance),
            (r.player_9_max_velocity, r.player_9_total_distance),
        ];
        let players = raw
            .into_iter()
            .enumerate()
            .filter(|(_, (mv, td))| *mv != 0 || *td != 0)
            .map(|(i, (max_velocity, total_distance))| {
                (
                    i as u8,
                    PlayerStats {
                        max_velocity,
                        total_distance,
                    },
                )
            })
            .collect();
        Self { players }
    }
}

fn player_cols(score: &DBHighscore, idx: u8) -> (i64, i64) {
    score
        .players
        .get(&idx)
        .map(|s| (s.max_velocity, s.total_distance))
        .unwrap_or((0, 0))
}

impl crate::DB {
    pub async fn upsert_highscore(&self, score: &DBHighscore) -> Result<(), sqlx::Error> {
        let (mv0, td0) = player_cols(score, 0);
        let (mv1, td1) = player_cols(score, 1);
        let (mv2, td2) = player_cols(score, 2);
        let (mv3, td3) = player_cols(score, 3);
        let (mv4, td4) = player_cols(score, 4);
        let (mv5, td5) = player_cols(score, 5);
        let (mv6, td6) = player_cols(score, 6);
        let (mv7, td7) = player_cols(score, 7);
        let (mv8, td8) = player_cols(score, 8);
        let (mv9, td9) = player_cols(score, 9);

        sqlx::query(
            r#"
            INSERT INTO highscore (
                user_id, mission_id,
                player_0_max_velocity, player_0_max_velocity_game_id,
                player_0_total_distance, player_0_total_distance_game_id,
                player_1_max_velocity, player_1_max_velocity_game_id,
                player_1_total_distance, player_1_total_distance_game_id,
                player_2_max_velocity, player_2_max_velocity_game_id,
                player_2_total_distance, player_2_total_distance_game_id,
                player_3_max_velocity, player_3_max_velocity_game_id,
                player_3_total_distance, player_3_total_distance_game_id,
                player_4_max_velocity, player_4_max_velocity_game_id,
                player_4_total_distance, player_4_total_distance_game_id,
                player_5_max_velocity, player_5_max_velocity_game_id,
                player_5_total_distance, player_5_total_distance_game_id,
                player_6_max_velocity, player_6_max_velocity_game_id,
                player_6_total_distance, player_6_total_distance_game_id,
                player_7_max_velocity, player_7_max_velocity_game_id,
                player_7_total_distance, player_7_total_distance_game_id,
                player_8_max_velocity, player_8_max_velocity_game_id,
                player_8_total_distance, player_8_total_distance_game_id,
                player_9_max_velocity, player_9_max_velocity_game_id,
                player_9_total_distance, player_9_total_distance_game_id
            )
            VALUES (
                $1,  $2,
                $3,  $23, $4,  $23,
                $5,  $23, $6,  $23,
                $7,  $23, $8,  $23,
                $9,  $23, $10, $23,
                $11, $23, $12, $23,
                $13, $23, $14, $23,
                $15, $23, $16, $23,
                $17, $23, $18, $23,
                $19, $23, $20, $23,
                $21, $23, $22, $23
            )
            ON CONFLICT (user_id, mission_id) DO UPDATE SET
                player_0_max_velocity           = GREATEST(excluded.player_0_max_velocity,   highscore.player_0_max_velocity),
                player_0_max_velocity_game_id   = CASE WHEN excluded.player_0_max_velocity > highscore.player_0_max_velocity
                                                       THEN excluded.player_0_max_velocity_game_id
                                                       ELSE highscore.player_0_max_velocity_game_id END,
                player_0_total_distance         = GREATEST(excluded.player_0_total_distance, highscore.player_0_total_distance),
                player_0_total_distance_game_id = CASE WHEN excluded.player_0_total_distance > highscore.player_0_total_distance
                                                       THEN excluded.player_0_total_distance_game_id
                                                       ELSE highscore.player_0_total_distance_game_id END,
                player_1_max_velocity           = GREATEST(excluded.player_1_max_velocity,   highscore.player_1_max_velocity),
                player_1_max_velocity_game_id   = CASE WHEN excluded.player_1_max_velocity > highscore.player_1_max_velocity
                                                       THEN excluded.player_1_max_velocity_game_id
                                                       ELSE highscore.player_1_max_velocity_game_id END,
                player_1_total_distance         = GREATEST(excluded.player_1_total_distance, highscore.player_1_total_distance),
                player_1_total_distance_game_id = CASE WHEN excluded.player_1_total_distance > highscore.player_1_total_distance
                                                       THEN excluded.player_1_total_distance_game_id
                                                      ELSE highscore.player_1_total_distance_game_id END,
                player_2_max_velocity           = GREATEST(excluded.player_2_max_velocity,   highscore.player_2_max_velocity),
                player_2_max_velocity_game_id   = CASE WHEN excluded.player_2_max_velocity > highscore.player_2_max_velocity
                                                       THEN excluded.player_2_max_velocity_game_id
                                                       ELSE highscore.player_2_max_velocity_game_id END,
                player_2_total_distance         = GREATEST(excluded.player_2_total_distance, highscore.player_2_total_distance),
                player_2_total_distance_game_id = CASE WHEN excluded.player_2_total_distance > highscore.player_2_total_distance
                                                        THEN excluded.player_2_total_distance_game_id
                                                       ELSE highscore.player_2_total_distance_game_id END,
                player_3_max_velocity           = GREATEST(excluded.player_3_max_velocity,   highscore.player_3_max_velocity),
                player_3_max_velocity_game_id   = CASE WHEN excluded.player_3_max_velocity > highscore.player_3_max_velocity
                                                       THEN excluded.player_3_max_velocity_game_id
                                                       ELSE highscore.player_3_max_velocity_game_id END,
                player_3_total_distance         = GREATEST(excluded.player_3_total_distance, highscore.player_3_total_distance),
                player_3_total_distance_game_id = CASE WHEN excluded.player_3_total_distance > highscore.player_3_total_distance
                                                       THEN excluded.player_3_total_distance_game_id
                                                       ELSE highscore.player_3_total_distance_game_id END,
                player_4_max_velocity           = GREATEST(excluded.player_4_max_velocity,   highscore.player_4_max_velocity),
                player_4_max_velocity_game_id   = CASE WHEN excluded.player_4_max_velocity > highscore.player_4_max_velocity
                                                       THEN excluded.player_4_max_velocity_game_id
                                                       ELSE highscore.player_4_max_velocity_game_id END,
                player_4_total_distance         = GREATEST(excluded.player_4_total_distance, highscore.player_4_total_distance),
                player_4_total_distance_game_id = CASE WHEN excluded.player_4_total_distance > highscore.player_4_total_distance
                                                       THEN excluded.player_4_total_distance_game_id
                                                       ELSE highscore.player_4_total_distance_game_id END,
                player_5_max_velocity           = GREATEST(excluded.player_5_max_velocity,   highscore.player_5_max_velocity),
                player_5_max_velocity_game_id   = CASE WHEN excluded.player_5_max_velocity > highscore.player_5_max_velocity
                                                       THEN excluded.player_5_max_velocity_game_id
                                                       ELSE highscore.player_5_max_velocity_game_id END,
                player_5_total_distance         = GREATEST(excluded.player_5_total_distance, highscore.player_5_total_distance),
                player_5_total_distance_game_id = CASE WHEN excluded.player_5_total_distance > highscore.player_5_total_distance
                                                       THEN excluded.player_5_total_distance_game_id
                                                       ELSE highscore.player_5_total_distance_game_id END,
                player_6_max_velocity           = GREATEST(excluded.player_6_max_velocity,   highscore.player_6_max_velocity),
                player_6_max_velocity_game_id   = CASE WHEN excluded.player_6_max_velocity > highscore.player_6_max_velocity
                                                       THEN excluded.player_6_max_velocity_game_id
                                                       ELSE highscore.player_6_max_velocity_game_id END,
                player_6_total_distance         = GREATEST(excluded.player_6_total_distance, highscore.player_6_total_distance),
                player_6_total_distance_game_id = CASE WHEN excluded.player_6_total_distance > highscore.player_6_total_distance
                                                       THEN excluded.player_6_total_distance_game_id
                                                       ELSE highscore.player_6_total_distance_game_id END,
                player_7_max_velocity           = GREATEST(excluded.player_7_max_velocity,   highscore.player_7_max_velocity),
                player_7_max_velocity_game_id   = CASE WHEN excluded.player_7_max_velocity > highscore.player_7_max_velocity
                                                       THEN excluded.player_7_max_velocity_game_id
                                                       ELSE highscore.player_7_max_velocity_game_id END,
                player_7_total_distance         = GREATEST(excluded.player_7_total_distance, highscore.player_7_total_distance),
                player_7_total_distance_game_id = CASE WHEN excluded.player_7_total_distance > highscore.player_7_total_distance
                                                       THEN excluded.player_7_total_distance_game_id
                                                       ELSE highscore.player_7_total_distance_game_id END,
                player_8_max_velocity           = GREATEST(excluded.player_8_max_velocity,   highscore.player_8_max_velocity),
                player_8_max_velocity_game_id   = CASE WHEN excluded.player_8_max_velocity > highscore.player_8_max_velocity
                                                       THEN excluded.player_8_max_velocity_game_id
                                                       ELSE highscore.player_8_max_velocity_game_id END,
                player_8_total_distance         = GREATEST(excluded.player_8_total_distance, highscore.player_8_total_distance),
                player_8_total_distance_game_id = CASE WHEN excluded.player_8_total_distance > highscore.player_8_total_distance
                                                       THEN excluded.player_8_total_distance_game_id
                                                       ELSE highscore.player_8_total_distance_game_id END,
                player_9_max_velocity           = GREATEST(excluded.player_9_max_velocity,   highscore.player_9_max_velocity),
                player_9_max_velocity_game_id   = CASE WHEN excluded.player_9_max_velocity > highscore.player_9_max_velocity
                                                       THEN excluded.player_9_max_velocity_game_id
                                                       ELSE highscore.player_9_max_velocity_game_id END,
                player_9_total_distance         = GREATEST(excluded.player_9_total_distance, highscore.player_9_total_distance),
                player_9_total_distance_game_id = CASE WHEN excluded.player_9_total_distance > highscore.player_9_total_distance
                                                       THEN excluded.player_9_total_distance_game_id
                                                       ELSE highscore.player_9_total_distance_game_id END
            "#,
        )
        .bind(score.user_id)
        .bind(score.mission_id)
        .bind(mv0).bind(td0)
        .bind(mv1).bind(td1)
        .bind(mv2).bind(td2)
        .bind(mv3).bind(td3)
        .bind(mv4).bind(td4)
        .bind(mv5).bind(td5)
        .bind(mv6).bind(td6)
        .bind(mv7).bind(td7)
        .bind(mv8).bind(td8)
        .bind(mv9).bind(td9)
        .bind(score.game_id)
        .execute(&self.0)
        .await?;

        sqlx::query(
            r#"
            INSERT INTO highscore_peak (
                mission_id,
                player_0_max_velocity, player_0_max_velocity_game_id,
                player_0_total_distance, player_0_total_distance_game_id,
                player_1_max_velocity, player_1_max_velocity_game_id,
                player_1_total_distance, player_1_total_distance_game_id,
                player_2_max_velocity, player_2_max_velocity_game_id,
                player_2_total_distance, player_2_total_distance_game_id,
                player_3_max_velocity, player_3_max_velocity_game_id,
                player_3_total_distance, player_3_total_distance_game_id,
                player_4_max_velocity, player_4_max_velocity_game_id,
                player_4_total_distance, player_4_total_distance_game_id,
                player_5_max_velocity, player_5_max_velocity_game_id,
                player_5_total_distance, player_5_total_distance_game_id,
                player_6_max_velocity, player_6_max_velocity_game_id,
                player_6_total_distance, player_6_total_distance_game_id,
                player_7_max_velocity, player_7_max_velocity_game_id,
                player_7_total_distance, player_7_total_distance_game_id,
                player_8_max_velocity, player_8_max_velocity_game_id,
                player_8_total_distance, player_8_total_distance_game_id,
                player_9_max_velocity, player_9_max_velocity_game_id,
                player_9_total_distance, player_9_total_distance_game_id
            )
            VALUES (
                $1,
                $2,  $22, $3,  $22,
                $4,  $22, $5,  $22,
                $6,  $22, $7,  $22,
                $8,  $22, $9,  $22,
                $10, $22, $11, $22,
                $12, $22, $13, $22,
                $14, $22, $15, $22,
                $16, $22, $17, $22,
                $18, $22, $19, $22,
                $20, $22, $21, $22
            )
            ON CONFLICT (mission_id) DO UPDATE SET
                player_0_max_velocity           = GREATEST(excluded.player_0_max_velocity,   highscore_peak.player_0_max_velocity),
                player_0_max_velocity_game_id   = CASE WHEN excluded.player_0_max_velocity > highscore_peak.player_0_max_velocity
                                                       THEN excluded.player_0_max_velocity_game_id
                                                       ELSE highscore_peak.player_0_max_velocity_game_id END,
                player_0_total_distance         = GREATEST(excluded.player_0_total_distance, highscore_peak.player_0_total_distance),
                player_0_total_distance_game_id = CASE WHEN excluded.player_0_total_distance > highscore_peak.player_0_total_distance
                                                       THEN excluded.player_0_total_distance_game_id
                                                       ELSE highscore_peak.player_0_total_distance_game_id END,
                player_1_max_velocity           = GREATEST(excluded.player_1_max_velocity,   highscore_peak.player_1_max_velocity),
                player_1_max_velocity_game_id   = CASE WHEN excluded.player_1_max_velocity > highscore_peak.player_1_max_velocity
                                                       THEN excluded.player_1_max_velocity_game_id
                                                       ELSE highscore_peak.player_1_max_velocity_game_id END,
                player_1_total_distance         = GREATEST(excluded.player_1_total_distance, highscore_peak.player_1_total_distance),
                player_1_total_distance_game_id = CASE WHEN excluded.player_1_total_distance > highscore_peak.player_1_total_distance
                                                       THEN excluded.player_1_total_distance_game_id
                                                       ELSE highscore_peak.player_1_total_distance_game_id END,
                player_2_max_velocity           = GREATEST(excluded.player_2_max_velocity,   highscore_peak.player_2_max_velocity),
                player_2_max_velocity_game_id   = CASE WHEN excluded.player_2_max_velocity > highscore_peak.player_2_max_velocity
                                                       THEN excluded.player_2_max_velocity_game_id
                                                       ELSE highscore_peak.player_2_max_velocity_game_id END,
                player_2_total_distance         = GREATEST(excluded.player_2_total_distance, highscore_peak.player_2_total_distance),
                player_2_total_distance_game_id = CASE WHEN excluded.player_2_total_distance > highscore_peak.player_2_total_distance
                                                       THEN excluded.player_2_total_distance_game_id
                                                       ELSE highscore_peak.player_2_total_distance_game_id END,
                player_3_max_velocity           = GREATEST(excluded.player_3_max_velocity,   highscore_peak.player_3_max_velocity),
                player_3_max_velocity_game_id   = CASE WHEN excluded.player_3_max_velocity > highscore_peak.player_3_max_velocity
                                                       THEN excluded.player_3_max_velocity_game_id
                                                       ELSE highscore_peak.player_3_max_velocity_game_id END,
                player_3_total_distance         = GREATEST(excluded.player_3_total_distance, highscore_peak.player_3_total_distance),
                player_3_total_distance_game_id = CASE WHEN excluded.player_3_total_distance > highscore_peak.player_3_total_distance
                                                       THEN excluded.player_3_total_distance_game_id
                                                       ELSE highscore_peak.player_3_total_distance_game_id END,
                player_4_max_velocity           = GREATEST(excluded.player_4_max_velocity,   highscore_peak.player_4_max_velocity),
                player_4_max_velocity_game_id   = CASE WHEN excluded.player_4_max_velocity > highscore_peak.player_4_max_velocity
                                                       THEN excluded.player_4_max_velocity_game_id
                                                       ELSE highscore_peak.player_4_max_velocity_game_id END,
                player_4_total_distance         = GREATEST(excluded.player_4_total_distance, highscore_peak.player_4_total_distance),
                player_4_total_distance_game_id = CASE WHEN excluded.player_4_total_distance > highscore_peak.player_4_total_distance
                                                       THEN excluded.player_4_total_distance_game_id
                                                       ELSE highscore_peak.player_4_total_distance_game_id END,
                player_5_max_velocity           = GREATEST(excluded.player_5_max_velocity,   highscore_peak.player_5_max_velocity),
                player_5_max_velocity_game_id   = CASE WHEN excluded.player_5_max_velocity > highscore_peak.player_5_max_velocity
                                                       THEN excluded.player_5_max_velocity_game_id
                                                       ELSE highscore_peak.player_5_max_velocity_game_id END,
                player_5_total_distance         = GREATEST(excluded.player_5_total_distance, highscore_peak.player_5_total_distance),
                player_5_total_distance_game_id = CASE WHEN excluded.player_5_total_distance > highscore_peak.player_5_total_distance
                                                       THEN excluded.player_5_total_distance_game_id
                                                       ELSE highscore_peak.player_5_total_distance_game_id END,
                player_6_max_velocity           = GREATEST(excluded.player_6_max_velocity,   highscore_peak.player_6_max_velocity),
                player_6_max_velocity_game_id   = CASE WHEN excluded.player_6_max_velocity > highscore_peak.player_6_max_velocity
                                                       THEN excluded.player_6_max_velocity_game_id
                                                       ELSE highscore_peak.player_6_max_velocity_game_id END,
                player_6_total_distance         = GREATEST(excluded.player_6_total_distance, highscore_peak.player_6_total_distance),
                player_6_total_distance_game_id = CASE WHEN excluded.player_6_total_distance > highscore_peak.player_6_total_distance
                                                       THEN excluded.player_6_total_distance_game_id
                                                       ELSE highscore_peak.player_6_total_distance_game_id END,
                player_7_max_velocity           = GREATEST(excluded.player_7_max_velocity,   highscore_peak.player_7_max_velocity),
                player_7_max_velocity_game_id   = CASE WHEN excluded.player_7_max_velocity > highscore_peak.player_7_max_velocity
                                                       THEN excluded.player_7_max_velocity_game_id
                                                       ELSE highscore_peak.player_7_max_velocity_game_id END,
                player_7_total_distance         = GREATEST(excluded.player_7_total_distance, highscore_peak.player_7_total_distance),
                player_7_total_distance_game_id = CASE WHEN excluded.player_7_total_distance > highscore_peak.player_7_total_distance
                                                       THEN excluded.player_7_total_distance_game_id
                                                       ELSE highscore_peak.player_7_total_distance_game_id END,
                player_8_max_velocity           = GREATEST(excluded.player_8_max_velocity,   highscore_peak.player_8_max_velocity),
                player_8_max_velocity_game_id   = CASE WHEN excluded.player_8_max_velocity > highscore_peak.player_8_max_velocity
                                                       THEN excluded.player_8_max_velocity_game_id
                                                       ELSE highscore_peak.player_8_max_velocity_game_id END,
                player_8_total_distance         = GREATEST(excluded.player_8_total_distance, highscore_peak.player_8_total_distance),
                player_8_total_distance_game_id = CASE WHEN excluded.player_8_total_distance > highscore_peak.player_8_total_distance
                                                       THEN excluded.player_8_total_distance_game_id
                                                       ELSE highscore_peak.player_8_total_distance_game_id END,
                player_9_max_velocity           = GREATEST(excluded.player_9_max_velocity,   highscore_peak.player_9_max_velocity),
                player_9_max_velocity_game_id   = CASE WHEN excluded.player_9_max_velocity > highscore_peak.player_9_max_velocity
                                                       THEN excluded.player_9_max_velocity_game_id
                                                       ELSE highscore_peak.player_9_max_velocity_game_id END,
                player_9_total_distance         = GREATEST(excluded.player_9_total_distance, highscore_peak.player_9_total_distance),
                player_9_total_distance_game_id = CASE WHEN excluded.player_9_total_distance > highscore_peak.player_9_total_distance
                                                       THEN excluded.player_9_total_distance_game_id
                                                       ELSE highscore_peak.player_9_total_distance_game_id END
            "#,
        )
        .bind(score.mission_id)
        .bind(mv0).bind(td0)
        .bind(mv1).bind(td1)
        .bind(mv2).bind(td2)
        .bind(mv3).bind(td3)
        .bind(mv4).bind(td4)
        .bind(mv5).bind(td5)
        .bind(mv6).bind(td6)
        .bind(mv7).bind(td7)
        .bind(mv8).bind(td8)
        .bind(mv9).bind(td9)
        .bind(score.game_id)
        .execute(&self.0)
        .await?;

        Ok(())
    }

    pub async fn fetch_highscore_user(
        &self,
        user_id: Uuid,
        mission_id: Uuid,
    ) -> Result<Option<DBHighscorePeak>, sqlx::Error> {
        Ok(sqlx::query_as::<_, HighscoreRow>(
            "SELECT \
                player_0_max_velocity, player_0_total_distance, \
                player_1_max_velocity, player_1_total_distance, \
                player_2_max_velocity, player_2_total_distance, \
                player_3_max_velocity, player_3_total_distance, \
                player_4_max_velocity, player_4_total_distance, \
                player_5_max_velocity, player_5_total_distance, \
                player_6_max_velocity, player_6_total_distance, \
                player_7_max_velocity, player_7_total_distance, \
                player_8_max_velocity, player_8_total_distance, \
                player_9_max_velocity, player_9_total_distance \
             FROM highscore WHERE user_id = $1 AND mission_id = $2",
        )
        .bind(user_id)
        .bind(mission_id)
        .fetch_optional(&self.0)
        .await?
        .map(Into::into))
    }

    pub async fn fetch_won_missions(&self, user_id: Uuid) -> Result<Vec<Uuid>, sqlx::Error> {
        let rows: Vec<(Uuid,)> =
            sqlx::query_as("SELECT DISTINCT mission_id FROM highscore WHERE user_id = $1")
                .bind(user_id)
                .fetch_all(&self.0)
                .await?;
        Ok(rows.into_iter().map(|(id,)| id).collect())
    }

    /// Reads the pre-computed mission peak from `highscore_peak` (O(1) key lookup).
    pub async fn fetch_highscore_overall(
        &self,
        mission_id: Uuid,
    ) -> Result<DBHighscorePeak, sqlx::Error> {
        Ok(sqlx::query_as::<_, HighscoreRow>(
            "SELECT \
                player_0_max_velocity, player_0_total_distance, \
                player_1_max_velocity, player_1_total_distance, \
                player_2_max_velocity, player_2_total_distance, \
                player_3_max_velocity, player_3_total_distance, \
                player_4_max_velocity, player_4_total_distance, \
                player_5_max_velocity, player_5_total_distance, \
                player_6_max_velocity, player_6_total_distance, \
                player_7_max_velocity, player_7_total_distance, \
                player_8_max_velocity, player_8_total_distance, \
                player_9_max_velocity, player_9_total_distance \
             FROM highscore_peak WHERE mission_id = $1",
        )
        .bind(mission_id)
        .fetch_optional(&self.0)
        .await?
        .map(Into::into)
        .unwrap_or_default())
    }
}
