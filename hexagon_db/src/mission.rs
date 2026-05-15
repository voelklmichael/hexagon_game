use hexagon_types::{MissionEntry, MissionKind};
use serde_json::Value;
use uuid::Uuid;

#[derive(sqlx::Type, Debug)]
#[sqlx(type_name = "mission_kind", rename_all = "PascalCase")]
enum DbMissionKind {
    HighScore,
    Delivery,
}

impl From<MissionKind> for DbMissionKind {
    fn from(k: MissionKind) -> Self {
        match k {
            MissionKind::HighScore => DbMissionKind::HighScore,
            MissionKind::Delivery => DbMissionKind::Delivery,
        }
    }
}

#[derive(sqlx::FromRow)]
struct MissionRow {
    id: Uuid,
    kind: DbMissionKind,
    name: String,
    number: i32,
    json: Value,
}

impl From<MissionRow> for MissionEntry {
    fn from(r: MissionRow) -> Self {
        Self {
            id: r.id,
            kind: match r.kind {
                DbMissionKind::HighScore => MissionKind::HighScore,
                DbMissionKind::Delivery => MissionKind::Delivery,
            },
            name: r.name,
            number: r.number as u32,
            json: r.json,
        }
    }
}

impl crate::DB {
    pub async fn fetch_missions_by_kind(
        &self,
        kind: MissionKind,
    ) -> Result<Vec<MissionEntry>, sqlx::Error> {
        let rows: Vec<MissionRow> = sqlx::query_as(
            "SELECT id, kind, name, number, json FROM missions WHERE kind = $1 ORDER BY number",
        )
        .bind(DbMissionKind::from(kind))
        .fetch_all(&self.0)
        .await?;
        Ok(rows.into_iter().map(Into::into).collect())
    }
}
