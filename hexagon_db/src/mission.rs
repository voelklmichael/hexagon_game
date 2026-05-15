use hexagon_types::{Mission, MissionEntry, MissionKind};
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
            MissionKind::HighScore => Self::HighScore,
            MissionKind::Delivery => Self::Delivery,
        }
    }
}

#[derive(sqlx::FromRow)]
struct MissionRow {
    id: Uuid,
    kind: DbMissionKind,
    name: String,
    description: String,
    number: i32,
    json: sqlx::types::Json<Mission>,
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
            description: r.description,
            number: r.number as u32,
            json: r.json.0,
        }
    }
}

impl crate::DB {
    pub async fn fetch_all_missions(&self) -> Result<Vec<MissionEntry>, sqlx::Error> {
        let rows: Vec<MissionRow> = sqlx::query_as(
            "SELECT id, kind, name, description, number, json FROM missions ORDER BY number",
        )
        .fetch_all(&self.0)
        .await?;
        Ok(rows.into_iter().map(Into::into).collect())
    }

    pub async fn fetch_missions_by_kind(
        &self,
        kind: MissionKind,
    ) -> Result<Vec<MissionEntry>, sqlx::Error> {
        let rows: Vec<MissionRow> = sqlx::query_as(
            "SELECT id, kind, name, description, number, json FROM missions WHERE kind = $1 ORDER BY number",
        )
        .bind(DbMissionKind::from(kind))
        .fetch_all(&self.0)
        .await?;
        Ok(rows.into_iter().map(Into::into).collect())
    }
}
