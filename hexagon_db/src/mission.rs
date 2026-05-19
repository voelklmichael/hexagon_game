use hexagon_types::{Mission, MissionEntry, MissionTag};
use uuid::Uuid;

#[derive(sqlx::Type, Debug)]
#[sqlx(type_name = "mission_tag", rename_all = "PascalCase")]
enum DbMissionTag {
    Tutorial,
    Deliviery,
}

impl From<MissionTag> for DbMissionTag {
    fn from(t: MissionTag) -> Self {
        match t {
            MissionTag::Tutorial => Self::Tutorial,
            MissionTag::Deliviery => Self::Deliviery,
        }
    }
}

impl From<DbMissionTag> for MissionTag {
    fn from(t: DbMissionTag) -> Self {
        match t {
            DbMissionTag::Tutorial => Self::Tutorial,
            DbMissionTag::Deliviery => Self::Deliviery,
        }
    }
}

#[derive(sqlx::FromRow)]
struct MissionRow {
    id: Uuid,
    tag: DbMissionTag,
    name: String,
    description: String,
    number: i32,
    json: sqlx::types::Json<Mission>,
}

impl From<MissionRow> for MissionEntry {
    fn from(r: MissionRow) -> Self {
        Self {
            id: r.id,
            tag: r.tag.into(),
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
            "SELECT id, tag, name, description, number, json FROM missions ORDER BY number",
        )
        .fetch_all(&self.0)
        .await?;
        Ok(rows.into_iter().map(Into::into).collect())
    }

    pub async fn upsert_mission(&self, entry: &MissionEntry) -> Result<(), sqlx::Error> {
        sqlx::query(
            "INSERT INTO missions (id, tag, name, description, number, json)
             VALUES ($1, $2, $3, $4, $5, $6)
             ON CONFLICT (id) DO UPDATE SET
               tag         = EXCLUDED.tag,
               name        = EXCLUDED.name,
               description = EXCLUDED.description,
               number      = EXCLUDED.number,
               json        = EXCLUDED.json",
        )
        .bind(entry.id)
        .bind(DbMissionTag::from(entry.tag))
        .bind(&entry.name)
        .bind(&entry.description)
        .bind(entry.number as i32)
        .bind(sqlx::types::Json(&entry.json))
        .execute(&self.0)
        .await?;
        Ok(())
    }
}
