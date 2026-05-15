use hexagon_db::{DBConfig, DBHighscore};
use hexagon_types::PlayerStats;

#[tokio::main]
async fn main() {
    let config: DBConfig = figment::Figment::new()
        .merge(figment::providers::Env::raw())
        .extract()
        .unwrap();
    println!("{:?}", &config);
    let db = config.connect().await.unwrap();

    let mission_id = uuid::Uuid::nil();
    let user_id = uuid::Uuid::from_u128(1);

    let previous_peak = db.fetch_highscore_overall(mission_id).await.unwrap();
    println!("{previous_peak:?}");

    db.upsert_highscore(&DBHighscore {
        user_id,
        mission_id,
        players: [
            (
                0u8,
                PlayerStats {
                    max_velocity: 5,
                    total_distance: 20,
                },
            ),
            (
                1u8,
                PlayerStats {
                    max_velocity: 3,
                    total_distance: 15,
                },
            ),
        ]
        .into(),
    })
    .await
    .unwrap();

    dbg!(db.fetch_highscore_user(user_id, mission_id).await.unwrap());
    let peak = db.fetch_highscore_overall(mission_id).await.unwrap();
    println!("{peak:?}");
}
