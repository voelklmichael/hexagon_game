use hexagon_db::{DBConfig, DBHighscore, MissionEntry};
use hexagon_types::{
    Board, ConnectorId, HexagonPosition, MissionHighscoreV1, PlayerStats, RandomNumberGenerator,
    Tile, WinningConditionHighscore,
};
use uuid::Uuid;

#[tokio::main]
async fn main() {
    let config: DBConfig = figment::Figment::new()
        .merge(figment::providers::Env::raw())
        .extract()
        .unwrap();
    tracing_subscriber::fmt::init();
    tracing::info!("Config{:?}", &config);
    let db = config.connect().await.unwrap();

    let number = 100u32;
    let mission_id = Uuid::from_u128(number as u128);

    let hand_size = 3;
    let random_seed = 42;
    let mut rng = RandomNumberGenerator::new(random_seed);
    let starting_hand = (0..hand_size)
        .map(|_| Tile::create_fully_connected(&mut rng))
        .collect();

    let hexagons = [
        HexagonPosition { x: 0, y: 0 },
        HexagonPosition { x: 0, y: 1 },
        HexagonPosition { x: 1, y: 0 },
        HexagonPosition { x: 1, y: 1 },
    ]
    .into();

    let mission = MissionEntry {
        id: mission_id,
        kind: hexagon_db::MissionKind::HighScore,
        name: "Tutorial #1".into(),
        description: "Move to the green target arrow".into(),
        number: 100,
        json: hexagon_types::Mission::HighScore(hexagon_types::MissionHighscore::V1(
            MissionHighscoreV1 {
                board: Board::create_board_from_hexagons(
                    hexagons,
                    hexagon_types::OuterConnectors::OnlyDeathEnds,
                )
                .unwrap(),
                starting_point: ConnectorId(0),
                random_seed: hand_size,
                starting_hand: starting_hand,
                winning_condition: WinningConditionHighscore {
                    min_velocity: None,
                    min_distance: None,
                    target: Some(ConnectorId(8)),
                },
            },
        )),
    };
    db.upsert_mission(&mission).await.unwrap();
    tracing::info!("Done")
}
