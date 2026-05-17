use hexagon_db::{DB, DBConfig, MissionEntry};
use hexagon_types::{
    Board, ConnectorEdgeSub, ConnectorId, Edge, EdgeSub, HexagonPosition, MissionHighscoreV1,
    RandomNumberGenerator, Sub, Tile, WinningConditionHighscoreV1,
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

    upsert_mission_1(&db).await;
    upsert_mission_2(&db).await;
    upsert_mission_3(&db).await;
    upsert_mission_4(&db).await;
    upsert_mission_5(&db).await;
    upsert_mission_6(&db).await;

    tracing::info!("Done")
}

async fn upsert_mission_1(db: &DB) {
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
        number: number,
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
                winning_condition: WinningConditionHighscoreV1 {
                    min_velocity: None,
                    min_distance: None,
                    target: Some(ConnectorId(25)),
                },
            },
        )),
    };
    db.upsert_mission(&mission).await.unwrap();
}

async fn upsert_mission_2(db: &DB) {
    let number = 200u32;
    let mission_id = Uuid::from_u128(number as u128);

    let hand_size = 3;
    let random_seed = 31;

    let mut rng = RandomNumberGenerator::new(random_seed);
    let starting_hand = (0..hand_size)
        .map(|_| Tile::create_fully_connected(&mut rng))
        .collect();

    let mission = MissionEntry {
        id: mission_id,
        kind: hexagon_db::MissionKind::HighScore,
        name: "Tutorial #2".into(),
        description: "Move to the green target arrow".into(),
        number: number,
        json: hexagon_types::Mission::HighScore(hexagon_types::MissionHighscore::V1(
            MissionHighscoreV1 {
                board: Board::create_board(3, hexagon_types::OuterConnectors::OnlyDeathEnds)
                    .unwrap(),
                starting_point: ConnectorId(3),
                random_seed: hand_size,
                starting_hand: starting_hand,
                winning_condition: WinningConditionHighscoreV1 {
                    min_velocity: None,
                    min_distance: None,
                    target: Some(ConnectorId(32)),
                },
            },
        )),
    };
    db.upsert_mission(&mission).await.unwrap();
}

async fn upsert_mission_3(db: &DB) {
    let number = 300u32;
    let mission_id = Uuid::from_u128(number as u128);

    let hand_size = 3;
    let random_seed = 65;

    let mut rng = RandomNumberGenerator::new(random_seed);
    let starting_hand = (0..hand_size)
        .map(|_| Tile::create_fully_connected(&mut rng))
        .collect();

    let mut board = Board::create_board(3, hexagon_types::OuterConnectors::OnlyDeathEnds).unwrap();
    let tile = Tile {
        inner_connectors: Edge::opposites()
            .into_iter()
            .map(|(a, b)| ConnectorEdgeSub {
                a: EdgeSub {
                    edge: a,
                    sub: Sub::Left,
                },
                b: EdgeSub {
                    edge: b,
                    sub: Sub::Right,
                },
            })
            .collect(),
    };
    board.play_tile(HexagonPosition { x: 0, y: 0 }, tile);

    let mission = MissionEntry {
        id: mission_id,
        kind: hexagon_db::MissionKind::HighScore,
        name: "Tutorial #3".into(),
        description: "Move to the green target arrow".into(),
        number: number,
        json: hexagon_types::Mission::HighScore(hexagon_types::MissionHighscore::V1(
            MissionHighscoreV1 {
                board,
                starting_point: ConnectorId(3),
                random_seed: hand_size,
                starting_hand: starting_hand,
                winning_condition: WinningConditionHighscoreV1 {
                    min_velocity: None,
                    min_distance: None,
                    target: Some(ConnectorId(32)),
                },
            },
        )),
    };
    db.upsert_mission(&mission).await.unwrap();
}

async fn upsert_mission_4(db: &DB) {
    let number = 400u32;
    let mission_id = Uuid::from_u128(number as u128);

    let hand_size = 3;
    let random_seed = 3446;

    let mut rng = RandomNumberGenerator::new(random_seed);
    let starting_hand = (0..hand_size)
        .map(|_| Tile::create_fully_connected(&mut rng))
        .collect();

    let mut board = Board::create_board(3, hexagon_types::OuterConnectors::OnlyDeathEnds).unwrap();
    let tile = Tile {
        inner_connectors: Edge::variants()
            .iter()
            .map(|&edge| ConnectorEdgeSub {
                a: EdgeSub {
                    edge,
                    sub: Sub::Left,
                },
                b: EdgeSub {
                    edge,
                    sub: Sub::Right,
                },
            })
            .collect(),
    };
    board.play_tile(HexagonPosition { x: 0, y: 0 }, tile);

    let mission = MissionEntry {
        id: mission_id,
        kind: hexagon_db::MissionKind::HighScore,
        name: "Tutorial #4".into(),
        description: "Move to the green target arrow".into(),
        number: number,
        json: hexagon_types::Mission::HighScore(hexagon_types::MissionHighscore::V1(
            MissionHighscoreV1 {
                board,
                starting_point: ConnectorId(3),
                random_seed: hand_size,
                starting_hand: starting_hand,
                winning_condition: WinningConditionHighscoreV1 {
                    min_velocity: None,
                    min_distance: None,
                    target: Some(ConnectorId(32)),
                },
            },
        )),
    };
    db.upsert_mission(&mission).await.unwrap();
}

async fn upsert_mission_5(db: &DB) {
    let number = 500u32;
    let mission_id = Uuid::from_u128(number as u128);

    let hand_size = 3;
    let random_seed = 343546;

    let mut rng = RandomNumberGenerator::new(random_seed);
    let starting_hand = (0..hand_size)
        .map(|_| Tile::create_fully_connected(&mut rng))
        .collect();

    let mut board =
        Board::create_board(3, hexagon_types::OuterConnectors::ReducedDeathEnds).unwrap();
    let tile = Tile {
        inner_connectors: Edge::variants()
            .iter()
            .map(|&edge| ConnectorEdgeSub {
                a: EdgeSub {
                    edge,
                    sub: Sub::Left,
                },
                b: EdgeSub {
                    edge,
                    sub: Sub::Right,
                },
            })
            .collect(),
    };
    board.play_tile(HexagonPosition { x: 0, y: 0 }, tile);

    let mission = MissionEntry {
        id: mission_id,
        kind: hexagon_db::MissionKind::HighScore,
        name: "Tutorial #5".into(),
        description: "Move at least 10".into(),
        number: number,
        json: hexagon_types::Mission::HighScore(hexagon_types::MissionHighscore::V1(
            MissionHighscoreV1 {
                board,
                starting_point: ConnectorId(3),
                random_seed: hand_size,
                starting_hand: starting_hand,
                winning_condition: WinningConditionHighscoreV1 {
                    min_velocity: None,
                    min_distance: Some(10_000),
                    target: Some(ConnectorId(21)),
                },
            },
        )),
    };
    db.upsert_mission(&mission).await.unwrap();
}

async fn upsert_mission_6(db: &DB) {
    let number = 600u32;
    let mission_id = Uuid::from_u128(number as u128);

    let hand_size = 3;
    let random_seed = 2443546;

    let mut rng = RandomNumberGenerator::new(random_seed);
    let starting_hand = (0..hand_size)
        .map(|_| Tile::create_fully_connected(&mut rng))
        .collect();

    let mut board =
        Board::create_board(3, hexagon_types::OuterConnectors::ReducedDeathEnds).unwrap();
    let tile = Tile {
        inner_connectors: Edge::opposites()
            .into_iter()
            .map(|(a, b)| ConnectorEdgeSub {
                a: EdgeSub {
                    edge: a,
                    sub: Sub::Left,
                },
                b: EdgeSub {
                    edge: b,
                    sub: Sub::Right,
                },
            })
            .collect(),
    };
    board.play_tile(HexagonPosition { x: 0, y: 0 }, tile);

    let mission = MissionEntry {
        id: mission_id,
        kind: hexagon_db::MissionKind::HighScore,
        name: "Tutorial #6".into(),
        description: "Move at high speed: Travel 5 segments in one turn".into(),
        number: number,
        json: hexagon_types::Mission::HighScore(hexagon_types::MissionHighscore::V1(
            MissionHighscoreV1 {
                board,
                starting_point: ConnectorId(3),
                random_seed: hand_size,
                starting_hand: starting_hand,
                winning_condition: WinningConditionHighscoreV1 {
                    min_velocity: Some(5_000),
                    min_distance: None,
                    target: Some(ConnectorId(21)),
                },
            },
        )),
    };
    db.upsert_mission(&mission).await.unwrap();
}
