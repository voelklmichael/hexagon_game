use hexagon_frontend_reqwest::{ApiClient, ApiError};
use hexagon_types::*;
use uuid::Uuid;

fn assert_status(err: ApiError, expected: u16, label: &str) {
    match err {
        ApiError::Status { status, .. } if status == expected => {
            println!("{label}: correctly rejected with {expected}");
        }
        other => panic!("{label}: unexpected error: {other:?}"),
    }
}

fn print_peak(label: &str, peak: &DBHighscorePeak) {
    let mut players: Vec<_> = peak.players.iter().collect();
    players.sort_by_key(|(id, _)| *id);
    for (id, stats) in players {
        println!(
            "{label}: player {id} — max_velocity={}, total_distance={}",
            stats.max_velocity, stats.total_distance
        );
    }
}

fn sample_score(user_id: Uuid, mission_id: Uuid) -> DBHighscore {
    DBHighscore {
        user_id,
        mission_id,
        players: [
            (
                0u8,
                PlayerStats {
                    max_velocity: 120,
                    total_distance: 5000,
                },
            ),
            (
                1u8,
                PlayerStats {
                    max_velocity: 80,
                    total_distance: 3200,
                },
            ),
        ]
        .into(),
    }
}

#[tokio::main]
async fn main() {
    let base_url =
        std::env::var("API_BASE_URL").unwrap_or_else(|_| "http://localhost:3000".to_string());
    dbg!(&base_url);
    let client = ApiClient::new(&base_url).expect("failed to build client");

    let id_a = Uuid::new_v4();
    let email_a = format!("{id_a}@example.com");
    let pw = "pw";

    let id_b = Uuid::new_v4();
    let email_b = format!("{id_b}@example.com");

    // 1. Health check
    client.healthz().await.expect("healthz failed");
    println!("healthz: OK");

    // 2. Create two users
    let user_id_a = client
        .create_user(pw, &email_a, &email_a)
        .await
        .expect("create_user a failed");
    println!("create_user a: {user_id_a}");

    let user_id_b = client
        .create_user(pw, &email_b, &email_b)
        .await
        .expect("create_user b failed");
    println!("create_user b: {user_id_b}");

    // 3. Login as user a
    let next = client
        .login(&email_a, pw, None)
        .await
        .expect("login failed");
    println!("login a: next={:?}", next.next);

    // 4. Submit a highscore as user a
    let mission_id = Uuid::new_v4();
    client
        .upsert_highscore(&sample_score(user_id_a, mission_id))
        .await
        .expect("upsert_highscore failed");
    println!("upsert_highscore: OK");

    // 5. Fetch user a's own highscore
    let user_peak = client
        .fetch_highscore_user(user_id_a, mission_id)
        .await
        .expect("fetch_highscore_user failed");
    print_peak("fetch_highscore_user", &user_peak);

    // 6. Fetch the overall highscore for the mission
    let overall_peak = client
        .fetch_highscore_overall(mission_id)
        .await
        .expect("fetch_highscore_overall failed");
    print_peak("fetch_highscore_overall", &overall_peak);

    // 7. Try to upsert/fetch for user b while logged in as user a — must be rejected with 403
    let err = client
        .upsert_highscore(&sample_score(user_id_b, mission_id))
        .await
        .expect_err("upsert_highscore for other user should be rejected");
    assert_status(err, 403, "upsert_highscore (wrong user)");

    let err = client
        .fetch_highscore_user(user_id_b, mission_id)
        .await
        .expect_err("fetch_highscore_user for other user should be rejected");
    assert_status(err, 403, "fetch_highscore_user (wrong user)");

    // 8. Fetch list of won missions for user a
    let won = client
        .fetch_won_missions(user_id_a)
        .await
        .expect("fetch_won_missions failed");
    assert!(
        won.contains(&mission_id),
        "mission should appear in won list"
    );
    println!("fetch_won_missions: {:?}", won);

    // 9. Fetching another user's won missions must be rejected with 403
    let err = client
        .fetch_won_missions(user_id_b)
        .await
        .expect_err("fetch_won_missions for other user should be rejected");
    assert_status(err, 403, "fetch_won_missions (wrong user)");

    // 10. Logout
    client.logout().await.expect("logout failed");
    println!("logout: OK");

    // 11. Try to upsert/fetch while not logged in — must be rejected with 401
    let err = client
        .upsert_highscore(&sample_score(user_id_a, mission_id))
        .await
        .expect_err("upsert_highscore should require login");
    assert_status(err, 401, "upsert_highscore (logged out)");

    let err = client
        .fetch_highscore_user(user_id_a, mission_id)
        .await
        .expect_err("fetch_highscore_user should require login");
    assert_status(err, 401, "fetch_highscore_user (logged out)");
}
