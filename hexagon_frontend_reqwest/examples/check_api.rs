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
        .upsert_highscore(&DBHighscore {
            user_id: user_id_a,
            mission_id,
            max_velocity: 120,
            total_distance: 5000,
        })
        .await
        .expect("upsert_highscore failed");
    println!("upsert_highscore: OK");

    // 5. Fetch user a's own highscore
    let user_peak = client
        .fetch_highscore_user(user_id_a, mission_id)
        .await
        .expect("fetch_highscore_user failed");
    println!(
        "fetch_highscore_user: max_velocity={}, total_distance={}",
        user_peak.max_velocity, user_peak.total_distance
    );

    // 6. Fetch the overall highscore for the mission
    let overall_peak = client
        .fetch_highscore_overall(mission_id)
        .await
        .expect("fetch_highscore_overall failed");
    println!(
        "fetch_highscore_overall: max_velocity={}, total_distance={}",
        overall_peak.max_velocity, overall_peak.total_distance
    );

    // 7. Try to upsert/fetch for user b while logged in as user a — must be rejected with 403
    let err = client
        .upsert_highscore(&DBHighscore {
            user_id: user_id_b,
            mission_id,
            max_velocity: 999,
            total_distance: 9999,
        })
        .await
        .expect_err("upsert_highscore for other user should be rejected");
    assert_status(err, 403, "upsert_highscore (wrong user)");

    let err = client
        .fetch_highscore_user(user_id_b, mission_id)
        .await
        .expect_err("fetch_highscore_user for other user should be rejected");
    assert_status(err, 403, "fetch_highscore_user (wrong user)");

    // 8. Logout
    client.logout().await.expect("logout failed");
    println!("logout: OK");

    // 9. Try to upsert/fetch while not logged in — must be rejected with 401
    let err = client
        .upsert_highscore(&DBHighscore {
            user_id: user_id_a,
            mission_id,
            max_velocity: 120,
            total_distance: 5000,
        })
        .await
        .expect_err("upsert_highscore should require login");
    assert_status(err, 401, "upsert_highscore (logged out)");

    let err = client
        .fetch_highscore_user(user_id_a, mission_id)
        .await
        .expect_err("fetch_highscore_user should require login");
    assert_status(err, 401, "fetch_highscore_user (logged out)");
}
