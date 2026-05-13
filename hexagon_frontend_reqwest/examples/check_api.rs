use hexagon_db::DBHighscore;
use hexagon_frontend_reqwest::ApiClient;
use uuid::Uuid;

#[tokio::main]
async fn main() {
    let base_url =
        std::env::var("API_BASE_URL").unwrap_or_else(|_| "http://localhost:3000".to_string());
    dbg!(&base_url);
    let client = ApiClient::new(&base_url).expect("failed to build client");

    // 1. Health check
    client.healthz().await.expect("healthz failed");
    println!("healthz: OK");

    // 2. Create a user
    let user_id = client
        .create_user("test_user4", "hunter2")
        .await
        .expect("create_user failed");
    println!("create_user: {user_id}");

    // 3. Login
    let next = client
        .login("test_user", "hunter2", None)
        .await
        .expect("login failed");
    println!("login: next={:?}", next.next);

    // 4. Submit a highscore
    let mission_id = Uuid::new_v4();
    client
        .upsert_highscore(&DBHighscore {
            user_id,
            mission_id,
            max_velocity: 120,
            total_distance: 5000,
        })
        .await
        .expect("upsert_highscore failed");
    println!("upsert_highscore: OK");

    // 5. Fetch the user's highscore
    let user_peak = client
        .fetch_highscore_user(user_id, mission_id)
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

    // 7. Logout
    client.logout().await.expect("logout failed");
    println!("logout: OK");
}
