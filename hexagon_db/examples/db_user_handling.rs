use hexagon_db::DBConfig;
use uuid::Uuid;

#[tokio::main]
async fn main() {
    let config = DBConfig::local_docker();
    println!("{:?}", &config);
    let db = config.connect().await.unwrap();

    let id = Uuid::new_v4();
    let user_name = Uuid::new_v4().to_string();
    let password_hash = "password";
    let name = "Test User";
    let email = "test@example.com";

    assert!(db.fetch_user_by_name(&user_name).await.unwrap().is_none());
    assert!(db.fetch_user_by_id(id).await.unwrap().is_none());

    let user_id = db
        .create_user(id, &user_name, password_hash, name, email)
        .await
        .unwrap();

    let returned = db.fetch_user_by_id(user_id).await.unwrap().unwrap();
    assert_eq!(returned.id, user_id);
    assert_eq!(returned.user_name, user_name);
    assert_eq!(returned.password_hash, password_hash);
    assert_eq!(returned.name, name);
    assert_eq!(returned.email, email);

    let returned = db.fetch_user_by_name(&user_name).await.unwrap().unwrap();
    assert_eq!(returned.id, user_id);
    assert_eq!(returned.user_name, user_name);
    assert_eq!(returned.password_hash, password_hash);
    assert_eq!(returned.name, name);
    assert_eq!(returned.email, email);

    println!("Success!");
}
