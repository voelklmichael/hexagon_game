use hexagon_db::DBConfig;
use uuid::Uuid;

#[tokio::main]
async fn main() {
    let config = DBConfig::local_docker();
    println!("{:?}", &config);
    let db = config.connect().await.unwrap();

    let user_name = Uuid::new_v4().to_string();
    let password_hash = "password";
    assert!(
        db.fetch_user_password_by_name(&user_name)
            .await
            .unwrap()
            .is_none()
    );
    assert!(
        db.fetch_user_password_by_id(Uuid::new_v4())
            .await
            .unwrap()
            .is_none()
    );
    let user_id = db.create_user(&user_name, password_hash).await.unwrap();

    let returned = db
        .fetch_user_password_by_id(user_id)
        .await
        .unwrap()
        .unwrap();
    assert_eq!(returned.id, user_id);
    assert_eq!(returned.user_name, user_name);
    assert_eq!(returned.password_hash, password_hash);

    let returned = db
        .fetch_user_password_by_name(&user_name)
        .await
        .unwrap()
        .unwrap();
    assert_eq!(returned.id, user_id);
    assert_eq!(returned.user_name, user_name);
    assert_eq!(returned.password_hash, password_hash);

    println!("Success!");
}
