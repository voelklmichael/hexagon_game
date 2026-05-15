use hexagon_db::DBConfig;
use uuid::Uuid;

#[tokio::main]
async fn main() {
    let config = DBConfig::local_docker();
    println!("{:?}", &config);
    let db = config.connect().await.unwrap();

    let tag = Uuid::new_v4();
    let password_hash = "password";
    let name = &tag.to_string();
    let email = &format!("{tag}@example.com");

    assert!(db.fetch_user_by_email(email).await.unwrap().is_none());

    let user_id = db.create_user(password_hash, name, email).await.unwrap();

    let returned = db.fetch_user_by_id(user_id).await.unwrap().unwrap();
    assert_eq!(returned.id, user_id);
    assert_eq!(returned.password_hash, password_hash);
    assert_eq!(&returned.email, email);

    let returned = db.fetch_user_by_email(email).await.unwrap().unwrap();
    assert_eq!(returned.id, user_id);
    assert_eq!(returned.password_hash, password_hash);
    assert_eq!(&returned.email, email);

    println!("Success!");
}
