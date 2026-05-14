DROP TABLE users_password;
DROP TABLE users_data;

CREATE TABLE users (
    id              UUID NOT NULL PRIMARY KEY,
    user_name       TEXT NOT NULL UNIQUE,
    password_hash   TEXT NOT NULL,
    name            TEXT NOT NULL,
    email           TEXT NOT NULL
);

