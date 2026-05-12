CREATE TABLE users_password (
    id              UUID NOT NULL PRIMARY KEY,
    password_hash   TEXT NOT NULL,
    user_name       TEXT NOT NULL UNIQUE
);

CREATE TABLE users_data (
    id              UUID NOT NULL PRIMARY KEY,
    name            TEXT NOT NULL,
    email           TEXT NOT NULL
);
