DROP TABLE users_password;
DROP TABLE users_data;

CREATE TABLE users (
    id              UUID NOT NULL PRIMARY KEY,
    password_hash   TEXT NOT NULL,
    name            TEXT NOT NULL UNIQUE,
    email           TEXT NOT NULL UNIQUE
);

