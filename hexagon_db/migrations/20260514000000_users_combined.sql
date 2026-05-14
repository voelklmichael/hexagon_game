CREATE TABLE users (
    id              UUID NOT NULL PRIMARY KEY DEFAULT gen_random_uuid(),
    password_hash   TEXT NOT NULL,
    name            TEXT NOT NULL UNIQUE,
    email           TEXT NOT NULL UNIQUE
);

