CREATE TYPE mission_kind AS ENUM ('HighScore', 'Delivery');

CREATE TABLE missions (
    id     UUID         NOT NULL DEFAULT gen_random_uuid(),
    kind   mission_kind NOT NULL,
    name   TEXT         NOT NULL,
    number INTEGER      NOT NULL,
    json   JSONB        NOT NULL,
    PRIMARY KEY (id)
);
