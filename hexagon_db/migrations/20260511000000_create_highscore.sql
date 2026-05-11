CREATE TABLE highscore (
    user_id       UUID   NOT NULL,
    mission_id    UUID   NOT NULL,
    max_velocity  BIGINT NOT NULL,
    total_distance BIGINT NOT NULL,
    history       JSONB  NOT NULL DEFAULT '[]',
    PRIMARY KEY (user_id, mission_id)
);
