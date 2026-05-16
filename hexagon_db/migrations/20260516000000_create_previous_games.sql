CREATE TABLE previous_games (
    id         BIGSERIAL NOT NULL,
    user_id    UUID      NOT NULL REFERENCES users(id),
    mission_id UUID      NOT NULL REFERENCES missions(id),
    game_state JSONB     NOT NULL,
    PRIMARY KEY (id)
);
