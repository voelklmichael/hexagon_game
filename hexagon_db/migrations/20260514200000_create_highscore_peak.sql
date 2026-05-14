-- Stores the all-time best values per mission across all players.
-- Updated on every highscore upsert; keyed on mission_id alone for O(1) lookup.
CREATE TABLE highscore_peak (
    mission_id               UUID   NOT NULL PRIMARY KEY,
    player_0_max_velocity    BIGINT NOT NULL DEFAULT 0,
    player_0_total_distance  BIGINT NOT NULL DEFAULT 0,
    player_1_max_velocity    BIGINT NOT NULL DEFAULT 0,
    player_1_total_distance  BIGINT NOT NULL DEFAULT 0,
    player_2_max_velocity    BIGINT NOT NULL DEFAULT 0,
    player_2_total_distance  BIGINT NOT NULL DEFAULT 0,
    player_3_max_velocity    BIGINT NOT NULL DEFAULT 0,
    player_3_total_distance  BIGINT NOT NULL DEFAULT 0,
    player_4_max_velocity    BIGINT NOT NULL DEFAULT 0,
    player_4_total_distance  BIGINT NOT NULL DEFAULT 0,
    player_5_max_velocity    BIGINT NOT NULL DEFAULT 0,
    player_5_total_distance  BIGINT NOT NULL DEFAULT 0,
    player_6_max_velocity    BIGINT NOT NULL DEFAULT 0,
    player_6_total_distance  BIGINT NOT NULL DEFAULT 0,
    player_7_max_velocity    BIGINT NOT NULL DEFAULT 0,
    player_7_total_distance  BIGINT NOT NULL DEFAULT 0,
    player_8_max_velocity    BIGINT NOT NULL DEFAULT 0,
    player_8_total_distance  BIGINT NOT NULL DEFAULT 0,
    player_9_max_velocity    BIGINT NOT NULL DEFAULT 0,
    player_9_total_distance  BIGINT NOT NULL DEFAULT 0
);
