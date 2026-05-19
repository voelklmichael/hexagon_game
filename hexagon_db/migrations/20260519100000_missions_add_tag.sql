CREATE TYPE mission_tag AS ENUM ('Tutorial', 'Deliviery');

ALTER TABLE missions ADD COLUMN tag mission_tag NOT NULL DEFAULT 'Tutorial';
ALTER TABLE missions DROP COLUMN kind;
DROP TYPE mission_kind;
