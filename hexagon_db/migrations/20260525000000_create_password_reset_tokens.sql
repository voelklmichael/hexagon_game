CREATE TABLE password_reset_tokens (
    token      UUID        NOT NULL PRIMARY KEY DEFAULT gen_random_uuid(),
    user_id    UUID        NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    expires_at TIMESTAMPTZ NOT NULL DEFAULT NOW() + INTERVAL '1 hour',
    sent_at    TIMESTAMPTZ
);
