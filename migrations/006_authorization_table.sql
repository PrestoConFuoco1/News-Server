CREATE TABLE news.token (
    user_id integer UNIQUE NOT NULL REFERENCES news.users(user_id) ON DELETE CASCADE,
    token text PRIMARY KEY NOT NULL CHECK (token <> ''::text)
);

