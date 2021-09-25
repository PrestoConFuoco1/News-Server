CREATE SEQUENCE news.author_author_id_seq;
CREATE TABLE news.author (
    author_id integer PRIMARY KEY NOT NULL DEFAULT nextval('news.author_author_id_seq'),
    user_id integer UNIQUE NOT NULL DEFAULT 1 REFERENCES news.users(user_id) ON DELETE CASCADE,
    description text UNIQUE NOT NULL CHECK (description <> ''::text)
);
ALTER SEQUENCE news.author_author_id_seq OWNED BY news.author.author_id;

