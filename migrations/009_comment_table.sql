CREATE SEQUENCE news.comment_comment_id_seq;
CREATE TABLE news.comment (
    comment_id integer PRIMARY KEY NOT NULL DEFAULT nextval('news.comment_comment_id_seq'),
    post_id integer NOT NULL REFERENCES news.post(post_id) ON DELETE CASCADE,
    content text NOT NULL CHECK (content <> ''::text),
    user_id integer NOT NULL DEFAULT 1 REFERENCES news.users(user_id) ON DELETE SET DEFAULT
);
ALTER SEQUENCE news.comment_comment_id_seq OWNED BY news.comment.comment_id;


