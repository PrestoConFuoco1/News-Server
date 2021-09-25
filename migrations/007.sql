CREATE SEQUENCE news.post_post_id_seq;
CREATE TABLE news.post (
    post_id integer PRIMARY KEY NOT NULL DEFAULT nextval('news.post_post_id_seq'),
    title text NOT NULL CHECK (title <> ''::text),
    creation_date date NOT NULL DEFAULT CURRENT_DATE,
    author_id integer NOT NULL DEFAULT 1 REFERENCES news.author(author_id) ON DELETE SET DEFAULT,
    category_id integer NOT NULL DEFAULT 1 REFERENCES news.category(category_id) ON DELETE SET DEFAULT,
    content text NOT NULL CHECK (content <> ''::text),
    photo text CHECK (photo <> ''::text),
    extra_photos text[]
);
ALTER SEQUENCE news.post_post_id_seq OWNED BY news.post.post_id;

