CREATE SEQUENCE news.draft_draft_id_seq;
CREATE TABLE news.draft (
    draft_id integer PRIMARY KEY NOT NULL DEFAULT nextval('news.draft_draft_id_seq'),
    title text NOT NULL CHECK (title <> ''::text),
    creation_date date NOT NULL DEFAULT CURRENT_DATE,
    author_id integer NOT NULL DEFAULT 1 REFERENCES news.author(author_id) ON DELETE SET DEFAULT,
    category_id integer NOT NULL DEFAULT 1 REFERENCES news.category(category_id) ON DELETE SET DEFAULT,
    content text NOT NULL CHECK (content <> ''::text),
    photo text CHECK (photo <> ''::text),
    extra_photos text[],
    post_id integer UNIQUE REFERENCES news.post(post_id) ON DELETE SET NULL
);
ALTER SEQUENCE news.draft_draft_id_seq OWNED BY news.draft.draft_id;

