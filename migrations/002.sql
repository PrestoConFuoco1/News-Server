CREATE SEQUENCE news.category_category_id_seq;
CREATE TABLE news.category (
    category_id integer PRIMARY KEY NOT NULL DEFAULT nextval('news.category_category_id_seq'),
    parent_category_id integer DEFAULT 1 REFERENCES news.category(category_id) ON DELETE CASCADE,
    name text UNIQUE NOT NULL CHECK (name <> ''::text)
);
ALTER SEQUENCE news.category_category_id_seq OWNED BY news.category.category_id;


