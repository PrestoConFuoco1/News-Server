CREATE SEQUENCE news.tag_tag_id_seq;
CREATE TABLE news.tag (
    tag_id integer PRIMARY KEY NOT NULL DEFAULT nextval('news.tag_tag_id_seq'),
    name text UNIQUE NOT NULL CHECK (name <> ''::text)
);
ALTER SEQUENCE news.tag_tag_id_seq OWNED BY news.tag.tag_id;
 
