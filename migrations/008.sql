CREATE TABLE news.post_tag (
    post_id integer NOT NULL REFERENCES news.post(post_id) ON DELETE CASCADE,
    tag_id  integer NOT NULL REFERENCES news.tag(tag_id) ON DELETE CASCADE,
    UNIQUE(post_id, tag_id)
);

