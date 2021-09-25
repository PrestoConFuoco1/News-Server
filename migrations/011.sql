CREATE TABLE news.draft_tag (
    draft_id integer NOT NULL REFERENCES news.draft(draft_id) ON DELETE CASCADE,
    tag_id  integer NOT NULL REFERENCES news.tag(tag_id) ON DELETE CASCADE,
    UNIQUE(draft_id, tag_id)
);

