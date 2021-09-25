
CREATE OR REPLACE VIEW news.draft_tag_total AS
 SELECT a.draft_id,
    a.title,
    a.creation_date,
    a.author_id,
    a.category_id,
    COALESCE(array_agg(t.tag_id) FILTER (WHERE t.tag_id IS NOT NULL), ARRAY[]::integer[]) AS tagids,
    COALESCE(array_agg(t.name) FILTER (WHERE t.name IS NOT NULL), ARRAY[]::name[]::text[]) AS tagnames,
    a.content,
    a.photo,
    a.extra_photos,
    a.post_id
   FROM news.draft a
     LEFT JOIN news.draft_tag dt ON a.draft_id = dt.draft_id
     LEFT JOIN news.tag t ON dt.tag_id = t.tag_id
  GROUP BY a.draft_id;


