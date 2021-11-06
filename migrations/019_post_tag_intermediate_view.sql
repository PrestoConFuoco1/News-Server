
CREATE OR REPLACE VIEW news.post_tag_total AS
 SELECT p.post_id,
    p.title,
    p.creation_date,
    p.author_id,
    p.category_id,
    COALESCE(array_agg(t.tag_id) FILTER (WHERE t.tag_id IS NOT NULL), ARRAY[]::integer[]) AS tagids,
    COALESCE(array_agg(t.name) FILTER (WHERE t.name IS NOT NULL), ARRAY[]::text[]) AS tagnames,
    p.content,
    p.photo,
    COALESCE(p.extra_photos, ARRAY[]::text[]) AS extra_photos
   FROM news.post p
     LEFT JOIN news.post_tag pt ON p.post_id = pt.post_id
     LEFT JOIN news.tag t ON pt.tag_id = t.tag_id
  GROUP BY p.post_id;


