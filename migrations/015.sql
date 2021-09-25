
CREATE OR REPLACE VIEW news.get_tags AS
 SELECT t.tag_id,
    t.name
   FROM news.tag t;


