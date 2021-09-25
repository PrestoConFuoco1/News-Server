
CREATE OR REPLACE VIEW news.get_drafts AS
 SELECT td.draft_id,
    td.title,
    td.creation_date AS draft_creation_date,
    td.author_id,
    a.description AS author_description,
    u.user_id,
    u.firstname AS user_firstname,
    u.lastname AS user_lastname,
    u.image AS user_image,
    u.login AS user_login,
    '***'::text AS user_pass,
    u.creation_date AS user_creation_date,
    NULL::text AS user_is_admin,
    td.tagids,
    td.tagnames,
    c.catids,
    c.catnames,
    td.content,
    td.photo,
    td.extra_photos,
    td.post_id
   FROM news.draft_tag_total td
     JOIN news.author a ON td.author_id = a.author_id
     JOIN news.users u ON a.user_id = u.user_id
     JOIN news.get_categories c ON td.category_id = c.category_id;


