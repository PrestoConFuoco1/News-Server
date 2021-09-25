
CREATE OR REPLACE VIEW news.get_posts AS
 SELECT tp.post_id,
    tp.title,
    tp.creation_date AS post_creation_date,
    tp.author_id,
    a.description AS author_description,
    u.user_id,
    u.firstname AS user_firstname,
    u.lastname AS user_lastname,
    u.image AS user_image,
    u.login AS user_login,
    '***'::text AS user_pass,
    u.creation_date AS user_creation_date,
    NULL::text AS user_is_admin,
    tp.tagids,
    tp.tagnames,
    c.catids,
    c.catnames,
    tp.content,
    tp.photo,
    tp.extra_photos
   FROM news.post_tag_total tp
     JOIN news.author a ON tp.author_id = a.author_id
     JOIN news.users u ON a.user_id = u.user_id
     JOIN news.get_categories c ON tp.category_id = c.category_id;


