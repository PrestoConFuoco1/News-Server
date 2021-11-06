
CREATE OR REPLACE VIEW news.get_comments AS
 SELECT c.comment_id,
    c.content,
    c.post_id,
    u.user_id,
    u.firstname,
    u.lastname,
    u.image,
    u.login,
    '***'::text AS pass_hash,
    u.creation_date,
    NULL::text AS is_admin
   FROM news.comment c
     JOIN news.users u ON c.user_id = u.user_id;


