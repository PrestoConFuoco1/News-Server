
CREATE OR REPLACE VIEW news.get_users_by_token AS
 SELECT u.user_id,
    u.firstname,
    u.lastname,
    u.image,
    u.login,
    u.pass_hash,
    u.creation_date,
    u.is_admin,
    t.token
   FROM news.token t
     JOIN news.users u ON t.user_id = u.user_id;


