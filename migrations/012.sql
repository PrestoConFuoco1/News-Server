
CREATE OR REPLACE VIEW news.get_authors AS
 SELECT a.author_id,
    a.description,
    a.user_id,
    u.firstname,
    u.lastname,
    u.image,
    u.login,
    '***'::text AS pass,
    u.creation_date
   FROM news.author a
     JOIN news.users u ON a.user_id = u.user_id;

-- view 2
-- view 3
-- 014
-- view 4
-- 015
-- view 5
-- 016
-- view 6
-- 017
-- view 7
-- 018
-- view 8
-- 019

-- view 9
-- 020

