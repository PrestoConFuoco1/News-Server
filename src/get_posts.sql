
CREATE VIEW post_tag_total_left AS
SELECT  p.post_id, p.title, p.creation_date,
        p.author_id,
        p.category_id,
        array_agg(t.tag_id) as tagIds, array_agg(t.name) as tagNames,
        p.content,
        p.photo,
        p.extra_photos
FROM post p
            LEFT JOIN post_tag pt ON p.post_id = pt.post_id
            LEFT JOIN tag t ON pt.tag_id = t.tag_id
GROUP BY p.post_id;

CREATE VIEW get_posts AS
SELECT tp.post_id, tp.title, tp.creation_date,
        tp.author_id, a.description, u.user_id, u.firstname, u.lastname,
            u.image, u.login, '***' as pass, u.creation_date, NULL as is_admin,
        tp.tagIds, tp.tagNames,
        c.arrcid, c.arrname,
        tp.content,
        tp.photo,
        tp.extra_photos
FROM post_tag_total2 tp
            INNER JOIN author a ON tp.author_id = a.author_id
            INNER JOIN users u ON a.author_id = u.user_id
            INNER JOIN temp2 c ON tp.category_id = c.category_id;
 


CREATE VIEW post_tag_total_left_fnull AS
SELECT  p.post_id, p.title, p.creation_date,
        p.author_id,
        p.category_id,
        array_agg(t.tag_id) FILTER (WHERE t.tag_id IS NOT NULL) as tagIds,
        array_agg(t.name) FILTER (WHERE t.name IS NOT NULL) as tagNames,
        p.content,
        p.photo,
        p.extra_photos
FROM post p
            LEFT JOIN post_tag pt ON p.post_id = pt.post_id
            LEFT JOIN tag t ON pt.tag_id = t.tag_id
GROUP BY p.post_id;


CREATE VIEW get_posts_fnull AS
SELECT tp.post_id AS post_id, tp.title AS title, tp.creation_date AS post_creation_date,
        tp.author_id as author_id, a.description AS description, u.user_id AS user_id,
        u.firstname AS firstname, u.lastname AS lastname,
            u.image AS image, u.login AS login, '***' as pass, u.creation_date AS user_creation_date, NULL as is_admin,
        tp.tagIds AS tagIds, tp.tagNames AS tagNames,
        c.arrcid AS arrcid, c.arrname AS arrname,
        tp.content AS content,
        tp.photo AS photo,
        tp.extra_photos AS extra_photos
FROM post_tag_total2 tp
            INNER JOIN author a ON tp.author_id = a.author_id
            INNER JOIN users u ON a.author_id = u.user_id
            INNER JOIN temp2 c ON tp.category_id = c.category_id;
----------------------------------------------------------------------------------

-- CREATE VIEW post_tag_total_left_fnull AS
CREATE VIEW post_tag_total AS
SELECT  p.post_id, p.title, p.creation_date,
        p.author_id,
        p.category_id,
        array_agg(t.tag_id) FILTER (WHERE t.tag_id IS NOT NULL) as tagIds,
        array_agg(t.name) FILTER (WHERE t.name IS NOT NULL) as tagNames,
        p.content,
        p.photo,
        p.extra_photos
FROM post p
            LEFT JOIN post_tag pt ON p.post_id = pt.post_id
            LEFT JOIN tag t ON pt.tag_id = t.tag_id
GROUP BY p.post_id;


CREATE VIEW get_posts AS
SELECT  tp.post_id AS post_id,
        tp.title AS title,
        tp.creation_date AS post_creation_date,
        tp.author_id as author_id, a.description AS description,
            u.user_id AS user_id,
            u.firstname AS firstname,
            u.lastname AS lastname,
            u.image AS image,
            u.login AS login,
            '***' as pass,
            u.creation_date AS user_creation_date,
            NULL as is_admin,
        COALESCE(tp.tagIds, ARRAY[]::integer[]) AS tagIds, COALESCE(tp.tagNames, ARRAY[]::text[]) AS tagNames,
        c.catIds AS catIds, c.catNames AS catNames,
        tp.content AS content,
        tp.photo AS photo,
        tp.extra_photos AS extra_photos
FROM post_tag_total_left_fnull tp
            INNER JOIN author a ON tp.author_id = a.author_id
            INNER JOIN users u ON a.author_id = u.user_id
            INNER JOIN get_categories c ON tp.category_id = c.category_id;

--------------------------------------------------------------------


-- CREATE VIEW post_tag_total_left_fnull AS
CREATE VIEW post_tag_total AS
SELECT  p.post_id, p.title, p.creation_date,
        p.author_id,
        p.category_id,
        COALESCE(array_agg(t.tag_id) FILTER (WHERE t.tag_id IS NOT NULL), ARRAY[]::integer[]) as tagIds,
        COALESCE(array_agg(t.name) FILTER (WHERE t.name IS NOT NULL), ARRAY[]::name[]) as tagNames,
        p.content,
        p.photo,
        p.extra_photos
FROM post p
            LEFT JOIN post_tag pt ON p.post_id = pt.post_id
            LEFT JOIN tag t ON pt.tag_id = t.tag_id
GROUP BY p.post_id;


CREATE OR REPLACE VIEW get_posts AS
SELECT  tp.post_id AS post_id,
        tp.title AS title,
        tp.creation_date AS post_creation_date,
        tp.author_id as author_id, a.description AS author_description,
            u.user_id AS user_id,
            u.firstname AS user_firstname,
            u.lastname AS user_lastname,
            u.image AS user_image,
            u.login AS user_login,
            '***' as user_pass,
            u.creation_date AS user_creation_date,
            NULL as user_is_admin,
        -- COALESCE(tp.tagIds, ARRAY[]::integer[]) AS tagIds, COALESCE(tp.tagNames, ARRAY[]::text[]) AS tagNames,
        tp.tagIds AS tagIds, tp.tagNames AS tagNames,
        c.catIds AS catIds, c.catNames AS catNames,
        tp.content AS content,
        tp.photo AS photo,
        tp.extra_photos AS extra_photos
FROM post_tag_total tp
            INNER JOIN author a ON tp.author_id = a.author_id
            INNER JOIN users u ON a.author_id = u.user_id
            INNER JOIN get_categories c ON tp.category_id = c.category_id;


