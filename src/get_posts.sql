--------------------------------------------------------------------


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
            INNER JOIN users u ON a.user_id = u.user_id
            INNER JOIN get_categories c ON tp.category_id = c.category_id;



-- CREATE VIEW post_tag_total_left_fnull AS
CREATE VIEW draft_tag_total AS
SELECT  a.draft_id, a.title, a.creation_date,
        a.author_id,
        a.category_id,
        COALESCE(array_agg(t.tag_id) FILTER (WHERE t.tag_id IS NOT NULL), ARRAY[]::integer[]) as tagIds,
        COALESCE(array_agg(t.name) FILTER (WHERE t.name IS NOT NULL), ARRAY[]::name[]) as tagNames,
        a.content,
        a.photo,
        a.extra_photos,
        a.post_id
FROM draft a
            LEFT JOIN draft_tag dt ON a.draft_id = dt.draft_id
            LEFT JOIN tag t ON dt.tag_id = t.tag_id
GROUP BY a.draft_id;





CREATE VIEW get_drafts AS
SELECT  td.draft_id AS draft_id,
        td.title AS title,
        td.creation_date AS draft_creation_date,
        td.author_id as author_id, a.description AS author_description,
            u.user_id AS user_id,
            u.firstname AS user_firstname,
            u.lastname AS user_lastname,
            u.image AS user_image,
            u.login AS user_login,
            '***' as user_pass,
            u.creation_date AS user_creation_date,
            NULL as user_is_admin,
        -- COALESCE(td.tagIds, ARRAY[]::integer[]) AS tagIds, COALESCE(td.tagNames, ARRAY[]::text[]) AS tagNames,
        td.tagIds AS tagIds, td.tagNames AS tagNames,
        c.catIds AS catIds, c.catNames AS catNames,
        td.content AS content,
        td.photo AS photo,
        td.extra_photos AS extra_photos,
        td.post_id AS post_id
FROM draft_tag_total td
            INNER JOIN author a ON td.author_id = a.author_id
            INNER JOIN users u ON a.user_id = u.user_id
            INNER JOIN get_categories c ON td.category_id = c.category_id;


