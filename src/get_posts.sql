
CREATE VIEW post_tag_total2 AS
SELECT  p.post_id, p.title, p.creation_date,
        p.author_id,
        p.category_id,
        array_agg(t.tag_id) as tagIds, array_agg(t.name) as tagNames,
        p.content,
        p.photo,
        p.extra_photos
FROM post p
            INNER JOIN post_tag pt ON p.post_id = pt.post_id
            INNER JOIN tag t ON pt.tag_id = t.tag_id
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
 

