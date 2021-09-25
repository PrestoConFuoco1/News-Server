
CREATE OR REPLACE VIEW news.get_categories AS
 WITH RECURSIVE temp2(category_id, arrcid, arrname) AS (
         SELECT c1.category_id,
            ARRAY[]::integer[] AS "array",
            ARRAY[]::text[] AS "array"
           FROM news.category c1
          WHERE c1.parent_category_id IS NULL
        UNION ALL
         SELECT c.category_id,
            array_prepend(c.category_id, t.arrcid) AS array_prepend,
            array_prepend(c.name, t.arrname) AS array_prepend
           FROM news.category c
             JOIN temp2 t ON c.parent_category_id = t.category_id
          WHERE c.parent_category_id IS NOT NULL
        )
 SELECT temp2.category_id,
    temp2.arrcid AS catids,
    temp2.arrname AS catnames
   FROM temp2;


