CREATE SEQUENCE news.users_user_id_seq;
CREATE TABLE news.users (
    user_id integer PRIMARY KEY NOT NULL DEFAULT nextval('news.users_user_id_seq'),
    firstname text NOT NULL CHECK (firstname <> ''::text),
    lastname text  NOT NULL CHECK (lastname <>  ''::text),
    image text CHECK (image <> ''::text),
    login text UNIQUE NOT NULL CHECK (login <> ''::text),
    pass_hash text NOT NULL CHECK (pass_hash <> ''::text),
    creation_date date NOT NULL DEFAULT CURRENT_DATE,
    is_admin boolean NOT NULL DEFAULT false
);
ALTER SEQUENCE news.users_user_id_seq OWNED BY news.users.user_id;

