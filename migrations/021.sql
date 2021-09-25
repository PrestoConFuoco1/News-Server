

INSERT INTO news.users (firstname, lastname, login, pass_hash, is_admin) VALUES
('deleted', 'deleted', 'deleted', '123', false),
('Aleksandr', 'Pushkin', 'Pyshkin', '123', false),
('admin', 'admin', 'admin', '123', true),
('Franz', 'Liszt', 'Composer666', '123', false),
('Sergei', 'Prokofiev', 'ComplicatedHarmony', '123', false);

INSERT INTO news.author (user_id, description) VALUES
(1, 'deleted'),
(2, 'Pushkin nashe vsyo'),
(3, 'admin');

INSERT INTO news.token (user_id, token) VALUES
(2, 'push'),
(3, 'admin'),
(4,'fail'),
(5,'prok');

INSERT INTO news.category (parent_category_id, name) VALUES
(NULL, 'All'),
(1, 'Programming'),
(1, 'Music'),
(1, 'Football'),
(2, 'Functional'),
(5, 'Haskell'),
(3, 'Classical music'),
(4, 'Champions league'),
(4, 'Europa league');

INSERT INTO news.tag (name) VALUES
('sports'),
('IT'),
('music'),
('cats'),
('animals'),
('hi-tech'),
('others');


