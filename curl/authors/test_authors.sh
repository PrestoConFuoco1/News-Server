#!/bin/bash

echo "getting authors"
./authors_get.sh
echo "trying to create author with wrong token"
./create_author.sh asdadada 2 "Pushkin1"
echo "trying to create author not being admin"
./create_author.sh push 2 "Pushkin2"
echo "trying to create author with admin but with invalid user_id"
./create_author.sh admin 666 "Pushkin3"
echo "trying to create author with admin but with invalid description"
./create_author.sh admin 2 ""
echo "creating author with admin"
./create_author.sh admin 2 "Pushkin4"

