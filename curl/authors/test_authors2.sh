#!/bin/bash
echo "trying to edit author without token"
./edit_author.sh "" $1 "Pushkin5"
echo "trying to edit author with wrong token"
./edit_author.sh sdadsasda $1 "Pushkin6"
echo "trying to edit author with right token but not being admin"
./edit_author.sh push $1 "Pushkin7"
echo "trying to edit author with all params empty"
./edit_author.sh admin "" ""
echo "editing author with admin"
./edit_author.sh admin $1 "Pushkin8"
echo "deleting author with admin"
./delete_author.sh admin $1
