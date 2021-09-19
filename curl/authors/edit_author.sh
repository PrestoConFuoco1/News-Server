#!/bin/bash
curl -G -d "token=$1&author_id=$2&description=$3" localhost:5555/authors/edit
#curl -v -G -d "token=admin&" localhost:5555/authors/edit
echo ""
