#!/bin/bash
curl -v -G -d "token=admin&author_id=$1&description=$2" localhost:5555/authors/edit
#curl -v -G -d "token=admin&" localhost:5555/authors/edit
echo ""
