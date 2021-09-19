#!/bin/bash
curl -G -d "token=$1&author_id=$2" localhost:5555/authors/delete
echo ""
