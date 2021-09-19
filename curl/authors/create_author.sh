#!/bin/bash
curl -G -d "token=$1&user_id=$2&description=$3" localhost:5555/authors/create
echo ""
