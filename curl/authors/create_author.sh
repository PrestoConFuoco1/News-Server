#!/bin/bash
curl -v -G -d "token=admin&user_id=$1&description=$2" localhost:5555/authors/create
echo ""
