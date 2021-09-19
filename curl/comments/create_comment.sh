#!/bin/bash
curl -G -d "token=$1&post_id=$2&content=$3" localhost:5555/comments/create
echo ""
