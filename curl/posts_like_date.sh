#!/bin/bash
#curl -G -d "search=is" localhost:5555/posts
curl -G -d "search=$1&created_at=$2" localhost:5555/posts
echo ""
