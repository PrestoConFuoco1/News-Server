#!/bin/bash
#curl -G -d "search=is" localhost:5555/posts
curl -G -d "search=$1" localhost:5555/posts
echo ""
