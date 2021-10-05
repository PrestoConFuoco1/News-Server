#!/bin/bash
curl -G -d "token=$1&title=Draft1&tags=$2&category_id=1&content=Draft1_13_content" localhost:5555/drafts/create
echo ""
