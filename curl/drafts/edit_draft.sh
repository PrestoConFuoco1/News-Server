#!/bin/bash
curl -G -d "token=$1&draft_id=$2&title=Draft2&tags=[1,7]&category_id=37&content=Draft2_content" localhost:5555/drafts/edit
echo ""
