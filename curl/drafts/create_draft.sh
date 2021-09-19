#!/bin/bash
curl -G -d "token=$1&title=Draft1&tags=[4,5]&category_id=37&content=Draft1_content" localhost:5555/drafts/create
echo ""
