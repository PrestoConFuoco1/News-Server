#!/bin/bash
curl -G -d "token=$1&title=Draft1&tags=[4,5]&category_id=1&content=Draft1_13_content&1=1&2=2&3=3&4=4&5=5&6=6" localhost:5555/drafts/create
echo ""
