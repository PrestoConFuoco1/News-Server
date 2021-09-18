#!/bin/bash
curl -G -d "token=$1&tag_id=$2&name=$3" localhost:5555/tags/edit
echo ""
