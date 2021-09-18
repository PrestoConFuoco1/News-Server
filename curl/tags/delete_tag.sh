#!/bin/bash
curl -G -d "token=$1&tag_id=$2" localhost:5555/tags/delete
echo ""
