#!/bin/bash
curl -G -d "token=$1&comment_id=$2" localhost:5555/comments/delete
echo ""
