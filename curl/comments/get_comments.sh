#!/bin/bash
curl -G -d "post_id=$1" localhost:5555/comments/get
echo ""
