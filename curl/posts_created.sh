#!/bin/bash
curl -G -d "created_at=$1" localhost:5555/posts
echo ""

